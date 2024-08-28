from scipy.stats import poisson, uniform
import numpy as np
import argparse
from dataclasses import dataclass

TARGET_CLOSING_TIME = (16 - 9) * 60


class PatientLabeler:
    def __init__(self):
        self.label = 0

    def get_label(self):
        label = self.label
        self.label += 1
        return label


labeler = PatientLabeler()


class Patient:
    def __init__(self):
        self.label = labeler.get_label()
        self.waiting_time = 0

    def __repr__(self):
        return f"Patient({self.label}, {self.waiting_time})"

    def step(self):
        self.waiting_time += 1


class Doctor:
    def __init__(self, label: str, appt_min: int, appt_max: int):
        self.label = label
        self.dist = uniform(appt_min, appt_max)
        self.time_remaining_with_current_patient = None

    def is_with_patient(self):
        return (self.time_remaining_with_current_patient is not None and
                self.time_remaining_with_current_patient > 0)

    def is_available(self):
        return not self.is_with_patient()

    def step(self):
        if self.time_remaining_with_current_patient is not None:
            self.time_remaining_with_current_patient -= 1

    def assign_patient(self):
        self.time_remaining_with_current_patient = int(np.ceil(self.dist.rvs(1)[0]))

    def __repr__(self):
        return f"Doctor({self.label}, {self.time_remaining_with_current_patient})"


def is_empty(lst):
    return len(lst) == 0


class Clinic:
    def __init__(self, args, closing_time=TARGET_CLOSING_TIME):
        self.doctors = [Doctor(chr(65 + i),
                               args.appt_min, args.appt_max)
                        for i in range(args.ndoctors)]
        self.closing_time = closing_time
        self.is_closed = False
        self.patient_queue = []

        self.patient_history = []

    def add_patient(self):
        self.patient_queue.append(Patient())

    def get_available_doctor(self):
        for doctor in self.doctors:
            if doctor.is_available():
                return doctor
        return None

    def has_no_patients(self):
        any_doctors_still_with_patients = any([
            d.time_remaining_with_current_patient > 0
            if d.time_remaining_with_current_patient is not None
            else False
            for d in self.doctors])
        return is_empty(self.patient_queue) and not any_doctors_still_with_patients

    def still_accepting_patients(self, time):
        return time < self.closing_time

    @staticmethod
    def print_status_headers():
        print('t,A,B,C,waiting')

    def print_status(self, time):
        doctor_statuses = ','.join([f"{d.time_remaining_with_current_patient}" for d in self.doctors])
        s = {'t': time,
             'doctor status': doctor_statuses,
             'patients waiting': len(self.patient_queue)}
        print(','.join([str(v) for v in s.values()]))

    def step(self, time):
        self.close_if_appropriate(time)

        if not self.is_closed and self.patient_queue:
            available_doctor = self.get_available_doctor()
            if available_doctor:
                patient_to_assign: Patient = self.patient_queue.pop(0)
                self.patient_history.append(patient_to_assign)
                available_doctor.assign_patient()
        # self.print_status(time)

        for doctor in self.doctors:
            doctor.step()
        for patient in self.patient_queue:
            patient.step()

    def close_if_appropriate(self, time):
        self.is_closed = self.has_no_patients() and time >= self.closing_time


class Simulation:
    def __init__(self, args):
        self.clinic = Clinic(args)
        self.time = 0
        self.arrivals_distribution = poisson(args.arrival_rate)
        self.minutes_until_next_patient = self.arrivals_distribution.rvs(1)[0]

    def step(self):
        if self.minutes_until_next_patient == 0:
            if self.clinic.still_accepting_patients(self.time):
                self.clinic.add_patient()
            self.minutes_until_next_patient = self.arrivals_distribution.rvs(1)[0]

        self.clinic.step(self.time)
        self.minutes_until_next_patient -= 1
        self.time += 1

    def run(self):
        while not self.clinic.is_closed:
            self.step()


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--ndoctors', type=int, default=3)
    parser.add_argument('--appt_min', type=int, default=5)
    parser.add_argument('--appt_max', type=int, default=20)
    parser.add_argument('--arrival_rate', type=int, default=10)

    return parser.parse_args()


@dataclass
class SimulationSummary:
    n_patients: int
    avg_waiting_time: float
    n_waited: int
    closing_time_diff: int

    def print_report(self):
        print(f"Saw {self.n_patients} patients, with an average waiting time of {self.avg_waiting_time:.2f} minutes. "
              f"{self.n_waited} had to wait.")
        print(f"The clinic closed {self.closing_time_diff // 60}h, {self.closing_time_diff % 60}m after 4pm.")


class Interval:
    def __init__(self, lb, ub):
        self.lb = lb
        self.ub = ub

    def __repr__(self):
        return f"Interval({self.lb}, {self.ub})"


def run_single_simulation():
    args = get_args()
    simulation = Simulation(args)
    # Clinic.print_status_headers()
    simulation.run()

    n_patients = len(simulation.clinic.patient_history)
    avg_waiting_time = float(np.mean([p.waiting_time for p in simulation.clinic.patient_history]))
    n_waited = len([p for p in simulation.clinic.patient_history if p.waiting_time > 0])
    actual_closing_time = simulation.time
    closing_time_diff = actual_closing_time - TARGET_CLOSING_TIME - 1
    return SimulationSummary(n_patients, avg_waiting_time, n_waited, closing_time_diff)


def get_interval(vec: np.array, lb: float, ub: float):
    vec = np.sort(vec)
    lower_idx = len(vec) * lb
    upper_idx = len(vec) * ub
    return Interval(vec[int(lower_idx)], vec[int(upper_idx)])


def run_multiple_simulations(n):
    summaries = []
    for _ in range(n):
        summaries.append(run_single_simulation())

    def get_stats(fn):
        n_patients = fn([s.n_patients for s in summaries])
        n_waited = fn([s.n_waited for s in summaries])
        avg_waiting_time = fn([s.avg_waiting_time for s in summaries])
        closing_time_diff = fn([s.closing_time_diff for s in summaries])
        return dict(n_patients=n_patients, n_waited=n_waited, avg_waiting_time=avg_waiting_time,
                    closing_time_diff=closing_time_diff)

    medians = SimulationSummary(**get_stats(np.median))
    means = SimulationSummary(**get_stats(np.mean))
    print()
    print("Medians:")
    medians.print_report()
    print()
    print("Means:")
    means.print_report()

    intervals = get_stats(lambda x: get_interval(x, 0.25, 0.75))
    print(intervals)


if __name__ == '__main__':
    # run_single_simulation().print_report()
    run_multiple_simulations(100)
