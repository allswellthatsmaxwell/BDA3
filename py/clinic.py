from scipy.stats import poisson, uniform
import numpy as np


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
        # print(f"Stepping patient {self.label}.")
        self.waiting_time += 1


class Doctor:
    def __init__(self, label: str):
        self.label = label
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
        # decide how long he'll be with this patient
        self.time_remaining_with_current_patient = int(np.ceil(np.random.uniform(5, 20)))
        # print(
        #     f"This patient will spend {int(np.ceil(self.time_remaining_with_current_patient))} minutes with the doctor.")

    def __repr__(self):
        return f"Doctor({self.label}, {self.time_remaining_with_current_patient})"


def is_empty(lst):
    return len(lst) == 0


class Clinic:
    def __init__(self, ndoctors=3, closing_time=(16 - 9) * 60):
        self.doctors = [Doctor(chr(65 + i)) for i in range(ndoctors)]
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
        for doctor in self.doctors:
            doctor.step()
        for patient in self.patient_queue:
            patient.step()

        self.close_if_appropriate(time)

        if not self.is_closed and self.patient_queue:
            available_doctor = self.get_available_doctor()
            if available_doctor:
                patient_to_assign: Patient = self.patient_queue.pop(0)
                self.patient_history.append(patient_to_assign)
                available_doctor.assign_patient()
                # print(f"Assigned {patient_to_assign} to {available_doctor}.")
        self.print_status(time)

    def close_if_appropriate(self, time):
        self.is_closed = self.has_no_patients() and time >= self.closing_time


class Simulation:
    def __init__(self, ndoctors=3):
        self.clinic = Clinic(ndoctors)
        self.time = 0
        self.arrivals_distribution = poisson(10)
        self.minutes_until_next_patient = self.arrivals_distribution.rvs(1)[0]

    def step(self):
        if self.minutes_until_next_patient == 0:
            if self.clinic.still_accepting_patients(self.time):
                # print("adding patient")
                self.clinic.add_patient()
            self.minutes_until_next_patient = self.arrivals_distribution.rvs(1)[0]

        self.minutes_until_next_patient -= 1
        self.clinic.step(self.time)
        self.time += 1

    def run(self):
        while not self.clinic.is_closed:
            # print(self.time)
            self.step()


if __name__ == '__main__':
    simulation = Simulation()
    Clinic.print_status_headers()
    simulation.run()
    # print(simulation.clinic.patient_history)
