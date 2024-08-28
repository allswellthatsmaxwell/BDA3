from scipy.stats import poisson, uniform
import numpy as np


class Doctor:
    def __init__(self):
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
        self.time_remaining_with_current_patient = np.random.uniform(5, 20)


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

    def __repr__(self):
        return f"Patient({self.label})"


def is_empty(lst):
    return len(lst) == 0


class Clinic:
    def __init__(self, ndoctors=3, closing_time=(16 - 9) * 60):
        self.doctors = [Doctor() for _ in range(ndoctors)]
        self.closing_time = closing_time
        self.is_closed = False
        self.patient_queue = []

    def add_patient(self):
        self.patient_queue.append(Patient())

    def get_available_doctor(self):
        for doctor in self.doctors:
            if doctor.is_available():
                return doctor
        return None

    def has_no_patients(self):
        return is_empty(self.patient_queue)

    def still_accepting_patients(self, time):
        return time < self.closing_time

    def step(self, time):
        for doctor in self.doctors:
            doctor.step()

        self.close_if_appropriate(time)

        if not self.is_closed and self.patient_queue:
            available_doctor = self.get_available_doctor()
            if available_doctor:
                available_doctor.assign_patient()
                self.patient_queue.pop(0)

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
                print("adding patient")
                self.clinic.add_patient()
            self.minutes_until_next_patient = self.arrivals_distribution.rvs(1)[0]

        self.minutes_until_next_patient -= 1
        self.clinic.step(self.time)
        self.time += 1

    def run(self):
        while not self.clinic.is_closed:
            print(self.time)
            self.step()


if __name__ == '__main__':
    simulation = Simulation()
    simulation.run()
