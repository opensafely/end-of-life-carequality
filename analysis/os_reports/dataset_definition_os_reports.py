# Creates the population needed for the analysis to feed the OS report using ehrQL

# Functions from ehrQL

from ehrql import (Dataset, days)

from ehrql.tables.beta.tpp import (
    appointments, 
    emergency_care_attendances, 
    ons_deaths,
    opa_diag,
    patients,
    practice_registrations,
    medications,
)

## CODELISTS ##

# Import codelists from the codelist folder

import codelists

## KEY VARIABLES ##

earliest_date = "2019-06-01"
latest_date = "2023-06-30"
date_range = (earliest_date, latest_date)

## STUDY DEFINITION ##

dataset = Dataset()

last_ons_death = ons_deaths.sort_by(ons_deaths.date).last_for_patient()

dod_ons = last_ons_death.date

has_died = dod_ons.is_on_or_between(*date_range)

was_registered_at_death = (
    practice_registrations.where(practice_registrations.start_date <= dod_ons)
    .except_where(practice_registrations.end_date <= dod_ons)
    .exists_for_patient()
)

dataset.define_population(
    has_died
    & was_registered_at_death
    & patients.sex.is_in(["female", "male"])
)

## CREATE VARIABLES ##

## Key cohort variables ##

## ONS date of death
dataset.dod_ons = last_ons_death.date

## ONS place of death
dataset.pod_ons = last_ons_death.place

## ONS cause of death
dataset.cod_ons = last_ons_death.underlying_cause_of_death

## Demographics ##

## Sex
dataset.sex = patients.sex

## Services ##

## GP consultations
dataset.gp_1m = appointments.where(
    appointments.status.is_in([
        "Arrived",
        "In Progress",
        "Finished",
        "Visit",
        "Waiting",
        "Patient Walked Out",
    ])).where(
        appointments.start_date.is_on_or_between(dod_ons - days(30), dod_ons)
    ).count_for_patient()

## Hospital activity

## A&E visits
dataset.aevis_1m = emergency_care_attendances.where(
    emergency_care_attendances.arrival_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Outpatient appointments
# Need to check this is correct/how to do attended appointments?
dataset.opapp_1m = opa_diag.where(
    opa_diag.appointment_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Medications for symptom management at end of life
dataset.eol_med_1m = medications.where(
    medications.dmd_code.is_in(codelists.eol_med_codes)
).where(
    medications.date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()
