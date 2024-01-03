# Creates the population needed for the analysis to feed the OS report using ehrQL

# Functions from ehrQL

from ehrql import (Dataset, days)

from ehrql.tables.beta.tpp import (
    appointments, 
    emergency_care_attendances, 
    hospital_admissions,
    ons_deaths,
    opa_diag,
    patients,
    practice_registrations,
    medications,
    clinical_events,
)

## CODELISTS ##

# Import codelists from the codelist folder

import codelists

## KEY VARIABLES ##

earliest_date = "2019-03-01"
latest_date = "2023-08-31"
date_range = (earliest_date, latest_date)

## STUDY DEFINITION ##

dataset = Dataset()

dod_ons = ons_deaths.date

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
dataset.dod_ons = ons_deaths.date

## ONS place of death
dataset.pod_ons = ons_deaths.place

## ONS cause of death
dataset.cod_ons = ons_deaths.underlying_cause_of_death

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

## Medications for symptom management at end of life
dataset.eol_med_1m = medications.where(
    medications.dmd_code.is_in(codelists.eol_med_codes)
).where(
    medications.date.is_on_or_between(dod_ons - days(30), dod_ons)
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

## Elective admissions
dataset.eladm_1m = hospital_admissions.where(
    hospital_admissions.admission_method.is_in(["11", "12", "13"])
).where(
    hospital_admissions.admission_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Emergency admissions
dataset.emadm_1m = hospital_admissions.where(
    hospital_admissions.admission_method.is_in(['21', '2A', '22', '23', '24', '25', '2D', '28', '2B'])
).where(
    hospital_admissions.admission_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Community nursing contacts
dataset.nursing_1m = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.community_nursing_codes)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Quality Indicators ## 

## Palliative care
dataset.palliative_3m = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.palcare_codes1)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient()

## A&E visits last 3 months of life
dataset.aevis_3m = emergency_care_attendances.where(
    emergency_care_attendances.arrival_date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient()

## Medications for symptom management last 3 months of life
dataset.eol_med_3m = medications.where(
    medications.dmd_code.is_in(codelists.eol_med_codes)
).where(
    medications.date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient() 

## Specialist palliative care last 3 months of life
dataset.specialist_3m = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.specialist_codes)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient()

## Advance care plan
dataset.has_careplan = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).exists_for_patient()

dataset.careplan_3m = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient()

first_careplan = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).sort_by(
    clinical_events.date).first_for_patient().date

dataset.length_careplan = (dod_ons - first_careplan).days
