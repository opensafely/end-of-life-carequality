# Creates the population needed for the analysis using ehrQL

# Functions from ehrQL

from ehrql import (Dataset, days, case, when)

from ehrql.tables.beta.tpp import (
    addresses,
    appointments, 
    clinical_events,
    emergency_care_attendances, 
    hospital_admissions,
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

earliest_date = "2018-12-01"
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
    & (patients.exists_for_patient())
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

## Age band 
age = patients.age_on(dod_ons)

dataset.age_band = case(
        when(age < 20).then("0-19"),
        when(age < 30).then("20-29"),
        when(age < 40).then("30-39"),
        when(age < 50).then("40-49"),
        when(age < 60).then("50-59"),
        when(age < 70).then("60-69"),
        when(age < 80).then("70-79"),
        when(age < 90).then("80-89"),
        when(age >= 90).then("90+"),
        otherwise="missing",
)

## Ethnicity
ethnicity_codelist = codelist_from_csv(
    "ethnicity_codelist_with_categories",
    column="snomedcode",
    category_column="Grouping_6",
)

dataset.latest_ethnicity_code = (
    clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_codelist))
    .where(clinical_events.date.is_on_or_before(dod_ons))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .snomedct_code
)
dataset.latest_ethnicity_group = dataset.latest_ethnicity_code.to_category(
    ethnicity_codelist
)

dataset.ethnity_new = case(
  when(latest_ethnicity_group = 1). then("White"),
  when(latest_ethnicity_group = 2). then("Mixed"),
  when(latest_ethnicity_group = 3). then("Asian or Asian British"),
  when(latest_ethnicity_group = 4). then("Black or Black British"),
  when(latest_ethnicity_group = 5). then("Chinese or Other Ethnic Groups"),
  otherwise="Not stated",
)

# No ethnicity from SUS in ehrQL

## Geography ##

## Index of multiple deprivation based on patient address
imd = addresses.for_patient_on(dod_ons).imd_rounded

dataset.imd_quintile = case(
    when((imd >= 0) & (imd < int(32844 * 1 / 5))).then("1"),
    when(imd < int(32844 * 2 / 5)).then("2"),
    when(imd < int(32844 * 3 / 5)).then("3"),
    when(imd < int(32844 * 4 / 5)).then("4"),
    when(imd < int(32844 * 5 / 5)).then("5"),
    default="0"
)

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
