# Creates the population needed for the analysis using ehrQL
# Note:
# Ethnicity recording in primary care records is supplemented with hospital records in the case of missing data
# Primary care ethnicity codes are based on a 6 category SNOMED UK ethnicity category codelist, standardised to the 2001 census categories, used elsewhere in the NHS.
# sus ethnicity codes are as defined by "Ethnic Category Code 2001" — the 16+1 ethnic data categories used in the 2001 census.


# Functions from ehrQL

from ehrql import (Dataset, days, case, when)

from ehrql.tables.beta.tpp import (
    addresses,
    appointments, 
    clinical_events,
    emergency_care_attendances, 
    hospital_admissions,
    ons_deaths,
    opa,
    opa_diag,
    patients,
    practice_registrations,
    medications,
    ethnicity_from_sus,
)

## CODELISTS ##

# Import codelists from the codelist folder

import codelists
from ehrql import codelist_from_csv

## KEY VARIABLES ##

earliest_date = "2019-03-01"
latest_date = "2023-12-31"
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
        when(age < 25).then("0-24"),
        when(age < 70).then("25-69"),
        when(age < 80).then("70-79"),
        when(age < 90).then("80-89"),
        when(age >= 90).then("90+"),
        otherwise="missing",
)

## Ethnicity
ethnicity_codelist_with_categories = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column = "snomedcode",
    category_column = "Grouping_6"
)

dataset.latest_ethnicity_code = (
    clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_codelist_with_categories))
    .where(clinical_events.date.is_on_or_before(dod_ons))
    .sort_by(clinical_events.date)
    .last_for_patient().snomedct_code
)

latest_ethnicity_group = dataset.latest_ethnicity_code.to_category(
    ethnicity_codelist_with_categories
)

# Add in code to extract ethnicity from SUS if it isn't present in primary care data. 

ethnicity_sus = ethnicity_from_sus.code

dataset.ethnicity_Combined = case(
  when((latest_ethnicity_group == "1") | ((latest_ethnicity_group.is_null()) & (ethnicity_sus.is_in(["A", "B", "C"])))).then("White"),
  when((latest_ethnicity_group == "2") | ((latest_ethnicity_group.is_null()) & (ethnicity_sus.is_in(["D", "E", "F", "G"])))).then("Mixed"),
  when((latest_ethnicity_group == "3") | ((latest_ethnicity_group.is_null()) & (ethnicity_sus.is_in(["H", "J", "K", "L"])))).then("Asian or Asian British"),
  when((latest_ethnicity_group == "4") | ((latest_ethnicity_group.is_null()) & (ethnicity_sus.is_in(["M", "N", "P"])))).then("Black or Black British"),
  when((latest_ethnicity_group == "5") | ((latest_ethnicity_group.is_null()) & (ethnicity_sus.is_in(["R", "S"])))).then("Chinese or Other Ethnic Groups"),
  otherwise="Not stated", 
) 

## Geography ##

## Index of multiple deprivation based on patient address. 1-most deprived, 5-least deprived
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

## Outpatient appointments (Attended only)
# Excludes most mental health care and community services

dataset.opapp_1m = opa.where(
    opa.attendance_status.is_in(["5", "6"])
).where(    
    opa.appointment_date.is_on_or_between(dod_ons - days(30), dod_ons)
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

## Advance care plan measures

## Presence of an advance care plan code in patients' GP record
dataset.has_careplan = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).exists_for_patient()

## Number of advance care plan codes in patients' GP record
dataset.careplan_3m = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(90), dod_ons)
).count_for_patient()

## Length of time for which advance care plan code has existed in patients' GP record
first_careplan = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.care_plan_palcare)
).sort_by(
    clinical_events.date).first_for_patient().date

dataset.length_careplan = (dod_ons - first_careplan).days
