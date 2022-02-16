# Functions from cohort extractor

from cohortextractor import (
    StudyDefinition,
    patients)

## CODELISTS ##

# Import codelists from the codelist folder

from codelists import *

## KEY VARIABLES ##

EARLIEST = "2019-03-01"
LATEST = "2021-02-28"

## STUDY DEFINITION ##

study = StudyDefinition(

    ## Default variable expectations 
    # e.g. dates between 1/1/1970 to 28/2/2021 with uniform frequency and 50% present
    # e.g. categorical and numeric present 50% 
    # e.g. binary 50% positive
    default_expectations = {
        "date": {"earliest": "1970-01-01", "latest": LATEST},
        "rate": "uniform",
        "incidence": 0.5
    },

    ## Study population
    # Everyone who died between March 2019 and February 2021
    # Registered with TPP on date of death - could make this more/less flexible
    # Not sure if this is doing what I want around registered
    population = patients.satisfying(
        "has_died AND has_registered",
        has_died = patients.died_from_any_cause(
        between = [EARLIEST, LATEST],
        return_expectations = {"incidence": 1.0}
        ),
        has_registered = patients.registered_as_of(
            "dod_ons",
            return_expectations = {"incidence": 0.98}
        )
    ),  
    #population = patients.died_from_any_cause(
    #    between = [EARLIEST, LATEST],
    #    return_expectations = {"incidence": 1.0}
    #),

    ## CREATE VARIABLES ##

    ## Key cohort variables ##

    ## ONS date of death
    # Want it present for 100% of individuals
    dod_ons = patients.died_from_any_cause(
        between = [EARLIEST, LATEST],
        returning = "date_of_death",
        date_format = "YYYY-MM-DD",
        return_expectations = {
            "date": {"earliest": EARLIEST},
            "rate": "uniform",
            "incidence": 1.0
        }
    ),

    ## ONS place of death 
    # Distribution from English death locations
    pod_ons=patients.died_from_any_cause(
       returning="place_of_death",
       return_expectations={
           "rate": "universal",
           "category": {"ratios": {"Care home": 0.236, "Elsewhere": 0.022, "Home": 0.274, "Hospice": 0.044, "Hospital": 0.42, "Other communal establishment": 0.004}}
       }
    ),

    ## ONS underlying cause of death 
    # Will want to create a categorised version of this later e.g. sudden deaths
    cod_ons = patients.died_from_any_cause(
        returning = "underlying_cause_of_death",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"I6":0.17, "J4":0.1, "F01":0.06, "J1":0.11 , "I2":0.43 , "C3":0.13}}
        }
    ),

    ## Demographics ##

    ## Sex - 49% male
    sex = patients.sex(
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}}
        }
    ),

    ## Age based on population distribution
    age = patients.age_as_of(
        "dod_ons",
        return_expectations = {
            "rate": "universal",
            "int": {"distribution": "population_ages"}
        }
    ),

    ## Ethnicity -  6 categories coded in primary care
    ethnicity6 = patients.with_these_clinical_events(
        ethnicity_codes_6,
        returning = "category",
        find_last_match_in_period = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
            "incidence": 0.75
        }
    ),

    ## Ethnicity -  6 categories coded in SUS
    ethnicity6_sus = patients.with_ethnicity_from_sus(
        returning = "group_6",
        use_most_frequent_code = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
            "incidence": 0.75
        }
    ),

    ## Index of multiple deprivation based on patient address
    imd = patients.address_as_of(
        "dod_ons",
        returning = "index_of_multiple_deprivation",
        round_to_nearest = 100,
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}}
        }
    ),

    ## Care home - TPP assigned if address is care home
    carehome = patients.care_home_status_as_of(
        "dod_ons"
    ),

    ## Care home type - TPP assigned 
    carehome_type = patients.care_home_status_as_of(
        "dod_ons",
        categorised_as = {
            "PC":
            """
            IsPotentialCareHome
            AND LocationDoesNotRequireNursing = 'Y'
            AND LocationRequiresNursing = 'N'
            """,
            "PN":
            """
            IsPotentialCareHome
            AND LocationDoesNotRequireNursing = 'N'
            AND LocationRequiresNursing = 'Y'
            """,
            "PS": "IsPotentialCareHome",
            "PR": "NOT IsPotentialCareHome",
            "": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"PC": 0.05, "PN": 0.05, "PS": 0.05, "PR": 0.84, "": 0.01}}
            }
    ),

    ## Household size - TPP algorithm - as of Feb 2020
    hhold_size = patients.household_as_of(
        "2020-02-01",
        returning = "household_size",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Geography ##

    ## MSOA of address 
    # As of Feb 2020 
    # Need to think about how we would link info to this
    msoa = patients.household_as_of(
        "2020-02-01",
        returning = "msoa",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}}
        }
    ),

    ## Health ##

    ## Long term conditions
    ltc = patients.with_these_clinical_events(
        ltc_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    physical_ltc = patients.with_these_clinical_events(
        physical_ltc_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    mental_ltc = patients.with_these_clinical_events(
        mental_ltc_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    frailty = patients.with_these_decision_support_values(
        algorithm = "electronic_frailty_index",
        returning = "binary_flag"
    ),

    # Atrial fibrillation
    ltc_afib = patients.with_these_clinical_events(
        afib_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),
    # Coronary heart disease
    ltc_chd = patients.with_these_clinical_events(
        chd_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Heart failure 
    ltc_hf = patients.with_these_clinical_events(
        hf_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Hypertension
    ltc_hyp = patients.with_these_clinical_events(
        hyp_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Peripheral arterial disease
    ltc_pad = patients.with_these_clinical_events(
        pad_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Stroke and transient ischaemic attack
    ltc_strk = patients.with_these_clinical_events(
        strk_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Asthma
    ltc_ast = patients.with_these_clinical_events(
        ast_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # COPD
    ltc_copd = patients.with_these_clinical_events(
        copd_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Cancer
    ltc_haemcan = patients.with_these_clinical_events(
        haemcan_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    ltc_can = patients.with_these_clinical_events(
        can_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Chronic kidney disease
    ltc_ckd1 = patients.with_these_clinical_events(
        ckd_codes1,
            returning = "binary_flag",
        find_first_match_in_period = True
    ),

    ltc_ckd2 = patients.with_these_clinical_events(
        ckd_codes2,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Diabetes mellitus
    ltc_dm = patients.with_these_clinical_events(
        dm_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Palliative care
    ltc_palcare1 = patients.with_these_clinical_events(
        palcare_codes1,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    #ltc_palcare2 = patients.with_these_clinical_events(
    #    palcare_codes2,
    #    returning = "binary_flag",
    #    find_first_match_in_period = True
    #),

    # Epilepsy
    ltc_epil = patients.with_these_clinical_events(
        epil_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Osteoporosis
    ltc_osteo = patients.with_these_clinical_events(
        osteo_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Rheumatoid arthiritis
    ltc_rarth = patients.with_these_clinical_events(
        rarth_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Non-diabetic hyperglycaemia? 
    ltc_ndh = patients.with_these_clinical_events(
        ndh_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Dementia
    ltc_dem = patients.with_these_clinical_events(
        dem_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Depression
    ltc_depr = patients.with_these_clinical_events(
        depr_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Schizophrenia, Bipolar effective disorder, Psychoses
    ltc_mh = patients.with_these_clinical_events(
        mh_codes,
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # Learning disability?
    ltc_ld = patients.with_these_clinical_events(
        ld_codes, 
        returning = "binary_flag",
        find_first_match_in_period = True
    ),

    # EOL register

    ## SERVICE USE ##

    ## Hospital activity in year prior to death

    aevis_1yr = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm_1yr = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    emadm_1yr = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eladm_1yr = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    # Bed days??

    opapp_1yr = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt_1yr = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        attended = True,
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## GP clinical coded activity in year prior to death
    # Use as a proxy for contact with GP
    gp_1yr = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    # EOL medication - code list

)

# opensafely run run_all --force-run-dependencies
# opensafely upgrade
# opensafely pull
