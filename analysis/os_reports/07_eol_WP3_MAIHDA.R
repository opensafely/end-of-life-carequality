##############################################################
# MAIHDA analysis
# Author: Miranda & Sophie 
# Date: 07/12/23 # nolint # nolint: commented_code_linter.
# Initial aim: Running descriptive statistics
# for potential stratum variables to determine whether group sizes are sufficient
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. Analysis focuses on patients who die at home, with a cancer diagnosis. 
# Ethnicity is considered as two groups for the purpose of MAIHDA, but more detailed analysis of ethnicity will be conducted separately.
# Analysis over a two calendar year period

# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-01-2022")
enddate <- dmy("31-12-2023")

# Prepare data

df <- read_csv(file = here::here("output", "os_reports", "input_os_reports.csv.gz")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , Ethnicity_3 = case_when(ethnicity_Combined == "White" ~ "White"
                             , ethnicity_Combined == "Asian or Asian British" | ethnicity_Combined == "Black or Black British" | ethnicity_Combined == "Mixed" | ethnicity_Combined == "Chinese or Other Ethnic Groups" | ethnicity_Combined == "Not stated" ~ "All other ethnic groups")
         , study_month = floor_date(dod_ons, unit = "month")
         , pod_ons_new = case_when(pod_ons == "Elsewhere" 
                                   | pod_ons == "Other communal establishment" ~ "Elsewhere/other"
                                   , TRUE ~ as.character(pod_ons))
         , cod_ons_3 = str_sub(cod_ons, 1, 3)
         , cod_ons_4 = str_sub(cod_ons, 1, 5)
         , codgrp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                             , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                             , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                             , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                             , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                             , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                             , TRUE ~ "All other causes")) %>%
  filter(study_month >= startdate & study_month <= enddate & imd_quintile >=1 & age_band != "0-24")

# Counts by grouping variables_RAW not for release

count_by_group_RAW <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  count(sex, age_band, ethnicity_Combined, imd_quintile) 

fwrite(count_by_group_RAW, here::here("output", "os_reports", "WP3", "count_by_group_RAW.csv"))

cols_of_interest <- c("n");

count_by_group <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  count(sex, age_band, ethnicity_Combined, imd_quintile) %>%
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));
  
fwrite(count_by_group, here::here("output", "os_reports", "WP3", "count_by_group.csv"))

# Form intersectional strata

#dplyr::group_by(sex, age_band, ethnicity_Combined, imd_quintile) %>% dplyr::mutate(strata = cur_group_id())





