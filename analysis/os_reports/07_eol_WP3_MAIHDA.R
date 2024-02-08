##############################################################
# MAIHDA analysis
# Author: Miranda & Sophie 
# Date: 07/12/23 # nolint # nolint: commented_code_linter.
# Initial aim: Running descriptive statistics
# for potential stratum variables to determine whether group sizes are sufficient
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. Analysis focuses on patients who die at home, with a cancer diagnosis. 

# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-09-2022")
enddate <- dmy("31-08-2023")

# Prepare data

df <- read_csv(file = here::here("output", "os_reports", "input_os_reports.csv.gz")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , Ethnicity_3 = case_when(ethnicity_Combined == "White" ~ "White"
                             , ethnicity_Combined == "Asian or Asian British" ~ "Asian or Asian British"
                             , ethnicity_Combined == "Black or Black British" | ethnicity_Combined == "Mixed" | ethnicity_Combined == "Chinese or Other Ethnic Groups" | ethnicity_Combined == "Not stated" ~ "All other ethnic groups")
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

# Counts of places of death by ethnicity for all deaths + cancer only - not for release

deaths_ethnicity_place_all_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new) %>%
  summarise(count = n())

fwrite(deaths_ethnicity_place_all_raw, here::here("output", "os_reports", "WP3", "deaths_ethnicity_place_all_raw.csv"))

deaths_ethnicity_place_cancer_raw <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(ethnicity_Combined, pod_ons_new) %>%
  summarise(count = n())

fwrite(deaths_ethnicity_place_cancer_raw, here::here("output", "os_reports", "WP3", "deaths_ethnicity_place_cancer_raw.csv"))

# Checking group sizes for three broad ethnicity groupings

deaths_ethnicity3_place_all_raw <- df %>%
  group_by(Ethnicity_3, pod_ons_new) %>%
  summarise(count = n())

fwrite(deaths_ethnicity_place_all_raw, here::here("output", "os_reports", "WP3", "deaths_ethnicity_place_all_raw.csv"))

deaths_ethnicity3_place_cancer_raw <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(Ethnicity_3, pod_ons_new) %>%
  summarise(count = n())

fwrite(deaths_ethnicity3_place_cancer_raw, here::here("output", "os_reports", "WP3", "deaths_ethnicity3_place_cancer_raw.csv"))

# Descriptive analysis to inform modelling - counts of age_band / sex / ethnicity and imd_rounded 

cols_of_interest <- c("n");

count_by_sex <- df %>%
  count(sex) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(count_by_sex, here::here("output", "os_reports", "WP3", "count_by_sex.csv"))

count_by_age_band <- df %>%
  count(age_band) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(count_by_age_band, here::here("output", "os_reports", "WP3", "count_by_age_band.csv"))

count_by_sex_age_band <- df %>%
  count(sex, age_band) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(count_by_sex_age_band, here::here("output", "os_reports", "WP3", "count_by_sex_age_band.csv"))

count_by_ethnicity <- df %>%
  count(ethnicity_Combined) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(count_by_ethnicity, here::here("output", "os_reports", "WP3", "count_by_ethnicity.csv"))

count_by_imd_quintile <- df %>%
  count(imd_quintile) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(count_by_imd_quintile, here::here("output", "os_reports", "WP3", "count_by_imd_quintile.csv"))

count_by_group <- df %>%
  count(sex, age_band, ethnicity_Combined, imd_quintile) %>%
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));
  
fwrite(count_by_group, here::here("output", "os_reports", "WP3", "count_by_group.csv"))

# Descriptive analysis to inform modelling for cancer deaths at home - counts of age_band / sex / ethnicity and imd_rounded ------

cancer_count_by_sex <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(sex) %>%
  count(sex) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_sex, here::here("output", "os_reports", "WP3", "cancer_count_by_sex.csv"))

cancer_count_by_age_band <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(age_band) %>%
  count(age_band) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_age_band, here::here("output", "os_reports", "WP3", "cancer_count_by_age_band.csv"))

cancer_count_by_sex_age_band <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(sex, age_band) %>%
  count(sex, age_band) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_sex_age_band, here::here("output", "os_reports", "WP3", "cancer_count_by_sex_age_band.csv"))

cancer_count_by_ethnicity <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(ethnicity_Combined) %>%
  count(ethnicity_Combined) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_ethnicity, here::here("output", "os_reports", "WP3", "cancer_count_by_ethnicity.csv"))

# Checking group sizes where ethnicity is split into 3 groups

cols_of_interest <- c("n");

cancer_count_by_Ethnicity_3 <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(Ethnicity_3) %>%
  count(Ethnicity_3) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_Ethnicity_3, here::here("output", "os_reports", "WP3", "cancer_count_by_Ethnicity_3.csv"))

cancer_count_by_imd_quintile <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(imd_quintile) %>%
  count(imd_quintile) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_imd_quintile, here::here("output", "os_reports", "WP3", "cancer_count_by_imd_quintile.csv"))

cancer_count_by_group <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(sex, age_band, imd_quintile, ethnicity_Combined) %>%
  count(sex, age_band, imd_quintile, ethnicity_Combined) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_group, here::here("output", "os_reports", "WP3", "cancer_count_by_group.csv"))

# Group counts where ethnicity is split into three broad groups - cancer only + deaths at home

cancer_count_by_group_eth3 <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  group_by(sex, age_band, imd_quintile, Ethnicity_3) %>%
  count(sex, age_band, imd_quintile, Ethnicity_3) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(cancer_count_by_group_eth3, here::here("output", "os_reports", "WP3", "cancer_count_by_group_3.csv"))






