###############################################################################
# CSV files for ethnicity analysis
# Date: 25.03.24
# Author: Miranda & Sophie 
# Aim: Descriptive analysis to explore breakdown of patients at end of life by ethnicity in combination with additional characteristics.
# Note: Files are created with and without rounding / redaction. Non-rounded/redacted files are not for release.
# Note: Descriptives cover two-years of data to mirror MAIHDA analysis period. 
###############################################################################

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
  filter(study_month >= startdate & study_month <= enddate) 

# Initial counts of ethnicity splits 

cols_of_interest <- c("count");

ethnicity_raw <- df %>%
  group_by(ethnicity_Combined) %>%
  summarise(count = n());

fwrite(ethnicity_raw, here::here("output", "os_reports", "WP3", "ethnicity_raw.csv"))

ethnicity <- df %>%
  group_by(ethnicity_Combined) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity, here::here("output", "os_reports", "WP3", "ethnicity.csv"))

# Ethnicity breakdown by age

cols_of_interest <- c("count");

ethnicity_age_raw <- df %>%
  group_by(ethnicity_Combined, age_band) %>%
  summarise(count = n());

fwrite(ethnicity_age_raw, here::here("output", "os_reports", "WP3", "ethnicity_age_raw.csv"))

ethnicity_age <- df %>%
  group_by(ethnicity_Combined, age_band) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_age, here::here("output", "os_reports", "WP3", "ethnicity_age.csv"))

# Ethnicity breakdown by IMD

cols_of_interest <- c("count");

ethnicity_IMD_raw <- df %>%
  group_by(ethnicity_Combined, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_IMD_raw, here::here("output", "os_reports", "WP3", "ethnicity_IMD_raw.csv"))

ethnicity_IMD <- df %>%
  group_by(ethnicity_Combined, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_IMD, here::here("output", "os_reports", "WP3", "ethnicity_IMD.csv"))

# Ethnicity breakdown by Sex

cols_of_interest <- c("count");

ethnicity_sex_raw <- df %>%
  group_by(ethnicity_Combined, sex) %>%
  summarise(count = n());

fwrite(ethnicity_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_sex_raw.csv"))

ethnicity_sex <- df %>%
  group_by(ethnicity_Combined, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_sex, here::here("output", "os_reports", "WP3", "ethnicity_sex.csv"))

# Ethnicity breakdown by Age and sex

cols_of_interest <- c("count");

ethnicity_age_sex_raw <- df %>%
  group_by(ethnicity_Combined, age_band, sex) %>%
  summarise(count = n());

fwrite(ethnicity_age_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_age_sex_raw.csv"))

ethnicity_age_sex <- df %>%
  group_by(ethnicity_Combined, age_band, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_age_sex, here::here("output", "os_reports", "WP3", "ethnicity_age_sex.csv"))

# Ethnicity breakdown by Age and IMD

cols_of_interest <- c("count");

ethnicity_age_imd_raw <- df %>%
  group_by(ethnicity_Combined, age_band, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_age_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_age_imd_raw.csv"))

ethnicity_age_imd <- df %>%
  group_by(ethnicity_Combined, age_band, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_age_imd, here::here("output", "os_reports", "WP3", "ethnicity_age_imd.csv"))

# Ethnicity breakdown by Age and sex and IMD

cols_of_interest <- c("count");

ethnicity_age_sex_imd_raw <- df %>%
  group_by(ethnicity_Combined, age_band, sex, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_age_sex_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_age_sex_imd_raw.csv"))

ethnicity_age_sex_imd <- df %>%
  group_by(ethnicity_Combined, age_band, sex, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_age_sex_imd, here::here("output", "os_reports", "WP3", "ethnicity_age_sex_imd.csv"))

# Ethnicity breakdown by sex and IMD

cols_of_interest <- c("count");

ethnicity_sex_imd_raw <- df %>%
  group_by(ethnicity_Combined, sex, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_sex_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_sex_imd_raw.csv"))

ethnicity_sex_imd <- df %>%
  group_by(ethnicity_Combined, sex, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_sex_imd, here::here("output", "os_reports", "WP3", "ethnicity_sex_imd.csv"))

# Ethnicity breakdown by place of death:

cols_of_interest <- c("count");

ethnicity_pod_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new) %>%
  summarise(count = n());

fwrite(ethnicity_pod_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_raw.csv"))

ethnicity_pod <- df %>%
  group_by(ethnicity_Combined, pod_ons_new) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod, here::here("output", "os_reports", "WP3", "ethnicity_pod.csv"))

# Ethnicity breakdown by place of death and age:

cols_of_interest <- c("count");

ethnicity_pod_age_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band) %>%
  summarise(count = n());

fwrite(ethnicity_pod_age_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_age_raw.csv"))

ethnicity_pod_age <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod_age, here::here("output", "os_reports", "WP3", "ethnicity_pod_age.csv"))

# Ethnicity breakdown by place of death and imd:

cols_of_interest <- c("count");

ethnicity_pod_imd_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_pod_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_imd_raw.csv"))

ethnicity_pod_imd <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod_imd, here::here("output", "os_reports", "WP3", "ethnicity_pod_imd.csv"))

# Ethnicity breakdown by place of death and sex

  cols_of_interest <- c("count");

ethnicity_pod_sex_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, sex) %>%
  summarise(count = n());

fwrite(ethnicity_pod_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_sex_raw.csv"))

ethnicity_pod_sex <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod_sex, here::here("output", "os_reports", "WP3", "ethnicity_pod_sex.csv"))

# Ethnicity breakdown by place of death and age and IMD

cols_of_interest <- c("count");

ethnicity_pod_age_imd_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_pod_age_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_age_imd_raw.csv"))

ethnicity_pod_age_imd <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod_age_imd, here::here("output", "os_reports", "WP3", "ethnicity_pod_age_imd.csv"))


# Ethnicity breakdown by place of death and age and IMD and sex

cols_of_interest <- c("count");

ethnicity_pod_age_imd_sex_raw <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band, imd_quintile, sex) %>%
  summarise(count = n());

fwrite(ethnicity_pod_age_imd_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_pod_age_imd_sex_raw.csv"))

ethnicity_pod_age_imd_sex <- df %>%
  group_by(ethnicity_Combined, pod_ons_new, age_band, imd_quintile, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_pod_age_imd_sex, here::here("output", "os_reports", "WP3", "ethnicity_pod_age_imd_sex.csv"))

# Ethnicity breakdown by cause of death:

cols_of_interest <- c("count");

ethnicity_Cod_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_raw.csv"))

ethnicity_Cod <- df %>%
  group_by(ethnicity_Combined, Codgrp) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod, here::here("output", "os_reports", "WP3", "ethnicity_Cod.csv"))

# Ethnicity breakdown by cause of death and age:

cols_of_interest <- c("count");

ethnicity_Cod_age_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_age_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age_raw.csv"))

ethnicity_Cod_age <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod_age, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age.csv"))

# Ethnicity breakdown by cause of death and imd:

cols_of_interest <- c("count");

ethnicity_Cod_imd_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_imd_raw.csv"))

ethnicity_Cod_imd <- df %>%
  group_by(ethnicity_Combined, Codgrp, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod_imd, here::here("output", "os_reports", "WP3", "ethnicity_Cod_imd.csv"))

# Ethnicity breakdown by cause of death and sex

cols_of_interest <- c("count");

ethnicity_Cod_sex_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp, sex) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_sex_raw.csv"))

ethnicity_Cod_sex <- df %>%
  group_by(ethnicity_Combined, Codgrp, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod_sex, here::here("output", "os_reports", "WP3", "ethnicity_Cod_sex.csv"))

# Ethnicity breakdown by cause of death and age and IMD

cols_of_interest <- c("count");

ethnicity_Cod_age_imd_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band, imd_quintile) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_age_imd_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age_imd_raw.csv"))

ethnicity_Cod_age_imd <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band, imd_quintile) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod_age_imd, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age_imd.csv"))


# Ethnicity breakdown by cause of death and age and IMD and sex

cols_of_interest <- c("count");

ethnicity_Cod_age_imd_sex_raw <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band, imd_quintile, sex) %>%
  summarise(count = n());

fwrite(ethnicity_Cod_age_imd_sex_raw, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age_imd_sex_raw.csv"))

ethnicity_Cod_age_imd_sex <- df %>%
  group_by(ethnicity_Combined, Codgrp, age_band, imd_quintile, sex) %>%
  summarise(count = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ recause(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(ethnicity_Cod_age_imd_sex, here::here("output", "os_reports", "WP3", "ethnicity_Cod_age_imd_sex.csv"))




