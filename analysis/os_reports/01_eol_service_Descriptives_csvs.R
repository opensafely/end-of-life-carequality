###############################################################################
# CSV files for end of life care descriptive analysis
# Date: 26.07.2023
# Author: Eilís & Miranda 
# Aim: Create CSV files to show measures by total volume, proportion of patients as well as group level means
# Note: Files are created with and without rounding / redaction. Non-rounded/redacted files are not for release
# PNG/Tables are created using these CSVs in file 02_eol_service_Descriptive_outputs
# Measures include: 
# Deaths in period
# Use of medications for symptom management
# General practice interactions
# A&E visits
# Outpatient appointments
# Elective admissions
# Emergency admissions
# Community nurse contacts
###############################################################################

# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)

# Create folder structure

fs::dir_create("output", "os_reports", "eol_service")

# Code settings

startdate <- dmy("01-12-2018")
enddate <- dmy("31-08-2023")

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


# Deaths in period 

# Number of deaths by month and place of death - including all deaths

cols_of_interest <- c("count");

deaths_month_place <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(deaths_month_place, here::here("output", "os_reports", "eol_service", "deaths_month_place.csv"))

# Number of deaths by month and cause of death

deaths_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(deaths_month_cod, here::here("output", "os_reports", "eol_service", "deaths_month_cod.csv"))


# Use of medications for symptom management 

cols_of_interest <- c("sum");

# Total number of medications prescribed for symptom management by month and place of death - including all deaths

eol_med_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(eol_med_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(eol_med_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(eol_med_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "eol_med_month_place_TOTAL.csv"))

# Total number of medications prescribed for symptom management by month and cause of death

eol_med_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(eol_med_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(eol_med_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(eol_med_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "eol_med_month_cod_TOTAL.csv"))


# Number of people with at least one medication prescribed for symptom management in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

eol_med_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows (df%>%
               group_by(study_month) %>%
               summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
               mutate (pod_ons_new = "All") %>%
               mutate (proportion = round(count / total *100,1)))

fwrite(eol_med_count_place_RAW, here::here("output", "os_reports", "eol_service", "eol_med_count_place_RAW.csv"))

eol_med_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate (proportion = round(count / total * 100, 1))
  
fwrite(eol_med_count_place_ROUND, here::here("output", "os_reports", "eol_service", "eol_med_count_place_ROUND.csv"))


# Number of people with at least one medication prescribed for symptom management in the last month of life by month and cause of death

eol_med_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows (df%>%
               group_by(study_month) %>%
               summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
               mutate (codgrp = "All") %>%
               mutate (proportion = round(count / total *100,1)))

fwrite(eol_med_count_cause_RAW, here::here("output", "os_reports", "eol_service", "eol_med_count_cause_RAW.csv"))


eol_med_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp= "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate (proportion = round(count / total * 100, 1))

fwrite(eol_med_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "eol_med_count_cause_ROUND.csv"))

# Medication use by place of death - including all deaths

eol_med_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(eol_med_1m, na.rm=TRUE),
            sd = sd(eol_med_1m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eol_med_1m, na.rm=TRUE),
                        sd = sd(eol_med_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(eol_med_month_raw, here::here("output", "os_reports", "eol_service", "eol_med_month_raw.csv"))


eol_med_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(eol_med_1m, na.rm=TRUE),
            sd = sd(eol_med_1m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
bind_rows(df %>%
            group_by(study_month) %>%
            summarise(count = n(),
                      mean = mean(eol_med_1m, na.rm=TRUE),
                      sd = sd(eol_med_1m, na.rm=TRUE)) %>%
            mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(eol_med_month, here::here("output", "os_reports", "eol_service", "eol_med_month.csv"))

# Medication use by month and cause of death

eol_med_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(eol_med_1m, na.rm = TRUE),
            sd = sd(eol_med_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eol_med_1m, na.rm = TRUE),
                        sd = sd(eol_med_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 
 
fwrite(eol_med_month_cod_raw, here::here("output", "os_reports", "eol_service", "eol_med_month_cod_raw.csv"))


eol_med_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(eol_med_1m, na.rm = TRUE),
            sd = sd(eol_med_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eol_med_1m, na.rm = TRUE),
                        sd = sd(eol_med_1m, na.rm=TRUE)) %>%
  mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(eol_med_month_cod, here::here("output", "os_reports", "eol_service", "eol_med_month_cod.csv"))

# General practice interactions 

cols_of_interest <- c("sum");

# Total number of general practice interactions by month and place of death - including all deaths

gp_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(gp_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(gp_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(gp_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "gp_month_place_TOTAL.csv"))


# Total number of general practice interactions by month and cause of death

gp_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(gp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(gp_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(gp_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "gp_month_cod_TOTAL.csv"))


# Number of people with at least one general practice interaction in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

gp_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(gp_count_place_RAW, here::here("output", "os_reports", "eol_service", "gp_count_place_RAW.csv"))

gp_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(gp_count_place_ROUND, here::here("output", "os_reports", "eol_service", "gp_count_place_ROUND.csv"))


# Number of people with at least one general practice interaction in the last month of life by month and cause of death

gp_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(gp_count_cause_RAW, here::here("output", "os_reports", "eol_service", "gp_count_cause_RAW.csv"))


gp_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(gp_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "gp_count_cause_ROUND.csv"))


# Mean GP interactions by month and place of death - including all deaths (a version including counts (not for release) and a version exlcuding counts)

gp_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(gp_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(gp_month_raw, here::here("output", "os_reports", "eol_service", "gp_month_raw.csv"))


gp_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(gp_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(gp_month, here::here("output", "os_reports", "eol_service", "gp_month.csv"))

# Mean GP interactions by month and cause of death

gp_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(gp_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(gp_month_cod_raw, here::here("output", "os_reports", "eol_service", "gp_month_cod_raw.csv"))


gp_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(gp_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(gp_month_cod, here::here("output", "os_reports", "eol_service", "gp_month_cod.csv"))

# A&E visits 

cols_of_interest <- c("sum");

# Total number of A&E visits by month and place of death - including all deaths

aevis_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(aevis_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(aevis_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(aevis_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "aevis_month_place_TOTAL.csv"))


# Total number of A&E visits by month and cause of death

aevis_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(aevis_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(aevis_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(aevis_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "aevis_month_cod_TOTAL.csv"))


# Number of people with at least one A&E visit in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

aevis_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(aevis_count_place_RAW, here::here("output", "os_reports", "eol_service", "aevis_count_place_RAW.csv"))

aevis_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count/total*100,1))

fwrite(aevis_count_place_ROUND, here::here("output", "os_reports", "eol_service", "aevis_count_place_ROUND.csv"))


# Number of people with at least one A&E visit in the last month of life by month and cause of death

aevis_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(aevis_count_cause_RAW, here::here("output", "os_reports", "eol_service", "aevis_count_cause_RAW.csv"))


aevis_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(aevis_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "aevis_count_cause_ROUND.csv"))

# mean A&E visits in month leading up to death, by month, by place of death (version including count not for release)

aevis_month_raw <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(aevis_1m, na.rm = TRUE),
                        sd = sd(aevis_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 

fwrite(aevis_month_raw, here::here("output", "os_reports", "eol_service", "aevis_month_raw.csv"))


aevis_month <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(aevis_1m, na.rm = TRUE),
                        sd = sd(aevis_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(aevis_month, here::here("output", "os_reports", "eol_service", "aevis_month.csv"))


# Mean A&E visits in month leading up to death, by month, by cause of death

aevis_month_cod_raw <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count =n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%          
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(aevis_1m, na.rm = TRUE)
                        , sd = sd(aevis_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(aevis_month_cod_raw, here::here("output", "os_reports", "eol_service", "aevis_month_cod_raw.csv"))


aevis_month_cod <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count =n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%          
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                mean = mean(aevis_1m, na.rm = TRUE)
                , sd = sd(aevis_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(aevis_month_cod, here::here("output", "os_reports", "eol_service", "aevis_month_cod.csv"))


# Outpatient appointments 

cols_of_interest <- c("sum");

# Total number of outpatient appointments by month and place of death - including all deaths

opapp_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(opapp_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(opapp_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(opapp_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "opapp_month_place_TOTAL.csv"))


# Total number of outpatient appointments by month and cause of death

opapp_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(opapp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(opapp_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(opapp_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "opapp_month_cod_TOTAL.csv"))


# Number of people with at least one outpatient appointment in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

opapp_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(opapp_count_place_RAW, here::here("output", "os_reports", "eol_service", "opapp_count_place_RAW.csv"))

opapp_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(opapp_count_place_ROUND, here::here("output", "os_reports", "eol_service", "opapp_count_place_ROUND.csv"))


# Number of people with at least one outpatient appointment in the last month of life by month and cause of death

opapp_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(opapp_count_cause_RAW, here::here("output", "os_reports", "eol_service", "opapp_count_cause_RAW.csv"))


opapp_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(opapp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(opapp_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "opapp_count_cause_ROUND.csv"))


# Mean outpatient appointments by month and place of death - including all deaths (versions including and excluding counts. Version including counts not for release)
opapp_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm =TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(opapp_1m, na.rm = TRUE),
                        sd = sd(opapp_1m, na.rm =TRUE))%>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(opapp_month_raw, here::here("output", "os_reports", "eol_service", "opapp_month_raw.csv"))


opapp_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm =TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
              mean = mean(opapp_1m, na.rm = TRUE),
              sd = sd(opapp_1m, na.rm =TRUE))%>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(opapp_month, here::here("output", "os_reports", "eol_service", "opapp_month.csv"))


# Mean outpatient appointments by month and cause of death - including all deaths (version including and excluding counts. Version including counts not for release)

opapp_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(opapp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(opapp_month_cod_raw, here::here("output", "os_reports", "eol_service", "opapp_month_cod_raw.csv"))


opapp_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                mean = mean(opapp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(opapp_month_cod, here::here("output", "os_reports", "eol_service", "opapp_month_cod.csv"))


# Elective admissions

cols_of_interest <- c("sum");

# Total number of elective admissions by month and place of death - including all deaths

eladm_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(eladm_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(eladm_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(eladm_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "eladm_month_place_TOTAL.csv"))

# Total number of elective admissions by month and cause of death

eladm_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(eladm_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(eladm_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(eladm_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "eladm_month_cod_TOTAL.csv"))


# Number of people with at least one elective admission in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

eladm_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(eladm_count_place_RAW, here::here("output", "os_reports", "eol_service", "eladm_count_place_RAW.csv"))

eladm_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(eladm_count_place_ROUND, here::here("output", "os_reports", "eol_service", "eladm_count_place_ROUND.csv"))


# Number of people with at least one elective admission in the last month of life by month and cause of death

eladm_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(eladm_count_cause_RAW, here::here("output", "os_reports", "eol_service", "eladm_count_cause_RAW.csv"))


eladm_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eladm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(eladm_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "eladm_count_cause_ROUND.csv"))


# Mean elective admissions by month and place of death - including all deaths (version including and excluding counts. Version including count not for release)

eladm_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(eladm_1m, na.rm = TRUE)
            , sd = sd(eladm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eladm_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 

fwrite(eladm_month_raw, here::here("output", "os_reports", "eol_service", "eladm_month_raw.csv"))


eladm_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(eladm_1m, na.rm = TRUE)
            , sd = sd(eladm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                      mean = mean(eladm_1m, na.rm = TRUE),
                      sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(eladm_month, here::here("output", "os_reports", "eol_service", "eladm_month.csv"))


# Mean elective admissions by month and cause of death (versions including and excluding counts. Version including count not for release)

eladm_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(eladm_1m, na.rm = TRUE)
            , sd = sd(eladm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eladm_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 

fwrite(eladm_month_cod_raw, here::here("output", "os_reports", "eol_service", "eladm_month_cod_raw.csv"))


eladm_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(eladm_1m, na.rm = TRUE)
            , sd = sd(eladm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(eladm_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(eladm_month_cod, here::here("output", "os_reports", "eol_service", "eladm_month_cod.csv"))


# Emergency admissions

cols_of_interest <- c("sum");

# Total number of emergency admissions by month and place of death - including all deaths

emadm_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(emadm_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(emadm_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(emadm_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "emadm_month_place_TOTAL.csv"))

# Total number of emergency admissions by month and cause of death

emadm_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(emadm_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(emadm_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(emadm_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "emadm_month_cod_TOTAL.csv"))


# Number of people with at least one emergency admission in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

emadm_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
           group_by(study_month) %>%
           summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>% 
           mutate(pod_ons_new = "All") %>% 
           mutate(proportion = round(count / total*100,1)))
         
fwrite(emadm_count_place_RAW, here::here("output", "os_reports", "eol_service", "emadm_count_place_RAW.csv"))


emadm_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>% 
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(emadm_count_place_ROUND, here::here("output", "os_reports", "eol_service", "emadm_count_place_ROUND.csv"))


# Number of people with at least one emergency admission in the last month of life by month and cause of death
emadm_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))
  
fwrite(emadm_count_cause_RAW, here::here("output", "os_reports", "eol_service", "emadm_count_cause_RAW.csv"))


emadm_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(emadm_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(emadm_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "emadm_count_cause_ROUND.csv"))


# Mean emergency admissions by month and place of death - including all deaths (Versions including and excluding counts. Version including count not for release)

emadm_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(emadm_1m, na.rm = TRUE)
            , sd = sd(emadm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(emadm_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(emadm_month_raw, here::here("output", "os_reports", "eol_service", "emadm_month_raw.csv"))


emadm_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(emadm_1m, na.rm = TRUE)
            , sd = sd(emadm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
              mean = mean(emadm_1m, na.rm = TRUE),
              sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(emadm_month, here::here("output", "os_reports", "eol_service", "emadm_month.csv"))


# Mean emergency admissions by month and cause of death

emadm_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(emadm_1m, na.rm = TRUE)
            , sd = sd(emadm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(emadm_1m, na.rm = TRUE),
                        sd = sd(emadm_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ )))

fwrite(emadm_month_cod_raw, here::here("output", "os_reports", "eol_service", "emadm_month_cod_raw.csv"))


emadm_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(emadm_1m, na.rm = TRUE)
            , sd = sd(emadm_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
              mean = mean(emadm_1m, na.rm = TRUE),
              sd = sd(emadm_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(emadm_month_cod, here::here("output", "os_reports", "eol_service", "emadm_month_cod.csv"))


# Community nurse contacts

cols_of_interest <- c("sum");

# Total number of community nursing contacts by month and place of death - including all deaths

nursing_month_place_TOTAL <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(sum = sum(nursing_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(nursing_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(nursing_month_place_TOTAL, here::here("output", "os_reports", "eol_service", "nursing_month_place_TOTAL.csv"))

# Total number of community nursing contacts by month and cause of death

nursing_month_cod_TOTAL <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(nursing_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(nursing_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(nursing_month_cod_TOTAL, here::here("output", "os_reports", "eol_service", "nursing_month_cod_TOTAL.csv"))

nursing_month_cod_TOTAL2 <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(sum = sum(nursing_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(nursing_1m, na.rm=TRUE)) %>%
              mutate(codgrp = "All"));

fwrite(nursing_month_cod_TOTAL2, here::here("output", "os_reports", "eol_service", "nursing_month_cod_TOTAL2.csv"))


# Number of people with at least one community nursing contact in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

nursing_count_place_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(nursing_count_place_RAW, here::here("output", "os_reports", "eol_service", "nursing_count_place_RAW.csv"))


nursing_count_place_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(nursing_count_place_ROUND, here::here("output", "os_reports", "eol_service", "nursing_count_place_ROUND.csv"))


# Number of people with at least one community nursing contact in the last month of life by month and cause of death

nursing_count_cause_RAW <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df%>%
              group_by(study_month) %>%
              summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(nursing_count_cause_RAW, here::here("output", "os_reports", "eol_service", "nursing_count_cause_RAW.csv"))


nursing_count_cause_ROUND <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(nursing_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
              dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
              mutate(proportion = round(count / total*100,1))

fwrite(nursing_count_cause_ROUND, here::here("output", "os_reports", "eol_service", "nursing_count_cause_ROUND.csv"))


# Mean number of community nurse contacts by month and place of death - including all deaths (versions including and excluding counts. Version including counts not for release)

nursing_month_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(nursing_1m, na.rm = TRUE)
            , sd = sd(nursing_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(nursing_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 

fwrite(nursing_month_raw, here::here("output", "os_reports", "eol_service", "nursing_month_raw.csv"))


nursing_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n(),
            mean = mean(nursing_1m, na.rm = TRUE)
              , sd = sd(nursing_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(nursing_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))

fwrite(nursing_month, here::here("output", "os_reports", "eol_service", "nursing_month.csv"))


# Mean community nursing interactions by month and cause of death (versions including and excluding counts. Version including counts not for release)

nursing_month_cod_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(nursing_1m, na.rm = TRUE)
            , sd = sd(nursing_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(nursing_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) 

fwrite(nursing_month_cod_raw, here::here("output", "os_reports", "eol_service", "nursing_month_cod_raw.csv"))


nursing_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = n(),
            mean = mean(nursing_1m, na.rm = TRUE)
            , sd = sd(nursing_1m, na.rm = TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n(),
                        mean = mean(nursing_1m, na.rm = TRUE),
                        sd = sd(gp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  select(-c(count))
 
fwrite(nursing_month_cod, here::here("output", "os_reports", "eol_service", "nursing_month_cod.csv"))