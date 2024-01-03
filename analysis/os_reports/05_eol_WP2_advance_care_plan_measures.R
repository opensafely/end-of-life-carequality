#-------------------------------------------------------------------------------
# Tables for WP2_quality_indicators
# Date: 18.09.2023 onwards
# Author: Sophie (and Stuti)
# Aim: Create quality indicators for end of life care.
#-------------------------------------------------------------------------------

# Load packages -----------------------------------------------------------
#install.packages(c("tidyverse", "lubridate", "here", "plyr"))
library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(data.table)

# Create folder structure -------------------------------------------------

fs::dir_create("output", "os_reports", "WP2_quality_indicators")

# Code settings -----------------------------------------------------------
# need to decide on this
startdate <- dmy("01-03-2019")
enddate <- dmy("31-08-2023")


# Prepare data ------------------------------------------------------------

df <- read_csv(file = here::here("output", "os_reports", "input_os_reports.csv.gz")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , study_month = floor_date(dod_ons, unit = "month")
         , study_quarter = case_when(month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2019 ~ 1
                                     , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2019 ~ 2
                                     , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2019 ~ 3
                                     , (month(dod_ons) == 12 & year(dod_ons) == 2019) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2020) ~ 4
                                     , month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2020 ~ 5
                                     , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2020 ~ 6
                                     , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2020 ~ 7
                                     , (month(dod_ons) == 12 & year(dod_ons) == 2020) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2021) ~ 8
                                     , month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2021 ~ 9
                                     , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2021 ~ 10
                                     , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2021 ~11
                                     , (month(dod_ons) == 12 & year(dod_ons) == 2021) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2022) ~ 12
                                     , month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2022 ~ 13
                                     , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2022 ~ 14
                                     , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2022 ~ 15
                                     , (month(dod_ons) == 12 & year(dod_ons) == 2022) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2023) ~ 16
                                     , month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2023 ~ 17
                                     , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2023 ~ 18)  
         , pod_ons_new = case_when(pod_ons == "Elsewhere" 
                                   | pod_ons == "Other communal establishment" ~ "Elsewhere/other"
                                   , TRUE ~ as.character(pod_ons))
         , cod_ons_3 = str_sub(cod_ons, 1, 3)
         , cod_ons_4 = str_sub(cod_ons, 1, 5)
         , codgrp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                              , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                              , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") 
                              | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                              , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                              , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                              , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                              , TRUE ~ "All other causes")) %>%
  filter(study_month >= startdate & study_month <= enddate) 

#----------------------------------------------------#
cols_of_interest <- c("count", "total")

#Advance care planning ----------------------------

#Number and proportion of patients with an advanced care plan in the last 90 days (3 months) of life by place of death

#Quarterly breakdown

#Rounded estimates

acp_pod_quarters_rounded <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_pod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp_pod_quarters_rounded.csv"))

#Raw estimates

acp_pod_quarters_raw <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_pod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp_pod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acp_pod_months_rounded <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_pod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp_pod_months_rounded.csv"))

#Raw estimates

acp_pod_months_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_pod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp_pod_months_raw.csv"))

#Number and proportion of patients with a care plan in the last 90 days (3 months) of life by cause of death

#Quarterly breakdown

#Rounded estimates

acp_cod_quarters_rounded <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_cod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp_cod_quarters_rounded.csv"))

#Raw estimates

acp_cod_quarters_raw <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_cod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp_cod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acp_cod_months_rounded <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_cod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp_cod_months_rounded.csv"))

#Raw estimates

acp_cod_months_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(acp_cod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp_cod_months_raw.csv"))

#Average number of care plan codes recorded per person during the last 90 days (3 months) of life by place of death

#Quarterly breakdown 

#Rounded estimates

acp3m_pod_quarters_rounded <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All") %>% 
              mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acp3m_pod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_pod_quarters_rounded.csv"))

#Raw estimates

acp3m_pod_quarters_raw <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All"))

fwrite(acp3m_pod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_pod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acp3m_pod_months_rounded <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All") %>% 
              mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acp3m_pod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_pod_months_rounded.csv"))

#Raw estimates

acp3m_pod_months_raw <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All"))

fwrite(acp3m_pod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_pod_months_raw.csv"))

#Average number of care plan codes recorded per person during the last 90 days (3 months) of life by cause of death

#Quarterly breakdown 

#Rounded estimates

acp3m_cod_quarters_rounded <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(codgrp = "All") %>% 
              mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acp3m_cod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_cod_quarters_rounded.csv"))

#Raw estimates

acp3m_cod_quarters_raw <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(codgrp = "All"))

fwrite(acp3m_cod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_cod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acp3m_cod_months_rounded <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(codgrp = "All") %>% 
              mutate(across(c(mean, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acp3m_cod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_cod_months_rounded.csv"))

#Raw estimates

acp3m_cod_months_raw <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(careplan_3m, na.rm=TRUE),
            sd = sd(careplan_3m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(careplan_3m, na.rm=TRUE),
                        sd = sd(careplan_3m, na.rm=TRUE)) %>%
              mutate(codgrp = "All"))

fwrite(acp3m_cod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acp3m_cod_months_raw.csv"))

#Duration of time for which care plan has been active by place of death

#Quarterly breakdown

#Rounded estimates

acpdur_pod_quarters_rounded <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All") %>% 
              mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acpdur_pod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_pod_quarters_rounded.csv"))

#Raw estimates

acpdur_pod_quarters_raw <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All"))

fwrite(acpdur_pod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_pod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acpdur_pod_months_rounded <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All") %>% 
              mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acpdur_pod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_pod_months_rounded.csv"))

#Raw estimates

acpdur_pod_months_raw <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All"))

fwrite(acpdur_pod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_pod_months_raw.csv"))

#Duration of time for which care plan has been active by cause of death

#Quarterly breakdown 

#Rounded estimates

acpdur_cod_quarters_rounded <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(codgrp = "All") %>% 
              mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acpdur_cod_quarters_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_cod_quarters_rounded.csv"))

#Raw estimates

acpdur_cod_quarters_raw <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(codgrp = "All"))

fwrite(acpdur_cod_quarters_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_cod_quarters_raw.csv"))

#Monthly breakdown

#Rounded estimates

acpdur_cod_months_rounded <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(codgrp = "All") %>% 
              mutate(across(c(mean, median, sd), ~case_when(count> 7 ~ .x, count ==0 ~ 0, TRUE ~ NA_real_ ))))

fwrite(acpdur_cod_months_rounded, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_cod_months_rounded.csv"))

#Raw estimates

acpdur_cod_months_raw <- df %>% 
  filter(length_careplan >= 0) %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
            mean = mean(length_careplan, na.rm=TRUE),
            median = median(length_careplan, na.rm = TRUE),
            sd = sd(length_careplan, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(has_careplan == 'TRUE', na.rm = TRUE),
                        mean = mean(length_careplan, na.rm=TRUE),
                        median = median(length_careplan, na.rm = TRUE),
                        sd = sd(length_careplan, na.rm=TRUE)) %>%
              mutate(codgrp = "All"))

fwrite(acpdur_cod_months_raw, here::here("output", "os_reports", "WP2_quality_indicators", "acpdur_cod_months_raw.csv"))
