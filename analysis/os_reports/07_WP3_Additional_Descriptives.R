##########################################################
# Additional descriptives to explore impact of demographic factors on end of life care
# Author: Miranda & Sophie 
# Date: 13/05/24 
# Initial aim: Descriptive analysis to explore specific service use measures by place of death and cause of death in combination. 
##############################################################

# Analysis over a two calendar year period (2022/2023)

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
                              , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                              , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                              , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                              , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                              , TRUE ~ "All other causes")) %>%
  filter(study_month >= startdate & study_month <= enddate) 

# GP interactions

cols_of_interest <- c("sum");

# Total number of general practice interactions by month by place of death and cause of death

gp_pod_cod_TOTAL <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
  summarise(sum = sum(gp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(gp_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(gp_pod_cod_TOTAL, here::here("output", "os_reports", "WP3", "gp_pod_cod_TOTAL.csv"))

# Number of people with at least one general practice interaction in the last month of life by month, place of death and cause of death - all deaths

cols_of_interest <- c("count", "total");

gp_count_pod_cod_RAW <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(gp_count_pod_cod_RAW, here::here("output", "os_reports", "WP3", "gp_count_pod_cod_RAW.csv"))

gp_count_pod_cod_ROUND <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
  summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(gp_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(gp_count_pod_cod_ROUND, here::here("output", "os_reports", "WP3", "gp_count_pod_cod_ROUND.csv"))

# Mean GP interactions by month, place of death and cause of death- including all deaths (a version including counts (not for release) and a version exlcuding counts)

gp_pod_cod_MEAN_RAW <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
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

fwrite(gp_pod_cod_MEAN_RAW, here::here("output", "os_reports", "WP3", "gp_pod_cod_MEAN_RAW.csv"))


gp_pod_cod_MEAN <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
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

fwrite(gp_pod_cod_MEAN, here::here("output", "os_reports", "WP3", "gp_pod_cod_MEAN.csv"))

# Outcome variable = A&E attendances

cols_of_interest <- c("sum");

# Total number of A&E visits by month, place of death and cause of death - including all deaths

aevis_pod_cod_TOTAL <- df %>%
  group_by(study_month, pod_ons_new, codgrp) %>%
  summarise(sum = sum(aevis_1m, na.rm=TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(sum = sum(aevis_1m, na.rm=TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)); 

fwrite(aevis_pod_cod_TOTAL, here::here("output", "os_reports", "WP3", "aevis_pod_cod_TOTAL.csv"))

# Number of people with at least one A&E visit in the last month of life by month and place of death - all deaths

cols_of_interest <- c("count", "total");

aevis_count_pod_cod_RAW <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All") %>%
              mutate(proportion = round(count / total*100,1)))

fwrite(aevis_count_pod_cod_RAW, here::here("output", "os_reports", "WP3", "aevis_count_pod_cod_RAW.csv"))

aevis_count_pod_cod_ROUND <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_1m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count/total*100,1))

fwrite(aevis_count_pod_cod_ROUND, here::here("output", "os_reports", "WP3", "aevis_count_pod_cod_ROUND.csv"))

# mean A&E visits in month leading up to death, by month, place of death and cause of death (version including count not for release)

aevis_pod_cod_MEAN_RAW <- df %>% 
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

fwrite(aevis_pod_cod_MEAN_RAW, here::here("output", "os_reports", "WP3", "aevis_pod_cod_MEAN_RAW.csv"))


aevis_pod_cod_MEAN <- df %>% 
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

fwrite(aevis_pod_cod_MEAN, here::here("output", "os_reports", "WP3", "aevis_pod_cod_MEAN.csv"))







