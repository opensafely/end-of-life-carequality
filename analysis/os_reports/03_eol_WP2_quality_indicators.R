#-------------------------------------------------------------------------------
# Charts for WP2_quality_indicators
# Date: 18.09.2023
# Author: Sophie
# Aim: Create quality indicators for end of life care.Each indicator will look back over the last 90 days of life. 
#-------------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
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

#Col of interest for redaction------------------------------ 
cols_of_interest <- c("count", "total")
  
#Palliative care recorded, with rounding ----------------------------------------------------
#By place of death
proportion_palcare_pod_rounding <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_palcare_pod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_ROUND.csv")))

#By cause of death 
proportion_palcare_cod_rounding <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_palcare_cod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_ROUND.csv")))

#Palliative care recorded not for release--------------------------
#By place of death
proportion_palcare_pod <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))
  

fwrite(proportion_palcare_pod, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_RAW.csv")))

#By cause of death 
proportion_palcare_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_palcare_cod, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_RAW.csv")))

#At least one A&E visits last 3 months of life, with rounding --------------------------
#By place of death 
proportion_aevis1_3m_pod_rounding <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>% 
  bind_rows(df %>%
              group_by(study_month) %>% 
            summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
            mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_pod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_ROUND.csv")))

#By cause of death 
proportion_aevis1_3m_cod_rounding <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>% 
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1)) 

fwrite(proportion_aevis1_3m_cod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_ROUND.csv")))


#At least one A&E visits last 3 months of life, not for release --------------------------
#By place of death 
proportion_aevis1_3m_pod <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_aevis1_3m_pod, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_RAW.csv")))

#By cause of death 
proportion_aevis1_3m_cod <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_cod, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_RAW.csv")))

#At least 3 A&E visits last 3 months of life, with rounding --------------------------
#By place of death 
proportion_aevis3_3m_pod_rounding <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_pod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_ROUND.csv")))

#By cause of death 
proportion_aevis3_3m_cod_rounding <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1)) 

fwrite(proportion_aevis3_3m_cod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_ROUND.csv")))


#At least 3 A&E visits last 3 months of life, not for release --------------------------
#By place of death
proportion_aevis3_3m_pod <- df %>% 
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>% 
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1)) 

fwrite(proportion_aevis3_3m_pod, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_RAW.csv")))

#By cause of death 
proportion_aevis3_3m_cod <- df %>% 
  group_by(study_month, codgrp) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_cod, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_RAW.csv")))

### Medications ###

#The proportion of people with medications prescribed for symptom management in the last three months of life

#With rounding 

#By place of death 

proportion_eolmed_pod_rounding <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_eolmed_pod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_ROUND.csv")))

#By cause of death 
proportion_eolmed_cod_rounding <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_eolmed_cod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_ROUND.csv")))

#Without rounding - not for release

#By place of death - new code-----------------
proportion_eolmed_pod <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_eolmed_pod, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_RAW.csv")))

#By cause of death 
proportion_eolmed_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_eolmed_cod, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_RAW.csv")))


##### Quarterly- in case of small numbers #####

#Quarterly Palliative care recorded, with rounding ----------------------------------------------------

#By place of death
proportion_palcare_pod_rounding_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_palcare_pod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_quarter_ROUND.csv")))

#By cause of death 
proportion_palcare_cod_rounding_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_palcare_cod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_quarter_ROUND.csv")))

#Quarterly Palliative care recorded, not for release--------------------------
#By place of death 
proportion_palcare_pod_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_palcare_pod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_pod_quarter_RAW.csv")))

#By cause of death 
proportion_palcare_cod_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>% 
              group_by(study_quarter) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_palcare_cod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "palcare_cod_quarter_RAW.csv")))


#Quarterly At least one A&E visits last 3 months of life, with rounding --------------------------
#By place of death 
proportion_aevis1_3m_pod_rounding_quarter <- df %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_pod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_quarter_ROUND.csv")))

#By cause of death 
proportion_aevis1_3m_cod_rounding_quarter <- df %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_cod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_quarter_ROUND.csv")))

#Quarterly At least one A&E visits last 3 months of life, not for release --------------------------
#By place of death 
proportion_aevis1_3m_pod_quarter <- df %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_pod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_pod_quarter_RAW.csv")))

#By cause of death 
proportion_aevis1_3m_cod_quarter <- df %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis1_3m_cod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis1_cod_quarter_RAW.csv")))

#Quarterly At least 3 A&E visits last 3 months of life, with rounding --------------------------
#By place of death 
proportion_aevis3_3m_pod_rounding_quarter <- df %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_pod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_quarter_ROUND.csv")))

#By cause of death 
proportion_aevis3_3m_cod_rounding_quarter <- df %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_cod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_quarter_ROUND.csv")))


#Quarterly At least 3 A&E visits last 3 months of life, not for release --------------------------
#By place of death
proportion_aevis3_3m_pod_quarter <- df %>% 
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_pod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_pod_quarter_RAW.csv")))

#By cause of death 
proportion_aevis3_3m_cod_quarter <- df %>% 
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(aevis_3m >= 3, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_aevis3_3m_cod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "aevis3_cod_quarter_RAW.csv")))

###### Quarterly medications prescribed for symptom management in the last three months of life

#Quarterly medications for symptom management recorded, with rounding ----------------------------------------------------
#By place of death
proportion_eolmed_pod_rounding_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_eolmed_pod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_quarter_ROUND.csv")))

#By cause of death 
proportion_eolmed_cod_rounding_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_eolmed_cod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_quarter_ROUND.csv")))

#Quarterly medications prescribed for symptom management recorded, not for release--------------------------
#By place of death 
proportion_eolmed_pod_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_eolmed_pod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_pod_quarter_RAW.csv")))

#By cause of death 
proportion_eolmed_cod_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>% 
              group_by(study_quarter) %>%
              summarise(count = sum(eol_med_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_eolmed_cod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "eolmed_cod_quarter_RAW.csv")))

### New quality measures-------------------------------
#Specialist palliative care, with rounding---------------------
#Place of death
proportion_spec_pod_rounding <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_spec_pod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_ROUND.csv")))

#cause of death 
proportion_spec_cod_rounding <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_spec_cod_rounding, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_ROUND.csv")))

#Specialist palliative care- not for release--------------------
#Place of death
proportion_spec_pod <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_spec_pod, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_RAW.csv")))

#cause of death 
proportion_spec_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_spec_cod, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_RAW.csv")))

##quarterly specialist palliative care --------------
#Place of death
proportion_spec_pod_rounding_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_spec_pod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_quarter_ROUND.csv")))

#cause of death 
proportion_spec_cod_rounding_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_spec_cod_rounding_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_quarter_ROUND.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_quarter_ROUND.csv")))

#Specialist palliative care- not for release--------------------
#Place of death
proportion_spec_pod_quarter <- df %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(proportion = round(count / total*100,1))


fwrite(proportion_spec_pod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_pod_quarter_RAW.csv")))

#cause of death 
proportion_spec_cod_quarter <- df %>%
  group_by(study_quarter, codgrp) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = n()) %>%
              mutate(codgrp = "All")) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(proportion_spec_cod_quarter, here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_quarter_RAW.csv"))

knitr::kable(read.csv(here::here("output", "os_reports", "WP2_quality_indicators", "specpal_cod_quarter_RAW.csv")))

#Palliative care break down for cancer deaths -----------

cancer_palcare_pod <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(cancer_palcare_pod, here::here("output", "os_reports", "WP2_quality_indicators", "cancer_palcare_ROUND.csv"))

# quarterly ------

cancer_palcare_pod_quarterly <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(palliative_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(cancer_palcare_pod_quarterly, here::here("output", "os_reports", "WP2_quality_indicators", "cancer_palcare_ROUND_quarter.csv"))



# specialist ----

cancer_spec_pod <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(cancer_spec_pod, here::here("output", "os_reports", "WP2_quality_indicators", "cancer_spec_ROUND.csv"))

#quarterly----

cancer_spec_pod_quarterly <- df %>%
  filter(codgrp == "Cancer") %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
  bind_rows(df %>%
              group_by(study_quarter) %>%
              summarise(count = sum(specialist_3m >= 1, na.rm = TRUE), total = sum(codgrp == "Cancer")) %>%
              mutate(pod_ons_new = "All")) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5)) %>%
  mutate(proportion = round(count / total*100,1))

fwrite(cancer_spec_pod_quarterly, here::here("output", "os_reports", "WP2_quality_indicators", "cancer_spec_ROUND_quarter.csv"))

