##############################################################
# Multilevel analysis of individual heterogeneity and discriminatory accuracy (MAIHDA) analysis 
# Retired code as OpenSAFELY could not support Library packages required to run this analysis
# Author: Miranda & Sophie 
# Date: 07/12/23 
# Initial aim: To develop code to run MAIHDA analysis to explore the relationship between patient demographic characteristics and no. of GP interactions in the last 90-days of life. 
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. 
# Analysis focuses on patients who die at home, with a cancer diagnosis. 
# Ethnicity is considered as two groups for the purpose of MAIHDA, but more detailed analysis of ethnicity will be conducted separately.
# Analysis over a two calendar year period (2022/2023)
# Code developed referring to https://strengejacke.github.io/ggeffects/articles/practical_intersectionality.html

# Install packages

#install.packages("parameters")
#install.packages("performance")
#install.packages("ggeffects")
#install.packages("Matrix")
#install.packages("TMB", type = "source")

# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(data.table)
library(parameters)# model summaries
library(performance)# model fit indices, ICC
library(ggeffects)   # predictions and significance testing
library(insight)     # extracting random effects variances
library(datawizard)  # data wrangling and preparation
library(glmmTMB)# multilevel modelling

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-01-2022")
enddate <- dmy("31-12-2023")

# Prepare data

df <- read_csv(file = here::here("output", "os_reports", "input_os_reports.csv.gz")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , Ethnicity_2 = case_when(ethnicity_Combined == "White" ~ "White"
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
  filter(study_month >= startdate & study_month <= enddate & imd_quintile >=1 & age_band != "0-24" & codgrp == "Cancer" & pod_ons_new == "Home")

# Counts by grouping variables _RAW not for release

count_by_group_RAW <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  count(sex, age_band, Ethnicity_2, imd_quintile) 

fwrite(count_by_group_RAW, here::here("output", "os_reports", "WP3", "count_by_group_RAW.csv"))

cols_of_interest <- c("n");

count_by_group <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  count(sex, age_band, Ethnicity_2, imd_quintile) %>%
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ replace(.x, (. <= 7 & .  > 0), NA))) %>% 
    dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));
  
fwrite(count_by_group, here::here("output", "os_reports", "WP3", "count_by_group.csv"))


# MAIDHA analysis with GP consultations as the outcome

# Table: Group level mean (GLM) GP Interactions (Counts rounded to the nearest 5)

cols_of_interest <- c("count");

GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "GLM_sex.csv"))

GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "GLM_Ethnicity_2.csv"))

GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "GLM_imd_quintile.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "GLM_age_band.csv"))

# Form intersectional strata (80 strata in total) ethnicity (2), sex (2), IMD (5), age-band(4) - development syntax

#gp_MAIHDA <-df %>%
#group_by(sex, age_band, Ethnicity_2, imd_quintile) %>% 
# dplyr::mutate(strata = cur_group_id())


df$strata <- ifelse(
  is.na(df$sex) | is.na(df$age_band) | is.na(df$Ethnicity_2) | is.na(df$imd_quintile),
  NA_character_,
  paste0(df$sex,",",df$age_band,",",df$Ethnicity_2,",",df$imd_quintile)
)

df$strata <- factor(df$strata)
data_tabulate(df$strata)


# Calculate sample sizes of strata

Count_strata <- df %>%
  group_by(strata) %>%
  summarise(count = n());

# Calculate mean gp interactions by strata

Mean_strata <- df %>%
  group_by(strata) %>%
  summarise(mean = mean(gp_1m, na.rm = TRUE))


# Calculate simple intersectional model

# Linear model with no fixed effects, only intersectional dimensions (sex, ethnicity, age and imd)

model_null <- glmmTMB(gp_1m ~ 1 + (1|strata), data = df, family = poisson)
summary(model1)

model_parameters(model_null)

# Quantify discriminatory accuracy - calculate the ICC
# The higher the ICC, the greater the similarity within strata regarding no. of gp interactions, and greater difference between strata in terms of gp interactions

icc(model_null)

# Partially-adjusted intersectional model and PCV
# To establish which dimensions contribute to inequality (explains the most between-stratum variance)
# PCV = proportional change in the between-stratum variance

# Fit four models each with one dimension as predictor

m_sex <- glmmTMB(gp_1m ~ sex + (1 | strata), data = df)
m_age_band <- glmmTMB(gp_1m ~ age_band + (1 | strata), data = df)
m_Ethnicity_2 <- glmmTMB(gp_1m ~ Ethnicity_2 + (1 | strata), data = df)
m_imd_quintile <- glmmTMB(gp_1m ~ imd_quintile + (1 | strata), data = df)

compare_parameters(m_sex, m_age_band, m_Ethnicity_2, m_imd_quintile)

icc(m_sex)$ICC_adjusted #Singularity
icc(m_age_band)$ICC_adjusted
icc(m_Ethnicity_2)$ICC_adjusted
icc(m_imd_quintile)$ICC_adjusted

# Calculate the proportional change in between-stratum variance (PCV).
# PCV ranges 0 - 1, and the closer to 1, the more this particular dimension explains social inequalities.

# extract random effect variances from all models
v_null <- get_variance(model_null)
v_sex <- get_variance(m_sex) #Singularity
v_age_band <- get_variance(m_age_band)
v_Ethnicity_2 <- get_variance(m_Ethnicity_2)
v_imd_quintile <- get_variance(m_imd_quintile)

# PCV (proportional change in between-stratum variance)

# from null-model to sex-model
(v_null$var.random - v_sex$var.random) / v_null$var.random

# PCV from null-model to age-model
(v_null$var.random - v_age_band$var.random) / v_null$var.random

# PCV from null-model to ethnicity-model
(v_null$var.random - v_Ethnicity_2$var.random) / v_null$var.random

# PCV from null-model to imd-model
(v_null$var.random - v_imd_quintile$var.random) / v_null$var.random

# Predict between-stratum variance and test for significant differences
# How do strata vary? Which combinations of characteristics define higher / lower gp interactions

predictions <- predict_response(
  model_null, 
  c("strata"),
  margin = "empirical"
)

plot(predictions)


# Pairwise comparisons to show which differences between groups are statistically significant.Use Test to look at specific comparisons as too many currently. 

test_predictions(predictions, test = NULL)