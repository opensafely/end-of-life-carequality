#######################################
# MAIHDA analysis 
# Authors: Sophie and Miranda 
# Date: 18/03/24
# Aim: run MAIHDA analysis for outcome outpatient appointments
######################################

# Set up------------------
# Load packages

library(tidyverse)
library(dplyr)

# MAIHDA packages

library(ggeffects)
library(insight)
library(datawizard)
library(parameters)
library(performance)
library(glmmTMB)

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-09-2022")
enddate <- dmy("31-08-2023")

# Prepare data------------

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
  filter(codgrp == "Cancer", pod_ons_new == "Home", age_band != "0-24", imd_quintile >=1)

#produce means and SD for each group ----------------
cols_of_interest <- c("count");
GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "WP3", "GLM_sex_opapp.csv"))


GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "WP3", "GLM_ethnicity_opapp.csv"))


GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "WP3", "GLM_IMD_opapp.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "WP3", "GLM_age_opapp.csv"))

#define strata--------------------

df$strata <- ifelse(
  is.na(df$sex) | is.na(df$age_band) | is.na(df$Ethnicity_2) | is.na(df$imd_quintile),
  NA_character_,
  paste0(df$sex,",",df$age_band,",",df$Ethnicity_2,",",df$imd_quintile)
)

df$strata <- factor(df$strata)
data_tabulate(df$strata)

# Null model ---------------------
#troubleshooting code
install.packages("Matrix")
install.packages("TMB", type = "source")
library(glmmTMB)


Model_null <- glmmTMB(opapp_1m ~ 1 +(1 | strata), data = df) 
model_parameters(Model_null)

icc(Model_null)

# Partially adjusted intersectional model ----------------

model_sex <- glmmTMB(opapp_1m ~ sex + (1|strata), data = df)
model_age <- glmmTMB(opapp_1m ~ age_band + (1|strata), data = df)
model_ethnicity <- glmmTMB(opapp_1m ~ Ethnicity_2 + (1|strata), data = df)
model_IMD <- glmmTMB(opapp_1m ~ imd_quintile + (1|strata), data = df)

compare_parameters(model_sex, model_age, model_ethnicity, model_IMD)  

icc(model_sex)$ICC_adjusted
icc(model_age)$ICC_adjusted #singularity 
icc(model_ethnicity)$ICC_adjusted
icc(model_IMD)$ICC_adjusted 

#PCV -----------------
# random effects 

variance_null <- get_variance(Model_null)
variance_sex <- get_variance(model_sex)
variance_age <- get_variance(model_age) #singularity again
variance_ethnicity <- get_variance(model_ethnicity)
variance_imd <- get_variance(model_IMD)

#PCVs 

(variance_null$var.random - variance_sex$var.random) / variance_null$var.random
(variance_null$var.random - variance_age$var.random) / variance_null$var.random
(variance_null$var.random - variance_ethnicity$var.random) / variance_null$var.random
(variance_null$var.random - variance_imd$var.random) / variance_null$var.random

#predict between strata differences----------------

predictions <- predict_response(
  Model_null, "strata",
  margin = "empirical"
) 

fwrite(predictions, here::here("output", "os_reports", "WP3", "predictions.csv"))

predictions_plot <- plot(predictions) +
  theme(axis.text.x = element_text(angle = 90))

#test predictions
test_predictions(predictions, test = NULL)