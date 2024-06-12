##########################################################
# Logistic regression to explore factors influencing A&E attendances
# Author: Miranda & Sophie 
# Date: 08/05/24 
# Initial aim: To develop code to run logistic regression to explore the relationship between patient demographic characteristics and A&E attendances in the last 90-days of life. & GP interactions in the last 30-days of life 
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. 
# Analysis focuses on patients who die at home, with a cancer diagnosis. 
# Ethnicity is considered as two groups for the purpose of MAIHDA, but more detailed analysis of ethnicity will be conducted separately.
# Analysis over a two calendar year period (2022/2023)

# Load packages

library(tidyverse)
library(lubridate)
library(haven)#enables R to read and write various data formats used by other statistical packages, such as SAS so might not need this
library(ggplot2)
library(glmmTMB)# multilevel modelling
library(data.table)
library(parameters)  # model summaries
library(performance) # model fit indices, ICC
library(insight) # variance

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-01-2022")
enddate <- dmy("31-12-2023")

# Prepare data

df <- read_csv(file = here::here("output", "os_reports", "input_os_reports.csv.gz")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , age_R = case_when(age_band == "25-69" ~ 1,
                    age_band == "70-79" ~ 2,
                    age_band == "80-89" ~ 3,
                    age_band == "90+" ~ 4)
         , Ethnicity_2 = case_when(ethnicity_Combined == "White" ~ "White"
                                  , ethnicity_Combined == "Asian or Asian British" | ethnicity_Combined == "Black or Black British" | ethnicity_Combined == "Mixed" | ethnicity_Combined == "Chinese or Other Ethnic Groups" | ethnicity_Combined == "Not stated" ~ "All other ethnic groups")
         , Ethnicity_R = case_when(Ethnicity_2 == "White" ~ 0, Ethnicity_2 == "All other ethnic groups" ~ 1)
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


#produce means and SD for each group (A&E attendances over 3-months = outcome--------------

cols_of_interest <- c("count", "total");
GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(aevis_3m, na.rm = TRUE)
            , sd = sd(aevis_3m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "AE_GLM_sex.csv"))


GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(aevis_3m, na.rm = TRUE)
            , sd = sd(aevis_3m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "AE_GLM_ethnicity.csv"))


GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(aevis_3m, na.rm = TRUE)
            , sd = sd(aevis_3m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "AE_GLM_IMD.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(aevis_3m, na.rm = TRUE)
            , sd = sd(aevis_3m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "AE_GLM_age.csv"))

#produce means and SD for each group (GP interactions over 1-month  = outcome----------------

cols_of_interest <- c("count", "total");
GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "GP_GLM_sex.csv"))


GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "GP_GLM_ethnicity.csv"))


GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "GP_GLM_IMD.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "GP_GLM_age.csv"))

# Outcome variable - A&E attendances

# Create a binary variable for A&E attendances over 3-months 

df$AE_R <- as.numeric(df$aevis_3m >= 1)

# Change IMD/age to be considered categorical. Reorder age so that 4 (90+) is the comparison. Reorder to that male is the comparison. 

df$imd_quintile_R <- factor(df$imd_quintile)

df$age_R <- factor(df$age_R, levels = c('4', '1', '2', '3'))

AE_MAIHDA <-df %>%
  group_by(sex, age_R, Ethnicity_2, imd_quintile_R) %>% 
  dplyr::mutate(strata = cur_group_id(), na.rm = TRUE)


# Binomial model with binary outcome variable for A&E attendances (null model)

m_null <- glmmTMB(AE_R ~ 1 + (1|strata), data = AE_MAIHDA, family = binomial)
model_parameters(m_null, exponentiate=TRUE)
icc(m_null)

# Saving output from the null model (model summary)

Null_summary <-capture.output(summary(m_null, exponentiate=TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "AE_null_summary.csv")

writeLines(Null_summary, con = Output_file)

cat("Output saved to", Output_file, "\n")


# Saving output from the null model (model parameters)

#Null_output <-capture.output(model_parameters(m_null, exponentiate=TRUE))

#Output_file <- here::here("output", "os_reports", "WP3", "AE_null_summary.txt")

#writeLines(Null_output, con = Output_file)

#cat("Output saved to", Output_file, "\n")


# Adjusted model

m_adj <- glmmTMB(AE_R ~ 1 + sex + age_R + Ethnicity_2 + imd_quintile_R + (1|strata), data = AE_MAIHDA, family = binomial)
model_parameters(m_adj,exponentiate=TRUE)
icc(m_adj)


Adj_output <-capture.output(summary(m_adj, exponentiate=TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "AE_adj_summary.txt")

writeLines(Adj_output, con = Output_file)

cat("Output saved to", Output_file, "\n")



# Saving output from the adjusted model (Model parameters)

#Adj_output <-capture.output(model_parameters(m_adj, exponentiate=TRUE))

#Output_file <- here::here("output", "os_reports", "WP3", "AE_adj_model.txt")

#writeLines(Adj_output, con = Output_file)

#cat("Output saved to", Output_file, "\n")


# Now calculate the PCV
#v_null <- get_variance(m_null)
#v_adj <- get_variance(m_adj)
#pcv <- (v_null$var.random - v_adj$var.random) / v_null$var.random
#pcv

# Get the random effects
#ref<-ranef(m_adj)
#print(ref)
#rr<-as.data.frame(ref) # Convert to obtain SD
#rr$lcl <- rr$condval - 1.96*rr$condsd
#rr$ucl <- rr$condval + 1.96*rr$condsd
#rr$rnk <- rank(rr$condval)
#rr <- rr[order(rr$rnk),] # Sort for plot

# Very simple plot
#ggplot(rr, aes(rnk, condval)) +
#  geom_errorbar(aes(ymin = lcl, ymax = ucl,width = 0.1)) +
#  geom_point(size = 2)+
#  labs(x="Stratum rank", y="Condional log odds")

# Outcome variable - GP interactions

# Create a binary variable for GP interactions over 3-months 

df$GP_R <- as.numeric(df$gp_1m >= 1)

df$imd_quintile_R <- factor(df$imd_quintile)

df$age_R <- factor(df$age_R, levels = c('4', '1', '2', '3'))

GP_MAIHDA <-df %>%
  group_by(sex, age_R, Ethnicity_2, imd_quintile_R) %>% 
  dplyr::mutate(strata = cur_group_id(), na.rm = TRUE)


# Binomial model with binary outcome variable for A&E attendances (null model)

m_null <- glmmTMB(GP_R ~ 1 + (1|strata), data = GP_MAIHDA, family = binomial)
model_parameters(m_null, exponentiate=TRUE)
icc(m_null)

# Saving output from the null model (model parameters)

Null_output <-capture.output(model_parameters(m_null, exponentiate=TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "GP_null_model.txt")

writeLines(Null_output, con = Output_file)

cat("Output saved to", Output_file, "\n")


# Adjusted model

m_adjGP <- glmmTMB(GP_R ~ 1 + sex + age_R + Ethnicity_2 + imd_quintile_R + (1|strata), data = GP_MAIHDA, family = binomial)
model_parameters(m_adj,exponentiate=TRUE)
icc(m_adjGP)

# Saving output from the adjusted model (model parameters)

Adj_output <-capture.output(model_parameters(m_adjGP, exponentiate=TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "GP_adj_model.txt")

writeLines(Adj_output, con = Output_file)

cat("Output saved to", Output_file, "\n")




















