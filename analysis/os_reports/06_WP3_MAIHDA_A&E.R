##########################################################
# Logistic regression to explore factors influencing A&E attendances
# Author: Miranda & Sophie 
# Date: 08/05/24 
# Initial aim: To develop code to run logistic regression to explore the relationship between patient demographic characteristics and A&E attendances in the last 90-days of life. 
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. 
# Analysis focuses on patients who die at home, with a cancer diagnosis. 
# Ethnicity is considered as two groups for the purpose of MAIHDA, but more detailed analysis of ethnicity will be conducted separately.
# Analysis over a two calendar year period (2022/2023)

# Install packages

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
         , Sex_R = case_when(sex == "male" ~ 0, sex == "female" ~ 1)
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

# Outcome variable - A&E attendances

# Create a binary variable for A&E attendances over 3-months 

df$AE_R <- as.numeric(df$aevis_3m >= 1)

# Change IMD/age to be considered categorical. Reorder age so that 4 (90+) is the comparison


df$imd_quintile <- factor(df$imd_quintile)
df$age_R <- factor(df$age_R)

df$age_R <- relevel(df$age_R,
ref = '4')

AE_MAIHDA <-df %>%
  group_by(Sex_R, age_R, Ethnicity_R, imd_quintile) %>% 
  dplyr::mutate(strata = cur_group_id(), na.rm = TRUE)


# Binomial model with binary outcome variable for A&E attendances (null model)

m_null <- glmmTMB(AE_R ~ 1 + (1|strata), data = AE_MAIHDA, family = binomial)
model_parameters(m_null, exponentiate=TRUE)
icc(m_null)

# Adjusted model

m_adj <- glmmTMB(AE_R ~ 1 + Sex_R + age_R + Ethnicity_R + imd_quintile + (1|strata), data = AE_MAIHDA, family = binomial)
model_parameters(m_adj,exponentiate=TRUE)
icc(m_adj)

# Now calculate the PCV
v_null <- get_variance(m_null)
v_adj <- get_variance(m_adj)
pcv <- (v_null$var.random - v_adj$var.random) / v_null$var.random
pcv

# Get the random effects
ref<-ranef(m_adj)
print(ref)
rr<-as.data.frame(ref) # Convert to obtain SD
rr$lcl <- rr$condval - 1.96*rr$condsd
rr$ucl <- rr$condval + 1.96*rr$condsd
rr$rnk <- rank(rr$condval)
rr <- rr[order(rr$rnk),] # Sort for plot

# Very simple plot
ggplot(rr, aes(rnk, condval)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl,width = 0.1)) +
  geom_point(size = 2)+
  labs(x="Stratum rank", y="Condional log odds")


















