##########################################################
# Multilevel analysis of individual heterogeneity and discriminatory accuracy (MAIHDA) analysis 
# Second attempt at MAIHDA code development trying to use library packages supported by MAIHDA
# Author: Miranda & Sophie 
# Date: 08/04/24 
# Initial aim: To develop code to run MAIHDA analysis to explore the relationship between patient demographic characteristics and no. of GP interactions/A&E attendances in the last 30-days of life. 
##############################################################

# Note: Patients with no IMD are excluded from the analysis as are patients aged 0-24. 
# Analysis focuses on patients who die at home, with a cancer diagnosis. 
# Ethnicity is considered as two groups for the purpose of MAIHDA, but more detailed analysis of ethnicity will be conducted separately.
# Analysis over a two calendar year period (2022/2023)
# Code developed referring to Leckie et al. (2020) Partitioning Variation in Multilevel Models for Count Data

# Install packages

#install.packages("TMB", type = "source")

# Load packages

library(tidyverse)
library(lubridate)
library(haven)#enables R to read and write various data formats used by other statistical packages, such as SAS so might not need this
library(ggplot2)
library(glmmTMB)# multilevel modelling
library(dplyr)
library(data.table)
library(broom)

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

# MAIDHA analysis with GP interactions as the outcome

cols_of_interest <- c("count");

GLM_GP_OVERALL <- df %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_GP_OVERALL, here::here("output", "os_reports", "WP3", "GP_OVERALL.csv"))


# Table: Group level mean (GLM) GP interactions (Counts rounded to the nearest 5)

cols_of_interest <- c("count");

GLM_GP_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_GP_sex, here::here("output", "os_reports", "WP3", "GP_sex.csv"))

GLM_GP_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_GP_Ethnicity_2, here::here("output", "os_reports", "WP3", "GP_Ethnicity_2.csv"))

GLM_GP_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_GP_imd_quintile, here::here("output", "os_reports", "WP3", "GP_imd_quintile.csv"))

GLM_GP_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(gp_1m, na.rm = TRUE)
            , sd = sd(gp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_GP_age_band, here::here("output", "os_reports", "WP3", "GP_age_band.csv"))

#########################################################################################################################################
# Outcome variable = GP interactions
# Form intersectional strata (80 strata in total)

GP_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile) %>% 
  dplyr::mutate(strata = cur_group_id(), na.rm = TRUE)

# Model 1  - includes a strata random intercept to account for clustering by strata #
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

GPfm1 <- glmmTMB(gp_1m ~ 1 + (1|strata), data = GP_MAIHDA, family = poisson)
summary(GPfm1)

# Intercept (mean no. of A&E attendances when all predictors = 0)
str(summary(GPfm1))
GPbeta0 <- summary(GPfm1)$coefficients$cond[1,1]
GPbeta0

write.csv (GPbeta0, file = 'GPbeta0.csv', row.names = FALSE)
GPbeta0 <- read_csv(file =  "GPbeta0.csv")
fwrite(GPbeta0, here::here("output", "os_reports", "WP3", "GPbeta0.csv"))

# Cluster variance
str(summary(GPfm1))
GPsigma2u <- summary(GPfm1)$varcor$cond$strata[1,1]
GPsigma2u

write.csv(GPsigma2u, file = 'GPsigma2u.csv', row.names = FALSE)
GPsigma2u <-read_csv(file = "GPsigma2u.csv")
fwrite(GPsigma2u, here::here("output", "os_reports", "WP3", "GPsigma2u.csv"))

# Marginal expectation (approximately = mean no. of A&E attendances)
GPexpectation <- exp(GPbeta0 + GPsigma2u/2)
GPexpectation

write.csv(GPexpectation, file = 'GPexpectation.csv', row.names = FALSE)
GPexpectation <-read_csv(file = "GPexpectation.csv")
fwrite(GPexpectation, here::here("output", "os_reports", "WP3", "GPexpectation.csv"))

# Marginal variance
GPvariance <- GPexpectation + GPexpectation^2*(exp(GPsigma2u) - 1)
GPvariance

write.csv(GPvariance, file = 'GPvariance.csv', row.names = FALSE)
GPvariance <-read_csv(file = "GPvariance.csv")
fwrite(GPvariance, here::here("output", "os_reports", "WP3", "GPvariance.csv"))

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
GPvariance2 <- GPexpectation^2*(exp(GPsigma2u) - 1)
GPvariance2

write.csv(GPvariance2, file = 'GPvariance2.csv', row.names = FALSE)
GPvariance2 <-read_csv(file = "GPvariance2.csv")
fwrite(GPvariance2, here::here("output", "os_reports", "WP3", "GPvariance2.csv"))

# Marginal variance: Level-1 component (variance within clusters)
GPvariance1 <- GPexpectation
GPvariance1

write.csv(GPvariance1, file = 'GPvariance1.csv', row.names = FALSE)
GPvariance1 <-read_csv(file = "GPvariance1.csv")
fwrite(GPvariance1, here::here("output", "os_reports", "WP3", "GPvariance1.csv"))

# Level-2 VPC (variance partition coefficient)
# VPC = proportion of the total variance located at the strata level - the global measure of intersectionality. 
# VPC = same as ICC
# A high VPC means the strata are useful for understanding differences in A&E attendances. 
# Where VPC = 0, strata = random sample from the population not relevant to understanding A&E attendances at end of life. 
GPvpc2 <- GPvariance2/(GPvariance2 + GPvariance1)
GPvpc2

write.csv(GPvpc2, 'GPvpc2.csv', row.names = FALSE)
GPvpc2 <-read_csv(file = "GPvpc2.csv")
fwrite(GPvpc2, here::here("output", "os_reports", "WP3", "GPvpc2.csv"))

# Level-1 VPC (variance partition coefficient - variance at the individual level)
GPvpc1 <- GPvariance1/(GPvariance2 + GPvariance1)
GPvpc1

write.csv(GPvpc1, 'GPvpc1.csv', row.names = FALSE)
GPvpc1 <-read_csv(file = "GPvpc1.csv")
fwrite(GPvpc1, here::here("output", "os_reports", "WP3", "GPvpc1.csv"))

#########################################################################################################################################

# MAIDHA analysis with A&E attendances as the outcome

# Mean A&E attendances for patients with cancer who die at home- Raw not for release

GLM_Aevis_RAW <- df %>%
  filter(codgrp == "Cancer"
         & pod_ons_new == "Home") %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) 

fwrite(GLM_Aevis_RAW, here::here("output", "os_reports", "WP3", "GLM_Aevis_RAW.csv"))


# Table: Group level mean (GLM) A&E attendances (Counts rounded to the nearest 5)

cols_of_interest <- c("count");

GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "GLM_sex.csv"))

GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "GLM_Ethnicity_2.csv"))

GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "GLM_imd_quintile.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(aevis_1m, na.rm = TRUE)
            , sd = sd(aevis_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "GLM_age_band.csv"))















# Outcome variable = A&E attendances

# Form intersectional strata (80 strata in total)

AE_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile) %>% 
  dplyr::mutate(strata = cur_group_id(), na.rm = TRUE)

# Model 1  - includes a strata random intercept to account for clustering by strata #
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

fm1 <- glmmTMB(aevis_1m ~ 1 + (1|strata), data = AE_MAIHDA, family = poisson)
summary(fm1)

# Intercept (mean no. of A&E attendances when all predictors = 0)
str(summary(fm1))
beta0 <- summary(fm1)$coefficients$cond[1,1]
beta0

write.csv (beta0, file = 'beta0.csv', row.names = FALSE)
beta0 <- read_csv(file =  "beta0.csv")
fwrite(beta0, here::here("output", "os_reports", "WP3", "beta0.csv"))

# Cluster variance
str(summary(fm1))
sigma2u <- summary(fm1)$varcor$cond$strata[1,1]
sigma2u

write.csv(sigma2u, file = 'sigma2u.csv', row.names = FALSE)
sigma2u <-read_csv(file = "sigma2u.csv")
fwrite(sigma2u, here::here("output", "os_reports", "WP3", "sigma2u.csv"))

# Marginal expectation (approximately = mean no. of A&E attendances)
expectation <- exp(beta0 + sigma2u/2)
expectation

write.csv(expectation, file = 'expectation.csv', row.names = FALSE)
expectation <-read_csv(file = "expectation.csv")
fwrite(expectation, here::here("output", "os_reports", "WP3", "expectation.csv"))

# Marginal variance
variance <- expectation + expectation^2*(exp(sigma2u) - 1)
variance

write.csv(variance, file = 'variance.csv', row.names = FALSE)
variance <-read_csv(file = "variance.csv")
fwrite(variance, here::here("output", "os_reports", "WP3", "variance.csv"))

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
variance2 <- expectation^2*(exp(sigma2u) - 1)
variance2

write.csv(variance2, file = 'variance2.csv', row.names = FALSE)
variance2 <-read_csv(file = "variance2.csv")
fwrite(variance2, here::here("output", "os_reports", "WP3", "variance2.csv"))

# Marginal variance: Level-1 component (variance within clusters)
variance1 <- expectation
variance1

write.csv(variance1, file = 'variance1.csv', row.names = FALSE)
variance1 <-read_csv(file = "variance1.csv")
fwrite(variance1, here::here("output", "os_reports", "WP3", "variance1.csv"))

# Level-2 VPC (variance partition coefficient)
# VPC = proportion of the total variance located at the strata level - the global measure of intersectionality. 
# VPC = same as ICC
# A high VPC means the strata are useful for understanding differences in A&E attendances. 
# Where VPC = 0, strata = random sample from the population not relevant to understanding A&E attendances at end of life. 
vpc2 <- variance2/(variance2 + variance1)
vpc2

write.csv(vpc2, 'vpc2.csv', row.names = FALSE)
vpc2 <-read_csv(file = "vpc2.csv")
fwrite(vpc2, here::here("output", "os_reports", "WP3", "vpc2.csv"))

# Level-1 VPC (variance partition coefficient - variance at the individual level)
vpc1 <- variance1/(variance2 + variance1)
vpc1

write.csv(vpc1, 'vpc1.csv', row.names = FALSE)
vpc1 <-read_csv(file = "vpc1.csv")
fwrite(vpc1, here::here("output", "os_reports", "WP3", "vpc1.csv"))

##################################################################################################################################

# Model 2: Two-level variance-components negative binomial model
# Regression coefficients, not just the intercept, are now allowed to vary across clusters
# Negative binomial models account for the variability caused by overdispersion

# Fit model
fm2 <- glmmTMB(aevis_1m ~ 1 + (1|strata), data = AE_MAIHDA, family = nbinom2)
summary(fm2)

# Intercept
str(summary(fm2))
beta0 <- summary(fm2)$coefficients$cond[1,1]
beta0

# Cluster variance
str(summary(fm2))
sigma2u <- summary(fm2)$varcor$cond$strata[1,1]
sigma2u

# Overdispersion parameter
str(summary(fm2))
alpha <- 1/(summary(fm2)$sigma)
alpha

# Marginal expectation
expectation <- exp(beta0 + sigma2u/2)
expectation

# Marginal variance
variance <- expectation + expectation^2*(exp(sigma2u)*(1 + alpha) - 1)
variance

# Marginal variance: Level-2 component
variance2 <- expectation^2*(exp(sigma2u) - 1)
variance2

# Marginal variance: Level-1 component
variance1 <- expectation + expectation^2*exp(sigma2u)*alpha
variance1

# Level-2 VPC
vpc2 <- variance2/(variance2 + variance1)
vpc2

# Level-1 VPC
vpc1 <- variance1/(variance2 + variance1)
vpc1

# Predict cluster random intercept effects
fm2u <- ranef(fm2)
fm2u

############################################################################

# Five-level variance-components negative binomial model
# Variance-component models quantify the proportion of variation in the response due to systematic differences between clusters.

# Fit model
fm3 <- glmmTMB(aevis_1m ~ 1 + (1|sex) + (1|age_band) + (1|Ethnicity_2) + (1|imd_quintile), 
               data = AE_MAIHDA, family = nbinom2)
summary(fm3)

# Intercept
str(summary(fm3))
beta0 <- summary(fm3)$coefficients$cond[1,1]
beta0

# Cluster variance - sex
str(summary(fm3))
sigma2u <- summary(fm3)$varcor$cond$sex[1,1]
sigma2u

# Cluster variance - imd_quintile
str(summary(fm3))
sigma2v <- summary(fm3)$varcor$cond$imd_quintile[1,1]
sigma2v

str(summary(fm3))
sigma2w <- summary(fm3)$varcor$cond$Ethnicity_2[1,1]
sigma2w

str(summary(fm3))
sigma2x <- summary(fm3)$varcor$cond$age_band[1,1]
sigma2x

# Overdispersion parameter
str(summary(fm3))
alpha <- 1/(summary(fm3)$sigma)
alpha

# Marginal expectation
expectation <- exp(beta0 + sigma2u/2 + sigma2v/2 + sigma2w/2 + sigma2x/2)
expectation

# Marginal variance
variance <- expectation + 
  expectation^2*(exp(sigma2u + sigma2v + sigma2w + sigma2x)*(1 + alpha) - 1)
variance

# Marginal variance: Level-5 component
variance5 <- (expectation^2*(exp(sigma2u) - 1))
variance5
             
# Marginal variance: Level-4 component
variance4 <- expectation^2*exp(sigma2u)*(exp(sigma2v) - 1)
variance4
             
# Marginal variance: Level-3 component
variance3 <- expectation^2*exp(sigma2u)*(exp(sigma2v)*(exp(sigma2w) - 1))
variance3

# Marginal variance: Level-2 component
variance2 <- expectation^2*exp(sigma2u)*(exp(sigma2v)*(exp(sigma2w)*(exp(sigma2x) - 1)))
variance2

# Marginal variance: Level-1 component
variance1 <- expectation + expectation^2*exp(sigma2u + sigma2v + sigma2w + sigma2x)*alpha
variance1

# Should the different level VPC be interpreted as the proportion of variance in outcome explained by each characteristic? 

# Level-5 VPC
vpc5 <- variance5/(variance5 + variance4 + variance3 + variance2 + variance1)
vpc5

# Level-4 VPC
vpc4 <- variance4/(variance5 + variance4 + variance3 + variance2 + variance1)
vpc4

# Level-3 VPC
vpc3 <- variance3/(variance5 + variance4 + variance3 + variance2 + variance1)
vpc3
             
# Level-2 VPC
vpc2 <- variance2/(variance5 + variance4 + variance3 + variance2 + variance1)
vpc2
             
# Level-1 VPC
vpc1 <- variance1/(variance5 + variance4 + variance3 + variance2 + variance1)
vpc1

