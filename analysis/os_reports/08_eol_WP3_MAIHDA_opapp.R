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
library(insight)
library(parameters)
library(performance)
library(glmmTMB)
library(broom)
library(data.table)
library(lubridate)

# Create folder structure

fs::dir_create("output", "os_reports", "WP3")

# Code settings

startdate <- dmy("01-01-2022")
enddate <- dmy("31-12-2023")

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
                              , TRUE ~ "All other causes")) #%>%
  filter(codgrp == "Cancer", pod_ons_new == "Home", age_band != "0-24", imd_quintile >=1)

#produce means and SD for each group ----------------
cols_of_interest <- c("count");
GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "OP_GLM_sex.csv"))


GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "OP_GLM_ethnicity.csv"))


GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "OP_GLM_IMD.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "OP_GLM_age.csv"))

#previous analysis ----------
#define strata-

df$strata <- ifelse(
  is.na(df$sex) | is.na(df$age_band) | is.na(df$Ethnicity_2) | is.na(df$imd_quintile),
  NA_character_,
  paste0(df$sex,",",df$age_band,",",df$Ethnicity_2,",",df$imd_quintile)
)

df$strata <- factor(df$strata)

#Null model

# Model_null <- glmmTMB(opapp_1m ~ 1 +(1 | strata), data = df) 
# model_parameters(Model_null)

# icc(Model_null)

# #fwrite(print(icc(Model_null)), here::here("output", "os_reports", "WP3", "OPnull.csv"))


# # Partially adjusted intersectional model 

# model_sex <- glmmTMB(opapp_1m ~ sex + (1|strata), data = df)
# model_age <- glmmTMB(opapp_1m ~ age_band + (1|strata), data = df)
# model_ethnicity <- glmmTMB(opapp_1m ~ Ethnicity_2 + (1|strata), data = df)
# model_IMD <- glmmTMB(opapp_1m ~ imd_quintile + (1|strata), data = df)

# compare_parameters(model_sex, model_age, model_ethnicity, model_IMD)  

# #fwrite(print(compare_parameters(model_sex, model_age, model_ethnicity, model_IMD)
#              #, here::here("output", "os_reports", "WP3", "OPparameters.csv")))

# icc(model_sex)$ICC_adjusted
# icc(model_age)$ICC_adjusted #singularity 
# icc(model_ethnicity)$ICC_adjusted
# icc(model_IMD)$ICC_adjusted 

# ###############################
# #fwrite(icc(model_sex)$ICC_adjusted, here::here("output", "os_reports", "WP3", "OPsexICC.csv"))


# #PCV -
# # random effects 

# variance_null <- get_variance(Model_null)
# variance_sex <- get_variance(model_sex)
# variance_age <- get_variance(model_age) #singularity again
# variance_ethnicity <- get_variance(model_ethnicity)
# variance_imd <- get_variance(model_IMD)

#new analysis------------
#define strata----
# Form intersectional strata (80 strata in total)

OP_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile) %>% 
  mutate(strata = cur_group_id())

# Model 1  - includes a strata random intercept to account for clustering by strata #
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

fm1 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = poisson)
summary(fm1)

# Intercept (mean no. of outpatient attendances when all predictors = 0)
str(summary(fm1))
OPbeta0 <- summary(fm1)$coefficients$cond[1,1]
OPbeta0

write.csv (OPbeta0, file = 'OPbeta0.csv', row.names = FALSE)
OPbeta0 <- read_csv(file =  "OPbeta0.csv")
fwrite(OPbeta0, here::here("output", "os_reports", "WP3", "OPbeta0.csv"))


# Cluster variance
str(summary(fm1))
OPsigma2u <- summary(fm1)$varcor$cond$strata[1,1]
OPsigma2u

write.csv(OPsigma2u, file = 'OPsigma2u.csv', row.names = FALSE)
OPsigma2u <-read_csv(file = "OPsigma2u.csv")
fwrite(OPsigma2u, here::here("output", "os_reports", "WP3", "OPsigma2u.csv"))

# Marginal expectation (approximately = mean no. of outpatient attendances)
OPexpectation <- exp(OPbeta0 + OPsigma2u/2)
OPexpectation

write.csv(OPexpectation, file = 'OPexpectation.csv', row.names = FALSE)
OPexpectation <-read_csv(file = "OPexpectation.csv")
fwrite(OPexpectation, here::here("output", "os_reports", "WP3", "OPexpectation.csv"))

# Marginal variance
OPvariance <- OPexpectation + OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance

write.csv(OPvariance, file = 'OPvariance.csv', row.names = FALSE)
OPvariance <-read_csv(file = "OPvariance.csv")
fwrite(OPvariance, here::here("output", "os_reports", "WP3", "OPvariance.csv"))

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
OPvariance2 <- OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance2

write.csv(OPvariance2, file = 'OPvariance2.csv', row.names = FALSE)
OPvariance2 <-read_csv(file = "OPvariance2.csv")
fwrite(OPvariance2, here::here("output", "os_reports", "WP3", "OPvariance2.csv"))

# Marginal variance: Level-1 component (variance within clusters)
OPvariance1 <- OPexpectation
OPvariance1

write.csv(OPvariance1, file = 'OPvariance1.csv', row.names = FALSE)
Ovariance1 <-read_csv(file = "OPvariance1.csv")
fwrite(OPvariance1, here::here("output", "os_reports", "WP3", "OPvariance1.csv"))

# Level-2 VPC (variance partition coefficient)
# VPC = proportion of the total variance located at the strata level - the global measure of intersectionality. 
# VPC = same as ICC
# A high VPC means the strata are useful for understanding differences in outpatient attendances. 
# Where VPC = 0, strata = random sample from the population not relevant to understanding outpatient attendances at end of life. 
vpc2 <- OPvariance2/(OPvariance2 + OPvariance1)
vpc2

write.csv(vpc2, file = 'vpc2.csv')
vpc2.csv <-read_csv(file = "vpc2.csv")
fwrite(vpc2, here::here("output", "os_reports", "WP3", "OPvpc2.csv"))

# Level-1 VPC (variance partition coefficient - variance at the individual level)
vpc1 <- OPvariance1/(OPvariance2 + OPvariance1)
vpc1

write.csv(vpc1, file = 'vpc1.csv')
vpc1.csv <-read_csv(file = "vpc1.csv")
fwrite(vpc1, here::here("output", "os_reports", "WP3", "OPvpc1.csv"))

##################################################################################################################################

# Model 2: Two-level variance-components negative binomial model
# Regression coefficients, not just the intercept, are now allowed to vary across clusters
# Negative binomial models account for the variability caused by overdispersion

# Fit model
fm2 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = nbinom2)
summary(fm2)

# Intercept
str(summary(fm2))
OPbeta0 <- summary(fm2)$coefficients$cond[1,1]
OPbeta0

# Cluster variance
str(summary(fm2))
OPsigma2u <- summary(fm2)$varcor$cond$strata[1,1]
OPsigma2u

# Overdispersion parameter
str(summary(fm2))
alpha <- 1/(summary(fm2)$sigma)
alpha

# Marginal expectation
OPexpectation <- exp(OPbeta0 + OPsigma2u/2)
OPexpectation

# Marginal variance
variance <- OPexpectation + OPexpectation^2*(exp(OPsigma2u)*(1 + alpha) - 1)
variance

# Marginal variance: Level-2 component
OPvariance2 <- OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance2

# Marginal variance: Level-1 component
OPvariance1 <- OPexpectation + OPexpectation^2*exp(OPsigma2u)*alpha
OPvariance1

# Level-2 VPC
vpc2 <- OPvariance2/(OPvariance2 + OPvariance1)
vpc2

# Level-1 VPC
vpc1 <- OPvariance1/(OPvariance2 + OPvariance1)
vpc1

# Predict cluster random intercept effects
fm2u <- ranef(fm2)
fm2u

############################################################################

# Five-level variance-components negative binomial model
# Variance-component models quantify the proportion of variation in the response due to systematic differences between clusters.

# Fit model
fm3 <- glmmTMB(opapp_1m ~ 1 + (1|sex) + (1|age_band) + (1|Ethnicity_2) + (1|imd_quintile), 
               data = OP_MAIHDA, family = nbinom2)
summary(fm3)

# Intercept
str(summary(fm3))
OPbeta0 <- summary(fm3)$coefficients$cond[1,1]
OPbeta0

# Cluster variance - sex
str(summary(fm3))
OPsigma2u <- summary(fm3)$varcor$cond$sex[1,1]
OPsigma2u

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
OPexpectation <- exp(OPbeta0 + OPsigma2u/2 + sigma2v/2 + sigma2w/2 + sigma2x/2)
OPexpectation

# Marginal variance
variance <- OPexpectation + 
  OPexpectation^2*(exp(OPsigma2u + sigma2v + sigma2w + sigma2x)*(1 + alpha) - 1)
variance

# Marginal variance: Level-5 component
variance5 <- (OPexpectation^2*(exp(OPsigma2u) - 1))
variance5

# Marginal variance: Level-4 component
variance4 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v) - 1)
variance4

# Marginal variance: Level-3 component
variance3 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v)*(exp(sigma2w) - 1))
variance3

# Marginal variance: Level-2 component
OPvariance2 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v)*(exp(sigma2w)*(exp(sigma2x) - 1)))
OPvariance2

# Marginal variance: Level-1 component
OPvariance1 <- OPexpectation + OPexpectation^2*exp(OPsigma2u + sigma2v + sigma2w + sigma2x)*alpha
OPvariance1

# Should the different level VPC be interpreted as the proportion of variance in outcome explained by each characteristic? 

# Level-5 VPC
vpc5 <- variance5/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
vpc5

# Level-4 VPC
vpc4 <- variance4/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
vpc4

# Level-3 VPC
vpc3 <- variance3/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
vpc3

# Level-2 VPC
vpc2 <- OPvariance2/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
vpc2

# Level-1 VPC
vpc1 <- OPvariance1/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
vpc1
