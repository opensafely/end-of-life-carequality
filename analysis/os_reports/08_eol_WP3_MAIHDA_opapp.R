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
                              , TRUE ~ "All other causes")) %>%
  filter(study_month >= startdate & study_month <= enddate & imd_quintile >=1 & age_band != "0-24" & codgrp == "Cancer" & pod_ons_new == "Home")

#produce means and SD for each group ----------------
cols_of_interest <- c("count", "total")

GLM_sex <- df %>%
  group_by(sex) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_sex, here::here("output", "os_reports", "WP3", "OP_GLM_sex.csv"))


GLM_Ethnicity_2 <- df %>%
  group_by(Ethnicity_2) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_Ethnicity_2, here::here("output", "os_reports", "WP3", "OP_GLM_ethnicity.csv"))


GLM_imd_quintile <- df %>%
  group_by(imd_quintile) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_imd_quintile, here::here("output", "os_reports", "WP3", "OP_GLM_IMD.csv"))

GLM_age_band <- df %>%
  group_by(age_band) %>%
  summarise(count = n(),
            mean = mean(opapp_1m, na.rm = TRUE)
            , sd = sd(opapp_1m, na.rm = TRUE)) %>%
  mutate(total = sum(count)) %>%
  mutate(across(c("mean", "sd"), round, 3)) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(GLM_age_band, here::here("output", "os_reports", "WP3", "OP_GLM_age.csv"))

#new analysis------------
#define strata----
# Form intersectional strata (80 strata in total)

df$imd_quintile_R <- factor(df$imd_quintile)

OP_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile_R) %>% 
  mutate(strata = cur_group_id())


#####Strata datadrame 
cols_of_interest2 <- "total"

df_strata <- OP_MAIHDA %>%
  select(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  group_by(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  summarise(total = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest2), .fns = ~ .x %>% `/`(5) %>% round()*5));


fwrite(df_strata, here::here("output", "os_reports", "WP3", "OPstrata_df.csv"))

# Model 1  - includes a strata random intercept to account for clustering by strata 
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

fm1 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = poisson)
summary(fm1)


OP_model1 <-capture.output(model_parameters(fm1, exponentiate = TRUE, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "OP_model1.txt")

writeLines(OP_model1, con = Output_file)

cat("Output saved to", Output_file, "\n")

#marginal statistics model 1 (null)
# Intercept (mean no. of outpatient attendances when all predictors = 0)
str(summary(fm1))
OPbeta0 <- summary(fm1)$coefficients$cond[1,1]
OPbeta0


# Cluster variance
str(summary(fm1))
OPsigma2u <- summary(fm1)$varcor$cond$strata[1,1]
OPsigma2u


# Marginal expectation (approximately = mean no. of outpatient attendances)
OPexpectation <- exp(OPbeta0 + OPsigma2u/2)
OPexpectation


#Marginal variance
OPvariance <- OPexpectation + OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
OPvariance2 <- OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance2


# Marginal variance: Level-1 component (variance within clusters)
OPvariance1 <- OPexpectation
OPvariance1

# Level-2 VPC (variance partition coefficient)
OPvpc2 <- OPvariance2/(OPvariance2 + OPvariance1)
OPvpc2

# Level-1 VPC (variance partition coefficient - variance at the individual level)
OPvpc1 <- OPvariance1/(OPvariance2 + OPvariance1)
OPvpc1

#Define variance 2 for PCV calculation
var_null <- OPvariance2

#save marginal stats together 
model1_output <- rbind(OPbeta0, OPsigma2u, OPexpectation, OPvariance, OPvariance2, OPvariance1, OPvpc2, OPvpc1)
rownames(model1_output) <-c("OPbeta0", "OPsigma2u", "OPexpectation", "OPvariance", "OPvariance2", "OPvariance1", "OPvpc2", "OPvpc1")

fwrite(model1_output, here::here("output", "os_reports", "WP3", "OPmodel1_output.csv"))


##################################################################################################################################
#Model 3::: adjusted model Poisson ---------------
# fully adjusted model 

fm3 <- glmmTMB(opapp_1m ~ 1 + sex + age_band + Ethnicity_2 + imd_quintile_R + (1|strata), data = OP_MAIHDA, family = poisson)
summary(fm3)


OP_model3 <-capture.output(model_parameters(fm3, exponentiate = TRUE, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "OP_model3.txt")

writeLines(OP_model3, con = Output_file)

cat("Output saved to", Output_file, "\n")

#marginal statistics model 3 (adjusted)
# Linear predictor
OP_MAIHDA$xb <- predict(fm3)
head(OP_MAIHDA)

# Cluster variance
str(summary(fm3))
OPsigma2u3 <- summary(fm3)$varcor$cond$strata[1,1]
OPsigma2u3

# Marginal expectation
OP_MAIHDA$OPexpectation3 <- exp(OP_MAIHDA$xb + OPsigma2u3$x/2)
head(OP_MAIHDA)

# Marginal variance
OP_MAIHDA$OPvariance3 <- OP_MAIHDA$OPexpectation3 + OP_MAIHDA$OPexpectation3^2 * (exp(OPsigma2u3$x) - 1)
head(OP_MAIHDA)

# Marginal variance: Level-2 component
OP_MAIHDA$OPvariance2m3 <- OP_MAIHDA$OPexpectation3^2*(exp(OPsigma2u3$x) - 1)
head(OP_MAIHDA)

# Marginal variance: Level-1 component
OP_MAIHDA$OPvariance1m3 <- OP_MAIHDA$OPexpectation3
head(OP_MAIHDA)

# Level-2 VPC
OP_MAIHDA$OPvpc2m3 <- OP_MAIHDA$OPvariance2m3/(OP_MAIHDA$OPvariance2m3 + OP_MAIHDA$OPvariance1m3)
head(OP_MAIHDA)

# Level-1 VPC
OP_MAIHDA$OPvpc1m3 <- OP_MAIHDA$OPvariance1m3/(OP_MAIHDA$OPvariance2m3 + OP_MAIHDA$OPvariance1m3)
head(OP_MAIHDA)

# Summarise marginal statistics------------------------------
colnames(OP_MAIHDA)
summ <- colMeans(OP_MAIHDA[32:38])
summ
summ_df <- data.frame(summ) 
rownames(summ_df) <- c("xb", "OPexpectation", "OPvariance", "OPvariance2", "OPvariance1", "OPvpc2", "OPvpc1")
summ_df

#define variance 2 for PCV
var_adj <- summ[4]

write.csv(summ_df, file=here::here("output", "os_reports", "WP3", "OPsumVPC.csv"))

#define PCV (measure of intersectionality)
OPpcv <- (var_null - var_adj)/var_null

write.csv(OPpcv, here::here("output", "os_reports", "WP3", "OPpcv.csv"))


#ranking strata -------------------------------
# Predict cluster random intercept effects 
fm3u <- ranef(fm3)
str(fm3u)
head(fm3u$cond$strata)

fm1u <- ranef(fm1)
fm1u

# Model 3 vs. model 1 predicted cluster random effects
fm3vsfm1 <- cbind(fm1u$cond$strata,fm3u$cond$strata)
colnames(fm3vsfm1)
colnames(fm3vsfm1) <- c("fm1u", "fm3u")
colnames(fm3vsfm1)
head(fm3vsfm1)

# Rank the model 1 predicted cluster random effects
fm3vsfm1$fm1urank <- rank(fm3vsfm1$fm1u)
head(fm3vsfm1)

# Rank the model 3 predicted cluster random effects (used for chart)
fm3vsfm1$fm3urank <- rank(fm3vsfm1$fm3u)
head(fm3vsfm1)

write.csv(fm3vsfm1$fm3urank, here::here("output", "os_reports", "WP3", "OPrank3.csv"))

#predictions for each strata
# Calculates mean and se per strata using linear regression with no intercept
# Calculates 95% CIs
OP_MAIHDA$strata <- as.factor(OP_MAIHDA$strata)
strata_predictions <- lm(OPexpectation3 ~ 0 + strata, data = OP_MAIHDA) 


coef(summary(strata_predictions))

OP_strata_predictions <-capture.output(coef(summary(strata_predictions)))

Output_file <- here::here("output", "os_reports", "WP3", "OP_strata_predictions.txt")

writeLines(OP_strata_predictions, con = Output_file)

cat("Output saved to", Output_file, "\n")

# Find CIs for strata
OP_strataCI <- confint(strata_predictions, level = 0.95)

fwrite(OP_strataCI, here::here("output", "os_reports", "WP3", "OPstrataCI.csv"))

#print model
model_parameters(strata_predictions)

