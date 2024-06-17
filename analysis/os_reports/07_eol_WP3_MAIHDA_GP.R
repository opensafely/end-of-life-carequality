##########################################################
# Logistic regression to explore factors influencing GP attendances
# Author: Miranda & Sophie 
# Date: 12/06/24 
# Initial aim: To develop code to run poisson regression to explore the relationship between patient demographic characteristics and GP interactions in the last 30-days of life 
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


#define strata----

# Form intersectional strata (80 strata in total)

df$imd_quintile_R <- factor(df$imd_quintile)

GP_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile_R) %>% 
  mutate(strata = cur_group_id())


#####Strata
cols_of_interest2 <- "total"

df_strata <- GP_MAIHDA %>%
  select(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  group_by(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  summarise(total = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest2), .fns = ~ .x %>% `/`(5) %>% round()*5));

fwrite(df_strata, here::here("output", "os_reports", "WP3", "GPstrata_df.csv"))

# Model 1  - includes a strata random intercept to account for clustering by strata #
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

fm1 <- glmmTMB(gp_1m ~ 1 + (1|strata), data = GP_MAIHDA, family = poisson)
summary(fm1)


GP_model1 <-capture.output(model_parameters(fm1, exponentiate = TRUE, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "GP_model1.txt")

writeLines(GP_model1, con = Output_file)

cat("Output saved to", Output_file, "\n")

# Intercept (mean no. of outpatient attendances when all predictors = 0)
str(summary(fm1))
GPbeta0 <- summary(fm1)$coefficients$cond[1,1]
GPbeta0

# Cluster variance
str(summary(fm1))
GPsigma2u <- summary(fm1)$varcor$cond$strata[1,1]
GPsigma2u

# Marginal expectation (approximately = mean no. of outpatient attendances)
GPexpectation <- exp(GPbeta0 + GPsigma2u/2)
GPexpectation

#Marginal variance
GPvariance <- GPexpectation + GPexpectation^2*(exp(GPsigma2u) - 1)
GPvariance

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
GPvariance2 <- GPexpectation^2*(exp(GPsigma2u) - 1)
GPvariance2

# Marginal variance: Level-1 component (variance within clusters)
GPvariance1 <- GPexpectation
GPvariance1

# Level-2 VPC (variance partition coefficient)
GPvpc2 <- GPvariance2/(GPvariance2 + GPvariance1)
GPvpc2

# Level-1 VPC (variance partition coefficient - variance at the individual level)
GPvpc1 <- GPvariance1/(GPvariance2 + GPvariance1)
GPvpc1

var_null <- GPvariance2

model1_output <- rbind(GPbeta0, GPsigma2u, GPexpectation, GPvariance, GPvariance2, GPvariance1, GPvpc2, GPvpc1)
rownames(model1_output) <-c("GPbeta0", "GPsigma2u", "GPexpectation", "GPvariance", "GPvariance2", "GPvariance1", "GPvpc2", "GPvpc1")

fwrite(model1_output, here::here("output", "os_reports", "WP3", "GPmodel1_output.csv"))

##################################################################################################################################
#Model 3::: adjusted model Poisson ---------------
# fully adjusted model 

fm3 <- glmmTMB(gp_1m ~ 1 + sex + age_band + Ethnicity_2 + imd_quintile_R + (1|strata), data = GP_MAIHDA, family = poisson)
summary(fm3)


GP_model3 <-capture.output(model_parameters(fm3, exponentiate = TRUE, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "GP_model3.txt")

writeLines(GP_model3, con = Output_file)

cat("Output saved to", Output_file, "\n")

# Linear predictor
GP_MAIHDA$xb <- predict(fm3)
head(GP_MAIHDA)

# Cluster variance
str(summary(fm3))
GPsigma2u3 <- summary(fm3)$varcor$cond$strata[1,1]
GPsigma2u3

# Marginal expectation
GP_MAIHDA$GPexpectation3 <- exp(GP_MAIHDA$xb + GPsigma2u3$x/2)
head(GP_MAIHDA)

# Marginal variance
GP_MAIHDA$GPvariance3 <- GP_MAIHDA$GPexpectation3 + GP_MAIHDA$GPexpectation3^2 * (exp(GPsigma2u3$x) - 1)
head(GP_MAIHDA)

# Marginal variance: Level-2 component
GP_MAIHDA$GPvariance2m3 <- GP_MAIHDA$GPexpectation3^2*(exp(GPsigma2u3$x) - 1)
head(GP_MAIHDA)

# Marginal variance: Level-1 component
GP_MAIHDA$GPvariance1m3 <- GP_MAIHDA$GPexpectation3
head(GP_MAIHDA)

# Level-2 VPC
GP_MAIHDA$GPvpc2m3 <- GP_MAIHDA$GPvariance2m3/(GP_MAIHDA$GPvariance2m3 + GP_MAIHDA$GPvariance1m3)
head(GP_MAIHDA)

# Level-1 VPC
GP_MAIHDA$GPvpc1m3 <- GP_MAIHDA$GPvariance1m3/(GP_MAIHDA$GPvariance2m3 + GP_MAIHDA$GPvariance1m3)
head(GP_MAIHDA)

# Summarise marginal statistics------------------------------
colnames(GP_MAIHDA)
summ <- colMeans(GP_MAIHDA[32:38])
summ
summ_df <- data.frame(summ) 
rownames(summ_df) <- c("xb", "GPexpectation", "GPvariance", "GPvariance2", "GPvariance1", "GPvpc2", "GPvpc1")

var_adj <- summ[4]


write.csv(summ_df,file=here::here("output", "os_reports", "WP3","GPsummVPC.csv"))


GPpcv <- (var_null - var_adj)/var_null
GPpcv <- data.frame(GPpcv)
rownames(GPpcv) <- c("GPpcv")

fwrite(GPpcv, here::here("output", "os_reports", "WP3", "GPpcv.csv"))





