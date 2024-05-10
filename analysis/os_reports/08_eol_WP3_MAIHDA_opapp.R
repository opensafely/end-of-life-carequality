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
#filter(study_month >= startdate & study_month <= enddate & imd_quintile >=1 & age_band != "0-24" & codgrp == "Cancer" & pod_ons_new == "Home")

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

#Marginal variance
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
OPvpc2 <- OPvariance2/(OPvariance2 + OPvariance1)
OPvpc2

write.csv(OPvpc2, file = 'OPvpc2.csv')
OPvpc2.csv <-read_csv(file = "OPvpc2.csv")
fwrite(OPvpc2, here::here("output", "os_reports", "WP3", "OPOPvpc2.csv"))

# Level-1 VPC (variance partition coefficient - variance at the individual level)
OPvpc1 <- OPvariance1/(OPvariance2 + OPvariance1)
OPvpc1

write.csv(OPvpc1, file = 'OPvpc1.csv')
OPvpc1.csv <-read_csv(file = "OPvpc1.csv")
fwrite(OPvpc1, here::here("output", "os_reports", "WP3", "OPOPvpc1.csv"))

##################################################################################################################################

# Model 2: Two-level variance-components negative binomial model
# Regression coefficients, not just the intercept, are now allowed to vary across clusters
# Negative binomial models account for the variability caused by overdispersion

# Fit model
fm2 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = nbinom2)
summary(fm2)

# Intercept
str(summary(fm2))
OPbeta02 <- summary(fm2)$coefficients$cond[1,1]
OPbeta02


write.csv (OPbeta02, file = 'OPbeta02.csv', row.names = FALSE)
OPbeta02 <- read_csv(file =  "OPbeta02.csv")
fwrite(OPbeta02, here::here("output", "os_reports", "WP3", "OPbeta02.csv"))

# Cluster variance
str(summary(fm2))
OPsigma2u2 <- summary(fm2)$varcor$cond$strata[1,1]
OPsigma2u2

write.csv(OPsigma2u2, file = 'OPsigma2u2.csv', row.names = FALSE)
OPsigma2u2 <-read_csv(file = "OPsigma2u2.csv")
fwrite(OPsigma2u2, here::here("output", "os_reports", "WP3", "OPsigma2u2.csv"))

# Overdispersion parameter
str(summary(fm2))
OPalpha2 <- 1/(summary(fm2)$OPsigma2u)
OPalpha2

write.csv(OPalpha2, file = 'OPalpha2.csv', row.names = FALSE)
OPalpha2 <-read_csv(file = "OPalpha2.csv")
fwrite(OPalpha2, here::here("output", "os_reports", "WP3", "OPalpha2.csv"))

# Marginal expectation
OPexpectation2 <- exp(OPbeta02 + OPsigma2u2/2)
OPexpectation2

write.csv(OPexpectation2, file = 'OPexpectation2.csv', row.names = FALSE)
OPexpectation2 <-read_csv(file = "OPexpectation2.csv")
fwrite(OPexpectation2, here::here("output", "os_reports", "WP3", "OPexpectation2.csv"))

# Marginal variance
OPvariancem2 <- OPexpectation2 + OPexpectation2^2*(exp(OPsigma2u2)*(1 + OPalpha2) - 1)
OPvariancem2

write.csv(OPvariancem2, file = 'OPvariancem2.csv', row.names = FALSE)
OPvariancem2 <-read_csv(file = "OPvariancem2.csv")
fwrite(OPvariancem2, here::here("output", "os_reports", "WP3", "OPvariancem2.csv"))

# Marginal variance: Level-2 component
OPvariance2m2 <- OPexpectation2^2*(exp(OPsigma2u2) - 1)
OPvariancem2

write.csv(OPvariance2m2, file = 'OPvariance2m2.csv', row.names = FALSE)
OPvariance2m2 <-read_csv(file = "OPvariance2m2.csv")
fwrite(OPvariance2m2, here::here("output", "os_reports", "WP3", "OPvariance2m2.csv"))

# Marginal variance: Level-1 component
OPvariance1m2 <- OPexpectation2 + OPexpectation2^2*exp(OPsigma2u2)*OPalpha2
OPvariance1m2

write.csv(OPvariance1m2, file = 'OPvariance1m2.csv', row.names = FALSE)
OPvariance1m2 <-read_csv(file = "OPvariance1m2.csv")
fwrite(OPvariance1m2, here::here("output", "os_reports", "WP3", "OPvariance1m2.csv"))

# Level-2 VPC
OPvpc2m2 <- OPvariance2m2/(OPvariance2m2 + OPvariance1m2)
OPvpc2m2

write.csv(OPvpc2m2, file = 'OPvpc2m2.csv')
OPvpc2m2.csv <-read_csv(file = "OPvpc2m2.csv")
fwrite(OPvpc2m2, here::here("output", "os_reports", "WP3", "OPOPvpc2m2.csv"))

# Level-1 VPC
OPvpc1m2 <- OPvariance1m2/(OPvariance2m2 + OPvariance1m2)
OPvpc1

write.csv(OPvpc1m2, file = 'OPvpc1m2.csv')
OPvpc1m2.csv <-read_csv(file = "OPvpc1m2.csv")
fwrite(OPvpc1m2, here::here("output", "os_reports", "WP3", "OPOPvpc1m2.csv"))

###############################################################################
#Two-level random-intercept negative binomial model 
# fully adjusted model 

fm3 <- glmmTMB(opapp_1m ~ 1 + sex + age_band + Ethnicity_2 + imd_quintile + (1|strata), data = OP_MAIHDA, family = poisson)
summary(fm3)

# Linear predictor
df$xb <- predict(fm3)
head(df)

write.csv(df$xb, file = 'OPpredict.csv', row.names = FALSE)
OPpredict <-read_csv(file = "OPpredict.csv")
fwrite(OPpredict, here::here("output", "os_reports", "WP3", "OPpredict.csv"))

# Cluster variance
str(summary(fm3))
OPsigma2u3 <- summary(fm3)$varcor$cond$strata[1,1]
OPsigma2u3

write.csv(OPsigma2u3, file = 'OPsigma2u3.csv', row.names = FALSE)
OPsigma2u3 <-read_csv(file = "OPsigma2u3.csv")
fwrite(OPsigma2u3, here::here("output", "os_reports", "WP3", "OPsigma2u3.csv"))

# Overdispersion parameter
str(summary(fm3))
OPalpha3 <- 1/(summary(fm3)$OPsigma2u3)
OPalpha3

write.csv(OPalpha3, file = 'OPalpha3.csv', row.names = FALSE)
OPalpha3 <-read_csv(file = "OPalpha3.csv")
fwrite(OPalpha3, here::here("output", "os_reports", "WP3", "OPalpha3.csv"))

#############################check saving##################################### ------------------------
# Marginal expectation
df$OPexpectation3 <- exp(df$xb + OPsigma2u3/2)
head(df)

write.csv(df$OPexpectation3, file = 'OPmarginalexpectation.csv', row.names = FALSE)
df$OPexpectation3 <-read_csv(file = "OPmarginalexpectation.csv")
fwrite(df$OPexpectation3, here::here("output", "os_reports", "WP3", "OPmarginalexpectation.csv"))

# Marginal variance
df$OPvariance3 <- df$OPexpectation3 +
  df$OPexpectation3^2*(exp(OPsigma2u3)*(1 + OPalpha3) - 1)
head(df)

write.csv(df$OPvariance3, file = 'OPmarginalvariance.csv', row.names = FALSE)
df$OPvariance3 <-read_csv(file = "OPmarginalvariance.csv")
fwrite(df$OPvariance3, here::here("output", "os_reports", "WP3", "OPmarginalvariance.csv"))

# Marginal variance: Level-2 component
df$OPvariance2m3 <- df$OPexpectation3^2*(exp(OPsigma2u3) - 1)
head(df)

write.csv(df$OPvariance2m3, file = 'OPmarginalvariance2.csv', row.names = FALSE)
df$OPvariance2m3 <-read_csv(file = "OPmarginalvariance2.csv")
fwrite(df$OPvariance2m3, here::here("output", "os_reports", "WP3", "OPmarginalvariance2.csv"))

# Marginal variance: Level-1 component
df$OPvariance1m3 <- df$OPexpectation3 + 
  df$OPexpectation3^2*exp(OPsigma2u3)*OPalpha3
head(df)

write.csv(df$OPvariance1m3, file = 'OPmarginalvarianceL1.csv', row.names = FALSE)
df$OPvariance1m3 <-read_csv(file = "OPmarginalvarianceL!.csv")
fwrite(df$OPvariance1m3, here::here("output", "os_reports", "WP3", "OPmarginalvarianceL1.csv"))

# Level-2 VPC
df$OPvpc2m3 <- df$OPvariance2m3/(df$OPvariance3 + df$OPvariance1m3)
head(df)

write.csv(df$OPvpc2m3, file = 'OPVPC2m3.csv', row.names = FALSE)
df$OPvpc2m3 <-read_csv(file = "OPVPC2m3.csv")
fwrite(df$OPvpc2m3, here::here("output", "os_reports", "WP3", "OPVPC2m3.csv"))


# Level-1 VPC
df$OPvpc1m3 <- df$OPvariance1m3/(df$OPvariance2m3 + df$OPvariance1m3)
head(df)

write.csv(df$OPvpc1m3, file = 'OPVPC1m3.csv', row.names = FALSE)
df$OPvpc1m3 <-read_csv(file = "OPVPC1m3.csv")
fwrite(df$OPvpc1m3, here::here("output", "os_reports", "WP3", "OPVPC1m3.csv"))

# Summarise marginal statistics------------------------------# clarification on this code?
colnames(df)
sapply(df[7:12], mean)

#Figures -------------------------------
# Line plot of Level-2 VPC against the marginal expectation
# lineplot <- ggplot(data = df, mapping = aes(x = OPexpectation3, y = OPvpc2m3)) + geom_line()
# print(lineplot)
# 
# ggsave(lineplot, dpi = 600, width = 20, height = 10, unit = "cm"
#        , filename = "lineplot.png"
#        , path = here::here("output", "os_reports", "WP3"))
# 
# # Spikeplot of marginal expectation
# histogram <- ggplot(data = df, mapping = aes(x = OPexpectation3)) + 
#   geom_histogram(binwidth=1)
# 
# ggsave(histogram, dpi = 600, width = 20, height = 10, unit = "cm"
#        , filename = "histogram.png"
#        , path = here::here("output", "os_reports", "WP3"))

# Predict cluster random intercept effects 
fm3u <- ranef(fm3)
str(fm3u)
head(fm3u$cond$strata)

write.csv(fm3u$cond$strata, file = 'OPclusterintercept3.csv', row.names = FALSE)
fm3u$cond$strata <-read_csv(file = "OPclusterintercept3.csv")
fwrite(fm3u$cond$strata, here::here("output", "os_reports", "WP3", "OPclusterintercept3.csv"))

fm1u <- ranef(fm1)
fm1u

# Scatterplot of model 3 vs. model 1 predicted cluster random effects
fm3vsfm1 <- cbind(fm1u$cond$strata,fm3u$cond$strata)
colnames(fm3vsfm1)
colnames(fm3vsfm1) <- c("fm1u", "fm3u")
colnames(fm3vsfm1)
head(fm3vsfm1)

write.csv(fm3vsfm1, file = 'OPfm3vsfm1.csv', row.names = FALSE, show_col_types = FALSE)
fm3vsfm1 <-read_csv(file = "OPfm3vsfm1.csv")
fwrite(fm3vsfm1, here::here("output", "os_reports", "WP3", "OPfm3vsfm1.csv"))


scatterplot <- ggplot(data = fm3vsfm1, mapping = aes(x = fm1u, y = fm3u)) + geom_point()
cor(fm3vsfm1)

ggsave(scatterplot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "scatterplot.png"
       , path = here::here("output", "os_reports", "WP3"))

# Rank the model 1 predicted cluster random effects
fm3vsfm1$fm1urank <- rank(fm3vsfm1$fm1u)
head(fm3vsfm1)

# Rank the model 3 predicted cluster random effects
fm3vsfm1$fm3urank <- rank(fm3vsfm1$fm3u)
head(fm3vsfm1)

# Figure 4: Scatterplot of ranks of model 3 vs. model 1 predicted effects
rankscatterplot <- ggplot(data = fm3vsfm1, mapping = aes(x = fm1urank, y = fm3urank)) + 
geom_point()
colnames(fm3vsfm1)
cor(fm3vsfm1[,3:4])

ggsave(rankscatterplot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "rankscatterplot.png"
       , path = here::here("output", "os_reports", "WP3"))





############################################################################

# Predict cluster random intercept effects
fm2u <- ranef(fm2)
fm2u

############################################################################

# Five-level variance-components negative binomial model
# Variance-component models quantify the proportion of variation in the response due to systematic differences between clusters.

# Fit model
# fm3 <- glmmTMB(opapp_1m ~ 1 + (1|sex) + (1|age_band) + (1|Ethnicity_2) + (1|imd_quintile), 
#                data = OP_MAIHDA, family = nbinom2)
# summary(fm3)
# 
# # Intercept
# str(summary(fm3))
# OPbeta0 <- summary(fm3)$coefficients$cond[1,1]
# OPbeta0
# 
# # Cluster variance - sex
# str(summary(fm3))
# OPsigma2u <- summary(fm3)$varcor$cond$sex[1,1]
# OPsigma2u
# 
# # Cluster variance - imd_quintile
# str(summary(fm3))
# sigma2v <- summary(fm3)$varcor$cond$imd_quintile[1,1]
# sigma2v
# 
# str(summary(fm3))
# sigma2w <- summary(fm3)$varcor$cond$Ethnicity_2[1,1]
# sigma2w
# 
# str(summary(fm3))
# sigma2x <- summary(fm3)$varcor$cond$age_band[1,1]
# sigma2x
# 
# # Overdispersion parameter
# str(summary(fm3))
# alpha <- 1/(summary(fm3)$sigma)
# alpha
# 
# # Marginal expectation
# OPexpectation <- exp(OPbeta0 + OPsigma2u/2 + sigma2v/2 + sigma2w/2 + sigma2x/2)
# OPexpectation
# 
# # Marginal variance
# variance <- OPexpectation + 
#   OPexpectation^2*(exp(OPsigma2u + sigma2v + sigma2w + sigma2x)*(1 + alpha) - 1)
# variance
# 
# # Marginal variance: Level-5 component
# variance5 <- (OPexpectation^2*(exp(OPsigma2u) - 1))
# variance5
# 
# # Marginal variance: Level-4 component
# variance4 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v) - 1)
# variance4
# 
# # Marginal variance: Level-3 component
# variance3 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v)*(exp(sigma2w) - 1))
# variance3
# 
# # Marginal variance: Level-2 component
# OPvariance2 <- OPexpectation^2*exp(OPsigma2u)*(exp(sigma2v)*(exp(sigma2w)*(exp(sigma2x) - 1)))
# OPvariance2
# 
# # Marginal variance: Level-1 component
# OPvariance1 <- OPexpectation + OPexpectation^2*exp(OPsigma2u + sigma2v + sigma2w + sigma2x)*alpha
# OPvariance1
# 
# # Should the different level VPC be interpreted as the proportion of variance in outcome explained by each characteristic? 
# 
# # Level-5 VPC
# vpc5 <- variance5/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
# vpc5
# 
# # Level-4 VPC
# vpc4 <- variance4/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
# vpc4
# 
# # Level-3 VPC
# vpc3 <- variance3/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
# vpc3
# 
# # Level-2 VPC
# OPvpc2 <- OPvariance2/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
# OPvpc2
# 
# # Level-1 VPC
# OPvpc1 <- OPvariance1/(variance5 + variance4 + variance3 + OPvariance2 + OPvariance1)
# OPvpc1
