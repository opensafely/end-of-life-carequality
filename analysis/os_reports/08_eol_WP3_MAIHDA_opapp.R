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

df$imd_quintile_R <- factor(df$imd_quintile)

OP_MAIHDA <-df %>%
  group_by(sex, age_band, Ethnicity_2, imd_quintile_R) %>% 
  mutate(strata = cur_group_id())


#####Strata
cols_of_interest2 <- "total"

df_strata <- OP_MAIHDA %>%
  select(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  group_by(strata, sex, age_band, Ethnicity_2, imd_quintile_R) %>%
  summarise(total = n()) %>%
  dplyr::mutate(across(.cols = all_of(cols_of_interest2), .fns = ~ .x %>% `/`(5) %>% round()*5));
  

write.csv(df_strata, file = 'OPstrata_df.csv', row.names = FALSE)
df_strata <-read_csv(file = "OPstrata_df.csv")
fwrite(df_strata, here::here("output", "os_reports", "WP3", "OPstrata_df.csv"))

# Model 1  - includes a strata random intercept to account for clustering by strata #
# Calculate simple intersectional model (Null model - Captures variation without considering the impact of any specific predictor)
# Poisson model = model for count data

fm1 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = poisson)
summary(fm1)


OP_model1 <-capture.output(model_parameters(fm1, exponentiate = TRUE, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "OP_model1.txt")

writeLines(OP_model1, con = Output_file)

cat("Output saved to", Output_file, "\n")

# Intercept (mean no. of outpatient attendances when all predictors = 0)
str(summary(fm1))
OPbeta0 <- summary(fm1)$coefficients$cond[1,1]
OPbeta0

# write.csv (OPbeta0, file = 'OPbeta0.csv', row.names = FALSE)
# OPbeta0 <- read_csv(file =  "OPbeta0.csv")
# fwrite(OPbeta0, here::here("output", "os_reports", "WP3", "OPbeta0.csv"))


# Cluster variance
str(summary(fm1))
OPsigma2u <- summary(fm1)$varcor$cond$strata[1,1]
OPsigma2u

# write.csv(OPsigma2u, file = 'OPsigma2u.csv', row.names = FALSE)
# OPsigma2u <-read_csv(file = "OPsigma2u.csv")
# fwrite(OPsigma2u, here::here("output", "os_reports", "WP3", "OPsigma2u.csv"))

# Marginal expectation (approximately = mean no. of outpatient attendances)
OPexpectation <- exp(OPbeta0 + OPsigma2u/2)
OPexpectation

# write.csv(OPexpectation, file = 'OPexpectation.csv', row.names = FALSE)
# OPexpectation <-read_csv(file = "OPexpectation.csv")
# fwrite(OPexpectation, here::here("output", "os_reports", "WP3", "OPexpectation.csv"))

#Marginal variance
OPvariance <- OPexpectation + OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance

# write.csv(OPvariance, file = 'OPvariance.csv', row.names = FALSE)
# OPvariance <-read_csv(file = "OPvariance.csv")
# fwrite(OPvariance, here::here("output", "os_reports", "WP3", "OPvariance.csv"))

# Marginal variance: Level-2 component (Variance between clusters - based on age groups, sex, etc)
OPvariance2 <- OPexpectation^2*(exp(OPsigma2u) - 1)
OPvariance2

# write.csv(OPvariance2, file = 'OPvariance2.csv', row.names = FALSE)
# OPvariance2 <-read_csv(file = "OPvariance2.csv")
# fwrite(OPvariance2, here::here("output", "os_reports", "WP3", "OPvariance2.csv"))

# Marginal variance: Level-1 component (variance within clusters)
OPvariance1 <- OPexpectation
OPvariance1

# write.csv(OPvariance1, file = 'OPvariance1.csv', row.names = FALSE)
# OPvariance1 <-read_csv(file = "OPvariance1.csv")
# fwrite(OPvariance1, here::here("output", "os_reports", "WP3", "OPvariance1.csv"))

# Level-2 VPC (variance partition coefficient)
OPvpc2 <- OPvariance2/(OPvariance2 + OPvariance1)
OPvpc2

# write.csv(OPvpc2, file = 'OPvpc2.csv')
# OPvpc2.csv <-read_csv(file = "OPvpc2.csv")
# fwrite(OPvpc2, here::here("output", "os_reports", "WP3", "OPOPvpc2.csv"))

# Level-1 VPC (variance partition coefficient - variance at the individual level)
OPvpc1 <- OPvariance1/(OPvariance2 + OPvariance1)
OPvpc1

# write.csv(OPvpc1, file = 'OPvpc1.csv')
# OPvpc1.csv <-read_csv(file = "OPvpc1.csv")
# fwrite(OPvpc1, here::here("output", "os_reports", "WP3", "OPOPvpc1.csv"))

var_null <- OPvariance2



write.csv (OPbeta0, file = 'OPbeta0.csv', row.names ="OPbeta0")
write.csv (OPsigma2u, file = 'OPsigma2u.csv', row.names = "OPsigma2u")
write.csv (OPexpectation, file = 'OPexpectation.csv', row.names = "OPexpectation")
write.csv (OPvariance, file = 'OPvariance.csv', row.names = "OPvariance")
write.csv (OPvariance2, file = 'OPvariance2.csv', row.names = "OPvariance2")
write.csv (OPvariance1, file = 'OPvariance1.csv', row.names = "OPvariance1")
write.csv (OPvpc2, file = 'OPvpc2.csv', row.names = "OPvpc2")
write.csv (OPvpc1, file = 'OPvpc1.csv', row.names = "OPvpc1")

model1_output <- rbind(OPbeta0, OPsigma2u, OPexpectation, OPvariance, OPvariance2, OPvariance1, OPvpc2, OPvpc1)

write.csv(model1_output, "OPmodel1_output.csv")
model1_output <-read_csv(file = "OPmodel1_output.csv")
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

# Linear predictor
OP_MAIHDA$xb <- predict(fm3)
head(OP_MAIHDA)

#predictions for each strata
# Calculates mean and se per strata using linear regression with no intercept
# Calculates 95% CIs
OP_MAIHDA$exb <-exp(OP_MAIHDA$xb) 
OP_MAIHDA$strata <- as.factor(OP_MAIHDA$strata)
strata_predictions <- lm(exb ~ 0 + strata, data = OP_MAIHDA) 

strata_ci <- confint(strata_predictions, level=0.95) # obtain 95% CIs

summary(strata_predictions)

OP_strata_predictions <-capture.output(summary(strata_predictions, print_trivials = TRUE))

Output_file <- here::here("output", "os_reports", "WP3", "OP_strata_predictions.txt")

writeLines(OP_strata_predictions, con = Output_file)

cat("Output saved to", Output_file, "\n")

write.csv(strata_ci, file = 'OPstrataCI.csv', row.names = TRUE)
OPstrataCI <-read_csv(file = "OPstrataCI.csv")
fwrite(OPstrataCI, here::here("output", "os_reports", "WP3", "OPstrataCI.csv"))


# write.csv(df$xb, file = 'OPpredict.csv', row.names = FALSE)
# OPpredict <-read_csv(file = "OPpredict.csv")
# fwrite(OPpredict, here::here("output", "os_reports", "WP3", "OPpredict.csv"))

# Cluster variance
str(summary(fm3))
OPsigma2u3 <- summary(fm3)$varcor$cond$strata[1,1]
OPsigma2u3


write.csv(OPsigma2u3, file = 'OPsigma2u3.csv', row.names = FALSE)
OPsigma2u3 <-read_csv(file = "OPsigma2u3.csv")
fwrite(OPsigma2u3, here::here("output", "os_reports", "WP3", "OPsigma2u3.csv"))

# Marginal expectation
OP_MAIHDA$OPexpectation3 <- exp(OP_MAIHDA$xb + OPsigma2u3$x/2)
head(OP_MAIHDA)


# write.csv(df$OPexpectation3, file = 'OPmarginalexpectation.csv', row.names = FALSE)
# df$OPexpectation3 <-read_csv(file = "OPmarginalexpectation.csv")
# fwrite(df$OPexpectation3, here::here("output", "os_reports", "WP3", "OPmarginalexpectation.csv"))

# Marginal variance
OP_MAIHDA$OPvariance3 <- OP_MAIHDA$OPexpectation3 + OP_MAIHDA$OPexpectation3^2 * (exp(OPsigma2u3$x) - 1)
head(OP_MAIHDA)

# write.csv(df$OPvariance3, file = 'OPmarginalvariance.csv', row.names = FALSE)
# df$OPvariance3 <-read_csv(file = "OPmarginalvariance.csv")
# fwrite(df$OPvariance3, here::here("output", "os_reports", "WP3", "OPmarginalvariance.csv"))

# Marginal variance: Level-2 component
OP_MAIHDA$OPvariance2m3 <- OP_MAIHDA$OPexpectation3^2*(exp(OPsigma2u3$x) - 1)
head(OP_MAIHDA)

# write.csv(df$OPvariance2m3, file = 'OPmarginalvariance2.csv', row.names = FALSE)
# df$OPvariance2m3 <-read_csv(file = "OPmarginalvariance2.csv")
# fwrite(df$OPvariance2m3, here::here("output", "os_reports", "WP3", "OPmarginalvariance2.csv"))

# Marginal variance: Level-1 component
OP_MAIHDA$OPvariance1m3 <- OP_MAIHDA$OPexpectation3
head(OP_MAIHDA)

# write.csv(df$OPvariance1m3, file = 'OPmarginalvarianceL1.csv', row.names = FALSE)
# df$OPvariance1m3 <-read_csv(file = "OPmarginalvarianceL1.csv")
# fwrite(df$OPvariance1m3, here::here("output", "os_reports", "WP3", "OPmarginalvarianceL1.csv"))

# Level-2 VPC
OP_MAIHDA$OPvpc2m3 <- OP_MAIHDA$OPvariance2m3/(OP_MAIHDA$OPvariance2m3 + OP_MAIHDA$OPvariance1m3)
head(OP_MAIHDA)

# write.csv(df$OPvpc2m3, file = 'OPVPC2m3.csv', row.names = FALSE)
# df$OPvpc2m3 <-read_csv(file = "OPVPC2m3.csv")
# fwrite(df$OPvpc2m3, here::here("output", "os_reports", "WP3", "OPVPC2m3.csv"))

# Level-1 VPC
OP_MAIHDA$OPvpc1m3 <- OP_MAIHDA$OPvariance1m3/(OP_MAIHDA$OPvariance2m3 + OP_MAIHDA$OPvariance1m3)
head(OP_MAIHDA)

# write.csv(df$OPvpc1m3, file = 'OPVPC1m3.csv', row.names = FALSE)
# df$OPvpc1m3 <-read_csv(file = "OPVPC1m3.csv")
# fwrite(df$OPvpc1m3, here::here("output", "os_reports", "WP3", "OPVPC1m3.csv"))

# Summarise marginal statistics------------------------------
colnames(OP_MAIHDA)
summ <- colMeans(OP_MAIHDA[32:38])
summ
summ_df <- data.frame(summ) 
rownames(summ_df) <- c("xb", "OPexpectation", "OPvariance", "OPvariance2", "OPvariance1", "OPvpc2", "OPvpc1")
summ_df

var_adj <- summ[4]

write.csv(summ_df, file = 'OPsummVPC.csv', row.names = TRUE)
summ_df <-read_csv(file = "OPsummVPC.csv")
fwrite(summ_df, here::here("output", "os_reports", "WP3", "OPsumVPC.csv"))

OPpcv <- (var_null - var_adj)/var_null

write.csv(OPpcv, file = 'OPpcv.csv', row.names = FALSE)
OPpcv <-read_csv(file = "OPpcv.csv")
fwrite(OPpcv, here::here("output", "os_reports", "WP3", "OPpcv.csv"))


#Figures -------------------------------
#Line plot of Level-2 VPC against the marginal expectation
# lineplot <- ggplot(data = OP_MAIHDA, mapping = aes(x = OPexpectation3$x, y = OPvpc2m3$x)) + geom_line()
# print(lineplot)
# 
# ggsave(lineplot, dpi = 600, width = 20, height = 10, unit = "cm"
#        , filename = "OPlineplot.png"
#        , path = here::here("output", "os_reports", "WP3"))

# Spikeplot of marginal expectation
histogram <- ggplot(data = OP_MAIHDA, mapping = aes(x = OPexpectation3)) +
  geom_histogram(binwidth=1)

ggsave(histogram, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "OPhistogram.png"
       , path = here::here("output", "os_reports", "WP3"))

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

write.csv(fm3vsfm1, file = 'OPfm3vsfm1.csv')
fm3vsfm1 <-read_csv(file = "OPfm3vsfm1.csv")
fwrite(fm3vsfm1, here::here("output", "os_reports", "WP3", "OPfm3vsfm1.csv"))

scatterplot <- ggplot(data = fm3vsfm1, mapping = aes(x = fm1u, y = fm3u)) + geom_point()
cor(fm3vsfm1)


ggsave(scatterplot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "OPscatterplot.png"
       , path = here::here("output", "os_reports", "WP3"))

# Rank the model 1 predicted cluster random effects
fm3vsfm1$fm1urank <- rank(fm3vsfm1$fm1u)
head(fm3vsfm1)

write.csv(fm3vsfm1$fm1urank, file = 'OPrank1.csv')
fm3vsfm1$fm1urank <-read_csv(file = "OPrank1.csv")
fwrite(fm3vsfm1$fm1urank, here::here("output", "os_reports", "WP3", "OPrank1.csv"))

# Rank the model 3 predicted cluster random effects
fm3vsfm1$fm3urank <- rank(fm3vsfm1$fm3u)
head(fm3vsfm1)

write.csv(fm3vsfm1$fm3urank, file = 'OPrank3.csv')
fm3vsfm1$fm3urank <-read_csv(file = "OPrank3.csv")
fwrite(fm3vsfm1$fm3urank, here::here("output", "os_reports", "WP3", "OPrank3.csv"))

# Figure 4: Scatterplot of ranks of model 3 vs. model 1 predicted effects
# rankscatterplot <- ggplot(data = fm3vsfm1$fm1urank$fm3urank, mapping = aes(x = fm1urank, y = fm3urank)) + geom_point()
# colnames(fm3vsfm1)
# cor(fm3vsfm1[,3:4])
# 
# # write.csv(cor(fm3vsfm1[,3:4]), file = 'OPvarrank.csv')
# # cor(fm3vsfm1[,3:4]) <-read_csv(file = "OPvarrank.csv")
# # fwrite(cor(fm3vsfm1[,3:4]), here::here("output", "os_reports", "WP3", "OPvarrank.csv"))
# 
# 
# ggsave(rankscatterplot, dpi = 600, width = 20, height = 10, unit = "cm"
#        , filename = "OPrankscatterplot.png"
#        , path = here::here("output", "os_reports", "WP3"))



############################################################################

# Model 2: Two-level variance-components negative binomial model
# To test fit

# # Fit model
# fm2 <- glmmTMB(opapp_1m ~ 1 + (1|strata), data = OP_MAIHDA, family = nbinom2)
# summary(fm2)
# 
# # Intercept
# str(summary(fm2))
# OPbeta02 <- summary(fm2)$coefficients$cond[1,1]
# OPbeta02
# 
# 
# write.csv (OPbeta02, file = 'OPbeta02.csv', row.names = FALSE)
# OPbeta02 <- read_csv(file =  "OPbeta02.csv")
# fwrite(OPbeta02, here::here("output", "os_reports", "WP3", "OPbeta02.csv"))
# 
# # Cluster variance
# str(summary(fm2))
# OPsigma2u2 <- summary(fm2)$varcor$cond$strata[1,1]
# OPsigma2u2
# 
# write.csv(OPsigma2u2, file = 'OPsigma2u2.csv', row.names = FALSE)
# OPsigma2u2 <-read_csv(file = "OPsigma2u2.csv")
# fwrite(OPsigma2u2, here::here("output", "os_reports", "WP3", "OPsigma2u2.csv"))
# 
# # Overdispersion parameter
# str(summary(fm2))
# OPalpha2 <- 1/(summary(fm2)$OPsigma2u)
# OPalpha2
# 
# write.csv(OPalpha2, file = 'OPalpha2.csv', row.names = FALSE)
# OPalpha2 <-read_csv(file = "OPalpha2.csv")
# fwrite(OPalpha2, here::here("output", "os_reports", "WP3", "OPalpha2.csv"))
# 
# # Marginal expectation
# OPexpectation2 <- exp(OPbeta02 + OPsigma2u2/2)
# OPexpectation2
# 
# write.csv(OPexpectation2, file = 'OPexpectation2.csv', row.names = FALSE)
# OPexpectation2 <-read_csv(file = "OPexpectation2.csv")
# fwrite(OPexpectation2, here::here("output", "os_reports", "WP3", "OPexpectation2.csv"))
# 
# # Marginal variance
# OPvariancem2 <- OPexpectation2 + OPexpectation2^2*(exp(OPsigma2u2)*(1 + OPalpha2) - 1)
# OPvariancem2
# 
# write.csv(OPvariancem2, file = 'OPvariancem2.csv', row.names = FALSE)
# OPvariancem2 <-read_csv(file = "OPvariancem2.csv")
# fwrite(OPvariancem2, here::here("output", "os_reports", "WP3", "OPvariancem2.csv"))
# 
# # Marginal variance: Level-2 component
# OPvariance2m2 <- OPexpectation2^2*(exp(OPsigma2u2) - 1)
# OPvariancem2
# 
# write.csv(OPvariance2m2, file = 'OPvariance2m2.csv', row.names = FALSE)
# OPvariance2m2 <-read_csv(file = "OPvariance2m2.csv")
# fwrite(OPvariance2m2, here::here("output", "os_reports", "WP3", "OPvariance2m2.csv"))
# 
# # Marginal variance: Level-1 component
# OPvariance1m2 <- OPexpectation2 + OPexpectation2^2*exp(OPsigma2u2)*OPalpha2
# OPvariance1m2
# 
# write.csv(OPvariance1m2, file = 'OPvariance1m2.csv', row.names = FALSE)
# OPvariance1m2 <-read_csv(file = "OPvariance1m2.csv")
# fwrite(OPvariance1m2, here::here("output", "os_reports", "WP3", "OPvariance1m2.csv"))
# 
# # Level-2 VPC
# OPvpc2m2 <- OPvariance2m2/(OPvariance2m2 + OPvariance1m2)
# OPvpc2m2
# 
# write.csv(OPvpc2m2, file = 'OPvpc2m2.csv')
# OPvpc2m2.csv <-read_csv(file = "OPvpc2m2.csv")
# fwrite(OPvpc2m2, here::here("output", "os_reports", "WP3", "OPOPvpc2m2.csv"))
# 
# # Level-1 VPC
# OPvpc1m2 <- OPvariance1m2/(OPvariance2m2 + OPvariance1m2)
# OPvpc1
# 
# write.csv(OPvpc1m2, file = 'OPvpc1m2.csv')
# OPvpc1m2.csv <-read_csv(file = "OPvpc1m2.csv")
# fwrite(OPvpc1m2, here::here("output", "os_reports", "WP3", "OPOPvpc1m2.csv"))
# 
# ###############################################################################
# 
# 
