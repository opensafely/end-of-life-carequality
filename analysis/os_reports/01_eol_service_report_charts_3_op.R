#-------------------------------------------------------------------------------
# Charts for eol_service_report
# Date: 26.07.2023
# Author: Eilís
# Aim: Create png image files of charts for OpenSAFELY reports
#-------------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dplyr)

# Load helper functions, code settings and input dataframe (df)
source("analysis/os_reports/helpers.R")

# Outpatient appointments -------------------------------------------

# Number of people with at least one outpatient appointment in the last month of life by month and place of death - all deaths

opapp_count_place <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = sum(opapp_1m >= 1), total = n()) %>%
  mutate(count = plyr::round_any(count, 10)
         ,  total = plyr::round_any(total, 10))

write_csv(opapp_count_place, here::here("output", "os_reports", "eol_service", "opapp_count_place.csv"))

# Number of people with at least one outpatient appointment in the last month of life by month and cause of death

opapp_count_cause <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(count = sum(opapp_1m >= 1), total = n()) %>%
  mutate(count = plyr::round_any(count, 10)
         ,  total = plyr::round_any(total, 10))

write_csv(opapp_count_cause, here::here("output", "os_reports", "eol_service", "opapp_count_cause.csv"))

# Mean outpatient appointments by month and place of death - including all deaths
opapp_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(mean = mean(opapp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(opapp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(mean = round(mean, 3))

write_csv(opapp_month, here::here("output", "os_reports", "eol_service", "opapp_month.csv"))

op_month_plot <- ggplot(opapp_month, aes(x = study_month, y = mean
                                         , group = pod_ons_new
                                         , colour = pod_ons_new
                                         , fill = pod_ons_new)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Average events per person") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expand = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0, plyr::round_any(max(opapp_month$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(opapp_month$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(op_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "opapp_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

# Mean outpatient appointments by month and cause of death - including all deaths
opapp_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(mean = mean(opapp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(opapp_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(mean = round(mean, 3))

write_csv(opapp_month_cod, here::here("output", "os_reports", "eol_service", "opapp_month_cod.csv"))

op_month_cod_plot <- ggplot(opapp_month_cod, aes(x = study_month, y = mean
                                         , group = codgrp
                                         , colour = codgrp
                                         , fill = codgrp)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Average events per person") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expand = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0, plyr::round_any(max(opapp_month_cod$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(opapp_month_cod$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(op_month_cod_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "opapp_month_cod_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

