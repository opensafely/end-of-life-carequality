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

# Deaths in period --------------------------------------------------------

# Number of deaths by month and place of death - including all deaths
deaths_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n()) %>%
              mutate(pod_ons_new = "All")) %>%
  mutate(count = plyr::round_any(count, 10))

write_csv(deaths_month, here::here("output", "os_reports", "eol_service", "deaths_month.csv"))

deaths_month_plot <- ggplot(deaths_month, aes(x = study_month, y = count
                                              , group = pod_ons_new
                                              , colour = pod_ons_new
                                              , fill = pod_ons_new)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Number of deaths") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expan = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0, plyr::round_any(max(deaths_month$count)
                                                     , 20000, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(deaths_month$count)
                                                      , 20000, f = ceiling)
                                    , 20000)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(deaths_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "deaths_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

# Use of medications for symptom management  -------------------------------------------

# Medication use by place of death - including all deaths
eol_med_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(mean = mean(eol_med_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(eol_med_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All")) %>% 
  mutate(mean = round(mean,3))

write_csv(eol_med_month, here::here("output", "os_reports", "eol_service", "eol_med_month.csv"))

eol_med_month_plot <- ggplot(eol_med_month, aes(x = study_month, y = mean
                                      , group = pod_ons_new
                                      , colour = pod_ons_new
                                      , fill = pod_ons_new)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Average number of medications taken per person") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expand = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0,
                                  plyr::round_any(max(eol_med_month$mean)
                                                  , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(eol_med_month$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(eol_med_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "eol_med_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

# Medication use by month and cause of death

eol_med_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(mean = mean(eol_med_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(eol_med_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(mean = round(mean, 3))

write_csv(eol_med_month_cod, here::here("output", "os_reports", "eol_service", "eol_med_month_cod.csv"))

eol_med_month_cod_plot <- ggplot(eol_med_month_cod, aes(x = study_month, y = mean
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
                     , limits = c(0, plyr::round_any(max(eol_med_month_cod$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(eol_med_month_cod$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(eol_med_month_cod_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "eol_med_month_cod_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))
