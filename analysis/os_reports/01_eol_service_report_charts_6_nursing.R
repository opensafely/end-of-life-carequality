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


# Community nurse contacts---------------------------------------------

# Mean number of community nurse contacts by month and place of death - including all deaths
nursing_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(mean = mean(nursing_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(nursing_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All"))

write_csv(nursing_month, here::here("output", "os_reports", "eol_service", "nursing_month.csv"))

nursing_month_plot <- ggplot(nursing_month, aes(x = study_month, y = mean
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
                     , limits = c(0, plyr::round_any(max(nursing_month$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(nursing_month$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(nursing_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "nursing_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

# Mean community nursing interactions by month and cause of death
nursing_month_cod <- df %>%
  group_by(study_month, codgrp) %>%
  summarise(mean = mean(nursing_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(nursing_1m, na.rm = TRUE)) %>%
              mutate(codgrp = "All")) %>%
  mutate(mean = round(mean, 3))

write_csv(nursing_month_cod, here::here("output", "os_reports", "eol_service", "nursing_month_cod.csv"))

nursing_month_cod_plot <- ggplot(nursing_month_cod, aes(x = study_month, y = mean
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
                     , limits = c(0, plyr::round_any(max(nursing_month_cod$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(nursing_month_cod$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(nursing_month_cod_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "nursing_month_cod_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))
