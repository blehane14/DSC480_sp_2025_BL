## Read data

## Change the working directory and file paths as appropriate

setwd("~/Dropbox/Projects/Senscio/DSC480_sp_2025")

library(tidyverse)

# Actual data files
events <- read_csv("../data/2024_11_25_utilization_events.csv")
report <- read_csv("../data/2024_11_25_utilization_report.csv")

# Toy data files- these have the row values for each column shuffled separately.
#events <- read_csv("../data/events_toy.csv")
#report <- read_csv("../data/report_toy.csv")


## A bit of cleaning
events_full <- events %>%
  mutate_at(vars(contains("timestamp")), ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate_at(vars(contains("timestamp")), ~as_date(.x)) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
  mutate(across(event_type, ~ str_replace(.x, " ", "_"))) |>
  mutate(across(event_type, ~as.factor(.x)))


report_full <- report %>%
  mutate_at(vars(contains("timestamp")), ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate_at(vars(contains("timestamp")), ~as_date(.x)) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
    mutate(across(event_type, ~ str_replace(.x, " ", "_"))) |>
  mutate(across(event_type, ~as.factor(.x)))

## add comment
