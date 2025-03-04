## Read data

## Change the working directory and file paths as appropriate

#setwd("~/Dropbox/Projects/Senscio/DSC480_sp_2025")

library(tidyverse)

# Actual data files
events <- read_csv("../data/2024_11_25_utilization_events.csv")
report <- read_csv("../data/2024_11_25_utilization_report.csv")

# Toy data files- these have the row values for each column shuffled separately.
<<<<<<< HEAD
events <- read_csv("events_toy.csv")
report <- read_csv("report_toy.csv")
=======
#events <- read_csv("../data/events_toy.csv")
#report <- read_csv("../data/report_toy.csv")
>>>>>>> 415b3eceb57d1bfeb34406a1ef1642e842742e17

str(events)
head(events$event_start_timestamp)

## A bit of cleaning
events_full <- events %>%
  mutate_at(vars(contains("timestamp")), ~ifelse(is.numeric(.x), as.POSIXct(.x, origin = "1970-01-01"), as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate_at(vars(contains("timestamp")), ~as_date(.x)) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
  mutate(across(event_type, ~ str_replace(.x, " ", "_"))) %>%
  mutate(across(event_type, ~as.factor(.x)))

report_full <- report %>%
  mutate_at(vars(contains("timestamp")), ~ifelse(is.numeric(.x), as.POSIXct(.x, origin = "1970-01-01"), as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate_at(vars(contains("timestamp")), ~as_date(.x)) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
  mutate(across(event_type, ~ str_replace(.x, " ", "_"))) %>%
  mutate(across(event_type, ~as.factor(.x)))

## add comment
