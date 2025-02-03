# Converting the date format (using both numeric timestamps and string timestamps)
events_full <- events %>%
  mutate(across(contains("timestamp"),
                ~ifelse(is.numeric(.x), 
                        as.POSIXct(.x, origin = "1970-01-01", tz = "UTC"),  # Numeric timestamps
                        as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))) %>%
  mutate(across(contains("timestamp"), ~as_date(.x))) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
  mutate(across(event_type, ~ str_replace(.x, " ", "_"))) %>%
  mutate(across(event_type, ~as.factor(.x)))

report_full <- report %>%
  mutate(across(contains("timestamp"),
                ~ifelse(is.numeric(.x), 
                        as.POSIXct(.x, origin = "1970-01-01", tz = "UTC"),  # Numeric timestamps
                        as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))) %>%
  mutate(across(contains("timestamp"), ~as_date(.x))) %>%
  rename_with(~str_replace(., "timestamp", "date"), contains("timestamp")) %>%
  mutate(across(event_type, ~ str_replace(.x, " ", "_"))) %>%
  mutate(across(event_type, ~as.factor(.x)))

# Checking results
head(report_full)
head(events_full)

# Check the column types of the needed variables
str(events_full)

# Making sure the dated columns are date class variables
events_full <- events_full %>%
  mutate(
    event_start_date = as.Date(event_start_date),
    coverage_start_date = as.Date(coverage_start_date),
    ibis_coverage_start_date = as.Date(ibis_coverage_start_date),
    ibis_coverage_end_date = as.Date(ibis_coverage_end_date))

# Creating the binary columns
events_full <- events_full %>%
  mutate(
    pre_ibis = ifelse(event_start_date >= coverage_start_date & event_start_date < ibis_coverage_start_date, 1, 0),
    during_ibis = ifelse(event_start_date >= ibis_coverage_start_date & event_start_date <= ibis_coverage_end_date, 1, 0))

# Checking the output
# We should be able to see the start dates and the correlating binary values
head(events_full %>% select(event_start_date, pre_ibis, during_ibis))
