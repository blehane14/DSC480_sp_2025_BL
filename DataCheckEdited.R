# Data Check Assignment

# Libraries in use
library(dbplyr)

head(events_full)
summary(events_full)

#______

## Overall counts without ER
# All unique PID values
unique_pid_count <- events_full %>%
  summarise(unique_pids = n_distinct(pid))

unique_pid_count

#______

# Number of events via type
event_type_count <- events_full %>%
  group_by(event_type) %>%
  summarise(count = n())

event_type_count

#______

# Median of coverage days
coverage_duration_stats <- events_full %>%
  summarise(mean_coverage_duration = mean(coverage_duration_days, na.rm = TRUE),
            median_coverage_duration = median(coverage_duration_days, na.rm = TRUE))

coverage_duration_stats

#______

# Number of different conditions occurrences 
condition_counts <- events_full %>%
  select(alzheimers:non_ibis_observation_admission_days) %>%
  summarise_all(~ sum(!is.na(.)))

condition_counts

#______

## Unicare study 

# Filter for records where org_name == 'Unicare - Study'
unicare_events_full <- events_full %>% filter(org_name == "Unicare - Study")

# Creating patients df
patients <- data.frame(pid = unique(unicare_events_full$pid))

# Calculating pre-IBIS admissions count, coverage days, and total members
pre_ibis_admissions_count <- unicare_events_full %>% 
  filter(event_start_date <= ibis_coverage_start_date & event_start_date >= coverage_start_date & event_type == "inpatient") %>%
  group_by(pid) %>% 
  summarize(admit_count = n(), coverage_days = sum(coverage_duration_days, na.rm = TRUE)) %>% 
  right_join(patients, by = "pid") %>% 
  mutate(admit_count = ifelse(is.na(admit_count), 0, admit_count), coverage_days = ifelse(is.na(coverage_days), 0, coverage_days))

# Calculate post-IBIS admissions count, coverage days, and total members
post_ibis_admissions_count <- unicare_events_full %>% 
  filter(event_start_date > ibis_coverage_start_date & event_start_date <= ibis_coverage_end_date & event_type == "inpatient") %>%
  group_by(pid) %>% 
  summarize(admit_count = n(), coverage_days = sum(coverage_duration_days, na.rm = TRUE)) %>% 
  right_join(patients, by = "pid") %>% 
  mutate(admit_count = ifelse(is.na(admit_count), 0, admit_count), coverage_days = ifelse(is.na(coverage_days), 0, coverage_days))

# Calculate during-IBIS admissions count, coverage days, and total members
during_ibis_admissions_count <- unicare_events_full %>% 
  filter(event_start_date > coverage_start_date & event_start_date <= ibis_coverage_start_date & event_start_date <= coverage_end_date & event_type == "inpatient") %>%
  group_by(pid) %>% 
  summarize(admit_count = n(), coverage_days = sum(coverage_duration_days, na.rm = TRUE)) %>% 
  right_join(patients, by = "pid") %>% 
  mutate(admit_count = ifelse(is.na(admit_count), 0, admit_count), coverage_days = ifelse(is.na(coverage_days), 0, coverage_days))

# Summarize pre-treatment Unicare admissions count, coverage days, and total members
pre_unicare_summary <- pre_ibis_admissions_count %>%
  summarize(unicare_pre_admits = sum(admit_count), total_pre_coverage_days = sum(coverage_days), total_pre_members = n())

# Summarize during-treatment Unicare admissions count, coverage days, and total members
during_unicare_summary <- during_ibis_admissions_count %>%
  summarize(unicare_during_admits = sum(admit_count), total_during_coverage_days = sum(coverage_days), total_during_members = n())

# Summarize post-treatment Unicare admissions count, coverage days, and total members
post_unicare_summary <- post_ibis_admissions_count %>%
  summarize(unicare_post_admits = sum(admit_count), total_post_coverage_days = sum(coverage_days), total_post_members = n())

# Combine the results into a summary table
summary_table <- data.frame(
  Treatment_Period = c("Pre-Treatment", "During-Treatment", "Post-Treatment"),
  Unicare_Admissions = c(pre_unicare_summary$unicare_pre_admits, during_unicare_summary$unicare_during_admits, post_unicare_summary$unicare_post_admits),
  Total_Coverage_Days = c(pre_unicare_summary$total_pre_coverage_days, during_unicare_summary$total_during_coverage_days, post_unicare_summary$total_post_coverage_days),
  Total_Members = c(pre_unicare_summary$total_pre_members, during_unicare_summary$total_during_members, post_unicare_summary$total_post_members)
)

# Print the summary table
print(summary_table)

#______

## IBIS member averages (pre-treatment)
events_full %>% filter(event_start_date  < ibis_coverage_start_date) %>% filter(event_start_date > coverage_start_date) %>%
  summarize(sum(pre_ibis_inpatient), sum(post_ibis_inpatient))

events_full %>% filter(event_start_date  >= ibis_coverage_start_date) %>% filter(event_start_date < ibis_coverage_end_date) %>% 
  summarize(sum(pre_ibis_inpatient), sum(post_ibis_inpatient))

#______

## All IBIS member averages (during-treatment)
# Filter All Ibis Member Average (Pre-Treatment)
pre_treatment_summary <- events_full %>%
  filter(event_start_date < ibis_coverage_start_date & event_start_date > coverage_start_date) %>%
  summarize(
    sum_pre_ibis_inpatient = sum(pre_ibis_inpatient, na.rm = TRUE),
    sum_post_ibis_inpatient = sum(post_ibis_inpatient, na.rm = TRUE),
    total_members_pre = n_distinct(pid),
    total_coverage_days_pre = sum(coverage_duration_days, na.rm = TRUE)
  )

# Filter for All Ibis Member Average (In Treatment)
in_treatment_summary <- events_full %>%
  filter(event_start_date >= ibis_coverage_start_date & event_start_date < ibis_coverage_end_date) %>%
  summarize(
    sum_pre_ibis_inpatient = sum(pre_ibis_inpatient, na.rm = TRUE),
    sum_post_ibis_inpatient = sum(post_ibis_inpatient, na.rm = TRUE),
    total_members_in_treatment = n_distinct(pid),
    total_coverage_days_in_treatment = sum(coverage_duration_days, na.rm = TRUE)
  )

# Combine the results into a summary table
summary_table <- data.frame(
  Treatment_Period = c("Pre-Treatment", "In-Treatment"),
  Sum_Pre_Ibis_Inpatient = c(pre_treatment_summary$sum_pre_ibis_inpatient, in_treatment_summary$sum_pre_ibis_inpatient),
  Sum_Post_Ibis_Inpatient = c(pre_treatment_summary$sum_post_ibis_inpatient, in_treatment_summary$sum_post_ibis_inpatient),
  Total_Members = c(pre_treatment_summary$total_members_pre, in_treatment_summary$total_members_in_treatment),
  Total_Coverage_Days = c(pre_treatment_summary$total_coverage_days_pre, in_treatment_summary$total_coverage_days_in_treatment)
)

# Print the summary table
print(summary_table)

#______

## Post IBIS admissions
# Filter for records where org_name == 'Unicare - Study'
unicare_events_full <- events_full %>% filter(org_name == "Unicare - Study")

# Create a patients dataframe
patients <- data.frame(pid = unique(unicare_events_full$pid))

# Calculate post-IBIS admissions count, coverage days, and total members
post_ibis_admissions_count <- unicare_events_full %>% 
  filter(event_start_date > ibis_coverage_start_date & event_start_date <= ibis_coverage_end_date & event_type == "inpatient") %>%
  group_by(pid) %>% 
  summarize(admit_count = n(), coverage_days = sum(coverage_duration_days, na.rm = TRUE)) %>% 
  right_join(patients, by = "pid") %>% 
  mutate(admit_count = ifelse(is.na(admit_count), 0, admit_count), coverage_days = ifelse(is.na(coverage_days), 0, coverage_days))

# Summarize post-treatment Unicare admissions count, coverage days, and total members
post_unicare_summary <- post_ibis_admissions_count %>%
  summarize(unicare_post_admits = sum(admit_count), total_post_coverage_days = sum(coverage_days), total_post_members = n())

# Print the summary
print(post_unicare_summary)

#______

# How many ibis_cov_start date = 0 and then change to the start date or a high number --- all in the pre period
# Define the pre-treatment period
pre_treatment_events <- unicare_events_full %>% 
  filter(event_start_date < ibis_coverage_start_date & event_start_date > coverage_start_date)

# Replace `ibis_coverage_start_date` of 0 with a high number (e.g., 99999)
pre_treatment_events <- pre_treatment_events %>%
  mutate(ibis_coverage_start_date = ifelse(ibis_coverage_start_date == 0, 99999, ibis_coverage_start_date))

# Calculate pre-IBIS admissions count, coverage days, and total members
pre_ibis_admissions_count <- pre_treatment_events %>% 
  filter(event_start_date <= ibis_coverage_start_date & event_type == "inpatient") %>%
  group_by(pid) %>% 
  summarize(admit_count = n(), coverage_days = sum(coverage_duration_days, na.rm = TRUE)) %>% 
  right_join(patients, by = "pid") %>% 
  mutate(admit_count = ifelse(is.na(admit_count), 0, admit_count), coverage_days = ifelse(is.na(coverage_days), 0, coverage_days))

# Summarize pre-treatment Unicare admissions count, coverage days, and total members
pre_unicare_summary <- pre_ibis_admissions_count %>%
  summarize(unicare_pre_admits = sum(admit_count), total_pre_coverage_days = sum(coverage_days), total_pre_members = n())

# Print the pre-treatment summary
print(pre_unicare_summary)

#______

# count total admissions pre and post for both the unicare-study and the whole group
# Define the pre-IBIS and post-IBIS periods
pre_ibis_period <- events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period <- events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Count total admissions for the entire group (pre and post)
total_admissions_pre <- pre_ibis_period %>% 
  filter(event_type == "inpatient") %>% 
  summarize(total_admissions = n())

total_admissions_post <- post_ibis_period %>% 
  filter(event_type == "inpatient") %>% 
  summarize(total_admissions = n())

# Filter for records where org_name == 'Unicare - Study'
unicare_events_full <- events_full %>% filter(org_name == "Unicare - Study")

# Define the pre-IBIS and post-IBIS periods for Unicare
pre_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Count total admissions for Unicare participants (pre and post)
total_admissions_pre_unicare <- pre_ibis_period_unicare %>% 
  filter(event_type == "inpatient") %>% 
  summarize(total_admissions = n())

total_admissions_post_unicare <- post_ibis_period_unicare %>% 
  filter(event_type == "inpatient") %>% 
  summarize(total_admissions = n())

# Combine the results into a summary table
summary_table <- data.frame(
  Group = c("Whole Group", "Whole Group", "Unicare Study", "Unicare Study"),
  Period = c("Pre-IBIS", "Post-IBIS", "Pre-IBIS", "Post-IBIS"),
  Total_Admissions = c(total_admissions_pre$total_admissions, total_admissions_post$total_admissions, 
                       total_admissions_pre_unicare$total_admissions, total_admissions_post_unicare$total_admissions)
)

# Print the summary table
print(summary_table)

#______

# compare days: number of days total of coverage for unicare-study vs everybody pre and post treatment
# Define the pre-IBIS and post-IBIS periods
pre_ibis_period <- events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period <- events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Calculate total coverage days for the entire group (pre and post)
total_coverage_days_pre <- pre_ibis_period %>% 
  summarize(total_coverage_days = sum(coverage_duration_days, na.rm = TRUE))

total_coverage_days_post <- post_ibis_period %>% 
  summarize(total_coverage_days = sum(coverage_duration_days, na.rm = TRUE))

# Filter for records where org_name == 'Unicare - Study'
unicare_events_full <- events_full %>% filter(org_name == "Unicare - Study")

# Define the pre-IBIS and post-IBIS periods for Unicare
pre_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Calculate total coverage days for Unicare participants (pre and post)
total_coverage_days_pre_unicare <- pre_ibis_period_unicare %>% 
  summarize(total_coverage_days = sum(coverage_duration_days, na.rm = TRUE))

total_coverage_days_post_unicare <- post_ibis_period_unicare %>% 
  summarize(total_coverage_days = sum(coverage_duration_days, na.rm = TRUE))

# Combine the results into a summary table
summary_table <- data.frame(
  Group = c("Whole Group", "Whole Group", "Unicare Study", "Unicare Study"),
  Period = c("Pre-IBIS", "Post-IBIS", "Pre-IBIS", "Post-IBIS"),
  Total_Coverage_Days = c(total_coverage_days_pre$total_coverage_days, total_coverage_days_post$total_coverage_days, 
                          total_coverage_days_pre_unicare$total_coverage_days, total_coverage_days_post_unicare$total_coverage_days)
)

# Print the summary table
print(summary_table)


#______


# add all the coverage days and also do counts by patient ultimately find the inpatient events for each patient

# Define the pre-IBIS and post-IBIS periods
pre_ibis_period <- events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period <- events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Calculate total coverage days and inpatient events by patient for the entire group (pre and post)
total_coverage_pre <- pre_ibis_period %>% 
  group_by(pid) %>% 
  summarize(coverage_days = sum(coverage_duration_days, na.rm = TRUE), inpatient_events = sum(event_type == "inpatient"))

total_coverage_post <- post_ibis_period %>% 
  group_by(pid) %>% 
  summarize(coverage_days = sum(coverage_duration_days, na.rm = TRUE), inpatient_events = sum(event_type == "inpatient"))

# Filter for records where org_name == 'Unicare - Study'
unicare_events_full <- events_full %>% filter(org_name == "Unicare - Study")

# Define the pre-IBIS and post-IBIS periods for Unicare
pre_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date < ibis_coverage_start_date)
post_ibis_period_unicare <- unicare_events_full %>% filter(event_start_date >= ibis_coverage_start_date)

# Calculate total coverage days and inpatient events by patient for Unicare participants (pre and post)
total_coverage_pre_unicare <- pre_ibis_period_unicare %>% 
  group_by(pid) %>% 
  summarize(coverage_days = sum(coverage_duration_days, na.rm = TRUE), inpatient_events = sum(event_type == "inpatient"))

total_coverage_post_unicare <- post_ibis_period_unicare %>% 
  group_by(pid) %>% 
  summarize(coverage_days = sum(coverage_duration_days, na.rm = TRUE), inpatient_events = sum(event_type == "inpatient"))

# Combine the results into a summary table for the entire group
summary_table_whole <- data.frame(
  Group = c("Whole Group Pre-IBIS", "Whole Group Post-IBIS"),
  Total_Coverage_Days = c(sum(total_coverage_pre$coverage_days), sum(total_coverage_post$coverage_days)),
  Total_Inpatient_Events = c(sum(total_coverage_pre$inpatient_events), sum(total_coverage_post$inpatient_events)),
  Total_Members = c(n_distinct(total_coverage_pre$pid), n_distinct(total_coverage_post$pid))
)

# Combine the results into a summary table for Unicare study participants
summary_table_unicare <- data.frame(
  Group = c("Unicare Study Pre-IBIS", "Unicare Study Post-IBIS"),
  Total_Coverage_Days = c(sum(total_coverage_pre_unicare$coverage_days), sum(total_coverage_post_unicare$coverage_days)),
  Total_Inpatient_Events = c(sum(total_coverage_pre_unicare$inpatient_events), sum(total_coverage_post_unicare$inpatient_events)),
  Total_Members = c(n_distinct(total_coverage_pre_unicare$pid), n_distinct(total_coverage_post_unicare$pid))
)

# Print the summary tables
print(summary_table_whole)
print(summary_table_unicare)