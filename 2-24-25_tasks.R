# Tasks 2-24-25

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)

# Load the CSV files
utilization_events <- read.csv("2025_02_18_utilization_events.csv")
utilization_report <- read.csv("2025_02_18_utilization_report.csv")

# Total inpatient admissions
total_pre_ibis_admissions <- sum(utilization_report$total_pre_ibis_inpatient, na.rm = TRUE)
total_ibis_admissions <- sum(utilization_report$total_ibis_inpatient, na.rm = TRUE)
total_post_ibis_admissions <- sum(utilization_report$total_post_ibis_inpatient, na.rm = TRUE)

# Total patient-days
total_pre_ibis_days <- sum(utilization_report$covered_days_pre_ibis, na.rm = TRUE)
total_ibis_days <- sum(utilization_report$covered_days_on_ibis, na.rm = TRUE)
total_post_ibis_days <- sum(utilization_report$covered_days_post_ibis, na.rm = TRUE)

# Converting patient days to years
total_pre_ibis_years <- total_pre_ibis_days / 365
total_ibis_years <- total_ibis_days / 365
total_post_ibis_years <- total_post_ibis_days / 365

# Admissions per 1000 years
admissions_per_1000_pre <- (total_pre_ibis_admissions / total_pre_ibis_years) * 1000
admissions_per_1000_ibis <- (total_ibis_admissions / total_ibis_years) * 1000
admissions_per_1000_post <- (total_post_ibis_admissions / total_post_ibis_years) * 1000

# T test for admissions of 1000 years
t_test_result <- t.test(
  (utilization_report$total_pre_ibis_inpatient / (utilization_report$covered_days_pre_ibis / 365) * 1000),
  (utilization_report$total_ibis_inpatient / (utilization_report$covered_days_on_ibis / 365) * 1000),
  paired = TRUE)

# Log reg for probability of admission
utilization_report$ibis_admission_flag <- as.factor(ibis_admission)
logistic_model <- glm(ibis_admission_flag ~ pre_admission, data = utilization_report, family = binomial)


# Need to work on Wilcoxon and other test