skim(report)

report %>% filter(is.na(first_reminder_complete_timestamp))

patients %>% distinct(pid)

#report %>% select(pid, org_name, status, contains("covered_days"), consent_date) %>%
  #distinct() %>% 
 # summarize(zdays_pre = sum(covered_days_pre_ibis == 0), 
        #    zdays_post = sum(covered_days_post_ibis == 0),
        #    nadays = (is.na(covered_days_post_ibis)),
        #  )
# days_since_coverage_start

events_full %>% summarize(sum(pre_ibis_inpatient), sum(post_ibis_inpatient))

#events_full %>% filter(org_name == "Unicare - Study") %>%
 # summarize(unicare_post_admits = sum(admits_count))

events_full %>% mutate(days_since_start = event_start_date - coverage_start_date) %>%
  mutate(days_since_ibis = event_start_date - ibis_coverage_start_date) %>%
  select(coverage_start_date, event_start_date, days_since_start, ibis_coverage_start_date, days_since_ibis)

  
# days_since_ibis_start