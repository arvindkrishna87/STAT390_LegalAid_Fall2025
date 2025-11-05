## Summary statistics for outcomes of family menu ----

# load packages
library(tidyverse)
library(lubridate)

# read in data
# note for reproducibility: 
# CAR data synthesized through CAR_data_import_R_CarolineCorr.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/CAR_data_import_R_CarolineCorr.R
car <- read_csv('data/combined_data.csv') |> janitor::clean_names() |>
  mutate(activity_start_timestamp = ymd_hms(activity_start_timestamp),
         across(where(is.character), ~ na_if(.x, "N/A")),
         hour = hour(activity_start_timestamp)) |>
  filter(activity_start_timestamp >= '2025-03-16 00:00:00') 

# only looking at contact session ids that visited the family menu after March 16, 2025
family_ids <-
  car |>
  filter(ep_name == 'Legal Family Menu Telephony EP') |>
  distinct(contact_session_id) |>
  pull(contact_session_id)

family_sessions <-
car |>
  filter(contact_session_id %in% family_ids) |>
  arrange(contact_session_id, activity_start_timestamp)

# get an idea of termination reasons
family_sessions |>
  count(termination_reason) |>
  print(n = 21)

# grab final menu traversed to
family_sessions <-
family_sessions |>
  group_by(contact_session_id) |>
  mutate(
    last_menu = last(activity_name, order_by = activity_start_timestamp, na_rm = TRUE),
    last_reason = last(termination_reason, order_by = activity_start_timestamp, na_rm = TRUE)
  ) |>
  ungroup()

# list of where calls involving family menu ended up...filter out those with 
# nothing to do with family menu/offshoots
final_menus <-
family_sessions |>
  count(last_menu, sort = TRUE) 

relevant_menus <- c('ClosedQueueMenu', 'ClinicVoicemailTransfer',
                    'DivorceOrParentingMenu', 'ChildSupportMenu',
                    'FamilyMenu', 'SimpleDivorceMenu')

family_sessions <-
family_sessions |>
  filter(last_menu %in% relevant_menus) 

negative <- c('AGENT_UNAVAILABLE',
              'AGENT_BUSY',
              'NO_ANSWER_FROM_AGENT',
              'MEDIA_MANAGER_INTERNAL_ERROR',
              'System disconnected the contact') 
positive <- c('Agent Left', 'AGENT_ENDS')
neutral <- c('Participant Invite timer expired',
             'NO_ANSWER_FROM_CUSTOMER',
             'CUSTOMER_BUSY',
             'MAX_CALLBACK_RETRY_LIMIT_REACHED',
             'CUSTOMER_UNAVAILABLE',
             'USER_DECLINED',
             'USER_BUSY',
             'USER_UNAVAILABLE',
             'NO_ANSWER_USER',
             'Queue Timeout')  

# create outcomes table
family_outcomes <-
family_sessions |>
  summarize(
    .by = contact_session_id,
    positive = any((!is.na(agent_name) & termination_reason == "Customer Left") |
                (termination_reason %in% positive)),
    neutral = any(termination_reason %in% neutral |
                  str_detect(activity_name, regex("ChildSupportMenu|SimpleDivorceMenu", ignore_case = TRUE))),
    negative = any(is.na(agent_name) & termination_reason == "Customer Left" |
                     (termination_reason %in% negative))
  ) |>
  mutate(
    outcome = case_when(
                neutral  ~ 'Neutral',
                positive ~ 'Postive',
                negative ~ 'Negative'
    )
  ) |>
  count(outcome) |>
  mutate(
    pct = (n / sum(n)) * 100
  ) |>
  knitr::kable(
    caption = 'Outcome Breakdown: Family Menu',
    col.names = c('Outcome Type', 'N', '%'),
    digits = 2
  )

save(family_outcomes, file = 'family_outcomes.rda')
