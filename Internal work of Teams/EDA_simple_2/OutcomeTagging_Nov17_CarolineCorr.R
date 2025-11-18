## Tagging Call Outcomes ----

# load packages
library(tidyverse)
library(lubridate)

# read in data (considering all data now, not just after 3/16)
# note for reproducibility: 
# CAR data synthesized through CAR_data_import_R_CarolineCorr.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/CAR_data_import_R_CarolineCorr.R
car <- read_csv('data/combined_data.csv') |> 
       janitor::clean_names() |>
       mutate(activity_start_timestamp = ymd_hms(activity_start_timestamp),
              across(where(is.character), ~ na_if(.x, "N/A")),
              hour = hour(activity_start_timestamp)) 

# grab last activity for each contact session id
final_activities <-
car |>
  group_by(contact_session_id) |>
  mutate(
    last_activity = last(activity_name[!is.na(activity_name)], na_rm = TRUE)
  ) |>
  ungroup()

summary <-
car |>
  summarize(
    .by = contact_session_id,
    last_activity = last(activity_name[!is.na(activity_name)], na_rm = TRUE),
    last_ep =  last(ep_name[!is.na(ep_name)], na_rm = TRUE),
    last_agent = last(agent_name[!is.na(agent_name)], na_rm = TRUE),
    last_menu = last(flow_name[!is.na(flow_name)], na_rm = TRUE),
    last_activity_time =
      last(activity_start_timestamp[!is.na(activity_name)]),
    end_time = last(activity_start_timestamp),
    time_gap = end_time - last_activity_time
  )
# keeping two versions of summaries so that I can pull different info if need be

# tag outcomes
outcomes_types <-
summary |>
  mutate(
    outcome = case_when(
      # Caroline
      last_activity == 'ClosedQueueMenu' ~ 'Negative',
      last_activity == 'StaffDirectoryEnglishTransfer' ~ 'Positive',
      last_activity == 'LanguageSelectionMenu' ~ 'Negative',
      last_activity == 'ClinicVoicemailTransfer' & last_ep == 'Closed Queue Menu Telephony EP' ~ 'Negative',
      last_activity == 'ClinicVoicemailTransfer' & last_ep != 'Closed Queue Menu Telephony EP' ~ 'Positive',
      last_activity == 'MainMenu' ~ 'Negative',
      last_activity == 'DisconnectContact1' & last_ep == 'Closed Hours-Holidays Menu Telephony EP' ~ 'Neutral',
      last_activity == 'DisconnectContact1' & last_ep != 'Closed Hours-Holidays Menu Telephony EP' ~ 'Negative',
      last_activity == 'LegalMenu2' ~ 'Negative',
      last_activity == 'LegalServerScreenPop' & !is.na(last_agent) ~ 'Positive',
      last_activity == 'LegalServerScreenPop' & is.na(last_agent) ~ 'Neutral',
      last_activity == 'FrontDeskTransfer1' ~ 'Positive',
      last_activity == 'ClosedMenu' ~ 'Neutral',
      last_activity == 'FrontDeskTransfer2' ~ 'Positive',
      last_activity == 'OtherLegalMenu' ~ 'Negative',
      last_activity == 'StaffDirectorySpanishTransfer' ~ 'Positive',
      last_activity == 'FrontDeskTransfer' ~ 'Positive',
      last_activity == 'TenantMenu' ~ 'Negative',
      last_activity == 'CriminalRecordsVoicemailTransfer' ~ 'Positive',
      last_activity == 'OtherLegalOtherMenu' ~ 'Negative',
      last_activity == 'DivorceOrParentingMenu' ~ 'Negative',
      last_activity == 'QueueMenu1' ~ 'Negative',
      last_activity == 'LegalMenu1' ~ 'Negative',
      last_activity == 'SeniorsMenu' ~ 'Negative',
      last_activity == 'TenantDeterrenceMenu' ~ 'Negative',
      last_activity == 'HousingMenu' ~ 'Negative',
      last_activity == 'HIVVoicemailTransfer' ~ 'Positive',
      last_activity == 'PlayMOH300s' ~ 'Negative',
      last_activity == 'HIVMenu' ~ 'Negative',
      # Hailee
      last_activity == 'AddressFaxHoursMenu' ~ 'Positive',
      last_activity == 'PreTenantMenu' ~ 'Neutral',
      last_activity == 'FamilyMenu' ~ 'Negative',
      last_activity == 'FrontDeskTransfer3' ~ 'Positive',
      last_activity == 'ChildSupportMenu' ~ 'Neutral',
      last_activity == 'ImmigrationMenu' ~ 'Negative',
      last_activity == 'SeniorsADAPTMenu' ~ 'Negative',
      last_activity == 'OtherLegalPersonalInjuryMenu' ~ 'Neutral',
      last_activity == 'OtherLegalCriminalCaseMenu' ~ 'Neutral',
      last_activity == 'SuburbsOrCityMenu' ~ 'Negative',
      last_activity == 'SuburbanSeniorsMenu' ~ 'Negative',
      last_activity == 'SimpleDivorceMenu' ~ 'Neutral',
      last_activity == 'BenefitsMenu' ~ 'Negative',
      last_activity == 'ThankYouGoodbye' ~ 'Neutral',
      last_activity == 'IntakePreQueueMessage1' ~ 'Neutral',
      last_activity == 'SeniorsConfirmationMenu' ~ 'Negative',
      last_activity == 'VeteransBenefitsVoicemailTransfer' ~ 'Positive',
      last_activity == 'EmploymentMenu' ~ 'Negative',
      last_activity == 'DisconnectContact2' ~ 'Neutral',
      last_activity == 'TraffickingVoicemailTransfer' ~ 'Positive',
      last_activity == 'AppointmentMenu' ~ 'Negative',
      last_activity == 'SeniorNotCookCoMenu' ~ 'Neutral',
      last_activity == 'ImmigrationOtherMenu' ~ 'Negative',
      last_activity == 'HolidayPrompt' ~ 'Neutral',
      # Nikole
      last_activity == "GetLoggedInSubSeniorTenantSPAgents" ~ "Negative",
      last_activity == "SubSeniorConsumerQueue" ~ "Negative",
      last_activity == "SubSeniorHomeownerQueue" ~ "Negative",
      last_activity == "PlayCCBConfirmation" ~ "Negative",
      last_activity == "EducationSPQueue" ~ "Negative",
      last_activity == "SubSeniorTenantQueue" ~ "Negative",
      last_activity == "GetLoggedInOtherSubSeniorsAgents" ~ "Negative",
      last_activity == "ImmigrationSPQueue" ~ "Negative",
      last_activity == "GetLoggedInConsumerAgents" ~ "Negative",
      last_activity == "SubSeniorOtherQueue" ~ "Negative",
      last_activity == "GetLoggedInSubSeniorOtherSPAgents" ~ "Negative",
      last_activity == "ConfirmCallbackNumber" ~ "Negative",
      last_activity == "CollectCallbackNumber" ~ "Negative",
      last_activity == "GetLoggedInSubSeniorOtherAgents" ~ "Negative",
      last_activity == "CallbackRetry" ~ "Neutral",
      last_activity == "PlayErrorMessage" ~ "Negative",
      last_activity == "ReadANI" ~ "Negative",
      last_activity == "GetLoggedInSubSeniorTenantAgents" ~ "Negative",
      last_activity == "WorkersCompMenu" ~ "Neutral",
      last_activity == "DisconnectContact" ~ "Negative",
      last_activity == "HelpWithLegalorOtherReasonMenu" ~ "Negative",
      last_activity == "PreQueueMessage2" ~ "Negative",
      last_activity == "ComplimentOrComplaintMenu" ~ "Positive",
      last_activity == "MigrantVoicemailTransfer" ~ "Positive",
      last_activity == "DisconnectCallbackContact" ~ "Negative"
    )
  ) |>
  mutate(
    sub_type = case_when(
      # Caroline
      last_activity == 'ClosedQueueMenu' ~ 'Closed Queue',
      #last_activity == 'StaffDirectoryEnglishTransfer' ~ 'Positive',
      last_activity == 'LanguageSelectionMenu' ~ 'Abandoned',
      last_activity == 'ClinicVoicemailTransfer' & last_ep == 'Closed Queue Menu Telephony EP' ~ 'Closed Queue',
      last_activity == 'ClinicVoicemailTransfer' & last_ep != 'Closed Queue Menu Telephony EP' ~ 'Reached Voicemail',
      last_activity == 'MainMenu' ~ 'Abandoned',
      last_activity == 'DisconnectContact1' & last_ep == 'Closed Hours-Holidays Menu Telephony EP' ~ 'After Hours Call',
      last_activity == 'DisconnectContact1' & last_ep != 'Closed Hours-Holidays Menu Telephony EP' ~ 'Closed Queue',
      last_activity == 'LegalMenu2' ~ 'Abandoned',
      last_activity == 'LegalServerScreenPop' & !is.na(last_agent) ~ 'Reached Agent',
      last_activity == 'LegalServerScreenPop' & is.na(last_agent) ~ 'Abandoned',
      last_activity == 'FrontDeskTransfer1' ~ 'Agent Reached',
      last_activity == 'ClosedMenu' ~ 'After Hours Call',
      last_activity == 'FrontDeskTransfer2' ~ 'Agent Reached',
      last_activity == 'OtherLegalMenu' ~ 'Abandoned',
      #last_activity == 'StaffDirectorySpanishTransfer' ~ 'Positive',
      last_activity == 'FrontDeskTransfer' ~ 'Agent Reached',
      last_activity == 'TenantMenu' ~ 'Abandoned',
      last_activity == 'CriminalRecordsVoicemailTransfer' ~ 'Reached Voicemail',
      last_activity == 'OtherLegalOtherMenu' ~ 'Abandoned',
      last_activity == 'DivorceOrParentingMenu' ~ 'Abandoned',
      last_activity == 'QueueMenu1' ~ 'Abandoned',
      last_activity == 'LegalMenu1' ~ 'Abandoned',
      last_activity == 'SeniorsMenu' ~ 'Abandoned',
      #last_activity == 'TenantDeterrenceMenu' ~ 'Negative',
      last_activity == 'HousingMenu' ~ 'Abandoned',
      last_activity == 'HIVVoicemailTransfer' ~ 'Reached Voicemail',
      last_activity == 'PlayMOH300s' ~ 'Abandoned',
      last_activity == 'HIVMenu' ~ 'Abandoned',
      # Hailee
      #last_activity == 'AddressFaxHoursMenu' ~ 'Positive',
      last_activity == 'PreTenantMenu' ~ 'Issue not served by LAC',
      last_activity == 'FamilyMenu' ~ 'Abandoned',
      last_activity == 'FrontDeskTransfer3' ~ 'Agent Reached',
      last_activity == 'ChildSupportMenu' ~ 'Issue not served by LAC',
      last_activity == 'ImmigrationMenu' ~ 'Abandoned',
      last_activity == 'SeniorsADAPTMenu' ~ 'Abandoned',
      last_activity == 'OtherLegalPersonalInjuryMenu' ~ 'Issue not served by LAC',
      last_activity == 'OtherLegalCriminalCaseMenu' ~ 'Issue not served by LAC',
      last_activity == 'SuburbsOrCityMenu' ~ 'Abandoned',
      last_activity == 'SuburbanSeniorsMenu' ~ 'Abandoned',
      last_activity == 'SimpleDivorceMenu' ~ 'Issue not served by LAC',
      last_activity == 'BenefitsMenu' ~ 'Abandoned',
      last_activity == 'ThankYouGoodbye' ~ 'Issue not served by LAC',
      last_activity == 'IntakePreQueueMessage1' ~ 'Issue not served by LAC',
      last_activity == 'SeniorsConfirmationMenu' ~ 'Abandoned',
      last_activity == 'VeteransBenefitsVoicemailTransfer' ~ 'Reached Voicemail',
      last_activity == 'EmploymentMenu' ~ 'Abandoned',
      last_activity == 'DisconnectContact2' ~ 'Issue not served by LAC',
      last_activity == 'TraffickingVoicemailTransfer' ~ 'Reached Voicemail',
      last_activity == 'AppointmentMenu' ~ 'Abandoned',
      last_activity == 'SeniorNotCookCoMenu' ~ 'Issue not served by LAC',
      last_activity == 'ImmigrationOtherMenu' ~ 'Abandoned',
      last_activity == 'HolidayPrompt' ~ 'After Hours Call',
      # Nikole
      #last_activity == "GetLoggedInSubSeniorTenantSPAgents" ~ "Negative",
      last_activity == "SubSeniorConsumerQueue" ~ "Abandoned",
      last_activity == "SubSeniorHomeownerQueue" ~ "Abandoned",
      #last_activity == "PlayCCBConfirmation" ~ "Negative",
      last_activity == "EducationSPQueue" ~ "Abandoned",
      last_activity == "SubSeniorTenantQueue" ~ "Abandoned",
      #last_activity == "GetLoggedInOtherSubSeniorsAgents" ~ "Negative",
      last_activity == "ImmigrationSPQueue" ~ "Abandoned",
      #last_activity == "GetLoggedInConsumerAgents" ~ "Negative",
      last_activity == "SubSeniorOtherQueue" ~ "Abandoned",
      #last_activity == "GetLoggedInSubSeniorOtherSPAgents" ~ "Negative",
      last_activity == "ConfirmCallbackNumber" ~ "Abandoned",
      last_activity == "CollectCallbackNumber" ~ "Abandoned",
      #last_activity == "GetLoggedInSubSeniorOtherAgents" ~ "Negative",
      last_activity == "CallbackRetry" ~ "Failed Courtesy Callback",
      last_activity == "PlayErrorMessage" ~ "Abandoned",
      #last_activity == "ReadANI" ~ "Negative",
      #last_activity == "GetLoggedInSubSeniorTenantAgents" ~ "Negative",
      last_activity == "WorkersCompMenu" ~ "Issue not served by LAC",
      #last_activity == "DisconnectContact" ~ "Negative",
      last_activity == "HelpWithLegalorOtherReasonMenu" ~ "Abandoned",
      last_activity == "PreQueueMessage2" ~ "Abandoned",
      #last_activity == "ComplimentOrComplaintMenu" ~ "Positive",
      last_activity == "MigrantVoicemailTransfer" ~ "Reached Voicemail",
      last_activity == "DisconnectCallbackContact" ~ "Failed Courtesy Callback"
    )) 
  

