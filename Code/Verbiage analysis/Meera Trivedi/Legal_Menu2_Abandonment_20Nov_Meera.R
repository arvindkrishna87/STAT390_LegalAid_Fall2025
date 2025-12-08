#LEGAL MENU 2 ABANDONMENT


library(tidyverse)
library(writexl)
library(janitor)
library(readxl)
library(hms)
library(scales)
library(gt)

#### LEGAL MENU 2--------

#krish's import code from python
krish_file_python <- read_csv("/Users/meeratrivedi/Downloads/STAT 390/krish_cleaning.csv") %>% 
  clean_names()

krish_full_file <- read_csv("/Users/meeratrivedi/Downloads/STAT 390/krish_cleaning_full.csv") %>% 
  clean_names()

#rows that are in this EP
legal <- krish_full_file %>% filter(ep_name == "Legal Menu Telephony EP")

legal2 <- krish_full_file %>% filter(activity_name == "LegalMenu2")

#all calls that have a row that says legal menu 2
##aka all calls that pass through the legal menu 2 at some point
legal_menu2 <- krish_full_file %>%
  semi_join(legal2, by = "contact_session_id")

#make call duration variable
legal_menu2 <- legal_menu2 %>% 
  mutate(time = as_hms(activity_start_timestamp)) %>% 
  arrange(activity_start_timestamp) %>% 
  mutate(time_diff = time-lag(time)) %>% 
  mutate(time_diff = as.numeric(time_diff)) %>% 
  mutate(call_duration = sum(time_diff, na.rm = TRUE)) 

#slice out last row of each legal menu 2 call
legal2_last_rows <- legal_menu2 %>%
  group_by(contact_session_id) %>%
  slice_tail(n = 1) %>%
  ungroup()
#number of rows confirms number of calls that passed through legal menu 2: 92,696

#slice out last 2 rows of each legal menu 2 call if activity name is na in the last row
legal2_last2_rows <- legal_menu2 %>%
  group_by(contact_session_id) %>%
  slice(
    if (is.na(last(activity_name))) {
      (n()-1):n()   # take last two rows if last activity_name is NA
    } else {
      n()           # take only the last row otherwise
    }
  ) %>%
  ungroup()

#slice out last 3 rows of each family menu call if activity name is na in the last 2 rows
legal2_last3_rows <- legal_menu2 %>%
  group_by(contact_session_id) %>%
  slice(
    if (is.na(last(activity_name))) {
      (n()-2):n()   # take last two rows if last activity_name is NA
    } else {
      n()           # take only the last row otherwise
    }
  ) %>%
  ungroup()


legal2_analysis <- legal_menu2 %>% 
  mutate(activity_analysis = case_when(contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "LegalMenu2"))$contact_session_id  
                                       ~ "Abandoned - Legal Menu 2",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "LegalMenu1"))$contact_session_id  
                                       ~ "Abandoned - Legal Menu 1",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "MainMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Left From Main Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "MainMenu" & 
                                                                                               (is.na(lead(activity_name)) |is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Left From Main Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(ep_name == "Main Number Telephony EP")
                                       )$contact_session_id 
                                       ~ "Not Abandoned - Left From Main Menu",
                                       
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "FamilyMenu"|
                                                                                             activity_name == "DivorceOrParentingMenu" |
                                                                                             activity_name == "SimpleDivorceMenu" |
                                                                                             activity_name == "ChildSupportMenu"
                                                                                             ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "DivorceOrParentingMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "DivorceOrParentingMenu" & 
                                                                                               (is.na(lead(activity_name)) | is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "SimpleDivorceMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "SimpleDivorceMenu" & 
                                                                                               (is.na(lead(activity_name)) | is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "ChildSupportMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "ChildSupportMenu" & 
                                                                                               (is.na(lead(activity_name)) | is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "FamilyMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "FamilyMenu" & 
                                                                                               (is.na(lead(activity_name)) | is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Family Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "ClosedQueueMenu" |
                                                                                               activity_name == "ClosedMenu" | 
                                                                                               flow_name == "ClosedQueueMenu"|
                                                                                               ep_name == "Closed Queue Menu Telephony EP"))$contact_session_id 
                                       ~ "Not Abandoned - Closed Queue Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(ep_name == "Closed Hours-Holidays Menu Telephony EP"))$contact_session_id 
                                       ~ "Not Abandoned - Closed Holidays Menu",
                                       
                                       
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "OtherLegalMenu"))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalMenu" & 
                                                                                               (is.na(lead(activity_name)) |is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "OtherLegalCriminalCaseMenu"))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalCriminalCaseMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalCriminalCaseMenu" & 
                                                                                               (is.na(lead(activity_name)) |is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalPersonalInjuryMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalPersonalInjuryMenu" & 
                                                                                               (is.na(lead(activity_name)) |is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "OtherLegalOtherMenu"))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalOtherMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "OtherLegalOtherMenu" & 
                                                                                               (is.na(lead(activity_name)) |is.na(lead(activity_name, 2)))
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Other Legal Menu",
                                       
                                       
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "BenefitsMenu" | 
                                                                                               activity_name == "HIVMenu" | 
                                                                                               activity_name == "EmploymentMenu"|
                                                                                               activity_name == "HousingMenu" |
                                                                                               activity_name == "TenantDeterrenceMenu" |
                                                                                               activity_name == "TenantMenu" |
                                                                                               activity_name == "ImmigrationMenu" | 
                                                                                               activity_name == "ImmigrationOtherMenu"|
                                                                                               ep_name == "Legal Housing Menu Telephony EP"))$contact_session_id 
                                       ~ "Not Abandoned - Exited to Another Menu",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(str_detect(activity_name, "Senior")))$contact_session_id ~ 
                                         "Not Abandoned - Exited to Another Menu",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(str_detect(ep_name, "Senior")))$contact_session_id ~ 
                                         "Not Abandoned - Exited to Another Menu",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(str_detect(activity_name, "Immigration")))$contact_session_id ~ 
                                         "Not Abandoned - Exited to Another Menu",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "IntakePreQueueMessage1"))$contact_session_id 
                                       ~ "Not Abandoned - Intake Pre Queue Message 1",
                                       
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "PreQueueMessage2"))$contact_session_id 
                                       ~ "Abandoned - Pre Queue Message 2",
                                       
                                       contact_session_id %in% (legal2_last3_rows %>% filter(activity_name == "PlayCCBConfirmation" | 
                                                                                               activity_name == "ConfirmCallbackNumber"|
                                                                                               ep_name == "Courtesy Callback Telephony EP"))$contact_session_id 
                                       ~ "Not Abandoned - Requested Callback",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "PlayMOH300s"))$contact_session_id 
                                       ~ "Abandoned - Quit Hold Music",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(activity_name == "QueueMenu1"))$contact_session_id 
                                       ~ "Abandoned - Queue Menu 1",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(str_detect(queue_name, regex("transfer", ignore_case = TRUE)))
                                       )$contact_session_id ~ "Not Abandoned - Transfer",
                                       contact_session_id %in% (family_last2_rows %>% filter(str_detect(activity_name, regex("Queue", ignore_case = TRUE)))
                                       )$contact_session_id ~ "Not Abandoned - Other Queue",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(is.na(activity_name) & !is.na(agent_name)))$contact_session_id 
                                       ~ "Not Abandoned - Agent Handled",
                                       contact_session_id %in% (legal2_last2_rows %>% filter(str_detect(activity_name, "Agent")))$contact_session_id 
                                       ~ "Not Abandoned - Agent Handled",
                                       .default = "Undetermined")) %>% 
  relocate(activity_analysis, contact_session_id)

legal2_analysis <- legal2_analysis %>% 
  filter(activity_analysis != "Not Abandoned - Exited to Another Menu" &
           activity_analysis != "Not Abandoned - Exited to Family Menu" &
           activity_analysis != "Abandoned - Legal Menu 1" &
           activity_analysis != "Not Abandoned - Left From Main Menu" &
           activity_analysis != "Not Abandoned - Other Queue" &
           activity_analysis != "Not Abandoned - Exited to Other Legal Menu")

#fix closed queue
legal2_analysis2 <- legal2_analysis %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Not Abandoned - Closed Queue Menu" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Not Abandoned - Closed Queue Menu" &
         ep_name == "Closed Queue Menu Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  )
#keep 7,681 instead of 46,231 
legal2_analysis2 %>% count(contact_session_id) #33,841

#fix callbacks
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Not Abandoned - Requested Callback" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Not Abandoned - Requested Callback" &
         ep_name == "All LAC Queues Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 
#keep 957 instead of 5678 
legal2_analysis2 %>% count(contact_session_id) #29,120

#fix intake pre queue 1
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Not Abandoned - Intake Pre Queue Message 1" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Not Abandoned - Intake Pre Queue Message 1" &
         ep_name == "All LAC Queues Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 
#keep 42 instead of 207

#fix closed holidays
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Not Abandoned - Closed Holidays Menu" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Not Abandoned - Closed Holidays Menu" &
         ep_name == "Closed Hours-Holidays Menu Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 
#keep 22 instead of 27

#fix queue 1
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Abandoned - Queue Menu 1" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Abandoned - Queue Menu 1" &
         ep_name == "All LAC Queues Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 
#keep 88 instead of 519

#fix quit hold music
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Abandoned - Quit Hold Music" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Abandoned - Quit Hold Music" &
         ep_name == "All LAC Queues Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 

#fix pre queue 2
legal2_analysis2 <- legal2_analysis2 %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_non_na_ep = lag(ep_name),
    last_non_na_ep = zoo::na.locf(last_non_na_ep, na.rm = FALSE)
  ) %>% 
  filter(
    # Keep ALL rows that are NOT the target activity
    activity_analysis != "Abandoned - Pre Queue Message 2" |
      
      # But if activity_analysis IS the target, enforce extra rules
      (activity_analysis == "Abandoned - Pre Queue Message 2" &
         ep_name == "All LAC Queues Telephony EP" &
         last_non_na_ep == "Legal Menu Telephony EP")
  ) 

legal2_analysis2 %>% count(contact_session_id) #28,238

#PROPORTION OF ABANDONMENT OR NOT ABANDONMENT REASONS
table2 <- legal2_analysis2 %>% 
  select(contact_session_id, activity_analysis) %>% 
  group_by(contact_session_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>% 
  count(activity_analysis) %>% 
  mutate(proportion = n/28238, 
         abandoned = case_when(
           grepl("Not Abandoned", activity_analysis, ignore.case = TRUE) ~ "Not Abandoned",
           grepl("Abandoned", activity_analysis, ignore.case = TRUE) ~ "Abandoned",
           grepl("Undetermined", activity_analysis, ignore.case = TRUE) ~ "Undetermined",
           TRUE ~ NA_character_
         )) %>% 
  relocate(abandoned) %>% 
  arrange(desc(n))

## SEE TABLE
table2 %>% 
  mutate(proportion = (percent(n/28238, accuracy = 0.01)))


#GT TABLE - ABANDONMENT/NOT REASONS
table2 %>% 
  group_by(abandoned) %>% 
  mutate(analysis = str_trim(str_remove(activity_analysis, "^[^-]+-"))) %>% 
  select(-activity_analysis) %>% 
  relocate(abandoned, analysis) %>% 
  gt(rowname_col = "analysis") %>% 
  tab_header(title = md("**Call End Reasons**"), 
             subtitle = "Legal Menu 2") |>
  cols_label(n = md("Number of Calls"), 
             proportion = md("Proportion of Calls")) |> 
  fmt_percent(
    columns = c(proportion),   
    decimals = 2                 
  ) %>% 
  tab_options(row_group.background.color = "#31356e", 
              table.font.size = px(14),      # Reduce font size for the entire table
              data_row.padding = px(5),      # Adjusts padding for data rows
              heading.padding = px(2),       # Adjusts padding for heading rows
              column_labels.font.size = px(11)) |>  # Reduce font size for column labels
  data_color(
    columns = c(n),
    colors = scales::col_factor(
      palette = c("transparent", "grey80"), # Change "yellow" to your desired color
      domain = NULL,
      na.color = "transparent"
    ),
    apply_to = "fill",
    fn = function(x) ifelse(x == max(x), "grey80", NA) # Replace "yellow" with your desired color
  )

#ABANDONED CALLS VS NOT ABANDONED CALLS PERCENTAGES
abandoned2 <- table2 %>% 
  group_by(abandoned) %>% 
  summarize(calls = sum(n), 
            proportion = sum(proportion))

#gt table
abandoned2 %>% 
  gt() %>% 
  tab_header(title = md("**Call Abandonment**"), 
             subtitle = "Legal Menu 2") |>
  cols_label(abandoned = md("Abandonment"), 
             calls = md("Number of Calls"), 
             proportion = md("Proportion of Calls")) |> 
  fmt_percent(
    columns = c(proportion),   
    decimals = 2                 
  ) %>% 
  tab_style(
    style = cell_fill(color = "#d4edda"),  # light green
    locations = cells_body(
      rows = abandoned == "Not Abandoned"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#f8d7da"),  # light red
    locations = cells_body(
      rows = abandoned == "Abandoned"
    )
  )


#OF ABANDONED CALLS - PROPORTIONS
table2 %>% 
  filter(abandoned == "Abandoned") %>% 
  group_by(abandoned) %>% 
  mutate(analysis = str_trim(str_remove(activity_analysis, "^[^-]+-"))) %>% 
  select(-activity_analysis) %>% 
  mutate(proportion = n/sum(n)) %>% 
  relocate(abandoned, analysis) %>% 
  gt(rowname_col = "analysis") %>% 
  tab_header(title = md("**Abandoned Call Reasons**"), 
             subtitle = "Legal Menu 2") |>
  cols_label(n = md("Number of Calls"), 
             proportion = md("Proportion of Calls")) |> 
  fmt_percent(
    columns = c(proportion),   
    decimals = 2                 
  )%>% 
  tab_options(row_group.background.color = "#31356e")

ggplot(abandoned2, aes(x = "", y = proportion, fill = abandoned))+
  geom_bar(width = 1, stat = "identity")+
  scale_x_discrete(NULL)+
  scale_y_continuous(NULL)+
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.01)), 
            position = position_stack(vjust = 0.35), 
            family = "Georgia", 
            size = 4, 
            hjust = 0.17)+
  scale_fill_manual(name = NULL, 
                    values = c("brown1", "seagreen", "grey80"),
  ) +
  labs(title = "Proportion of Abandonment", 
       subtitle = "Legal Menu 2", 
       fill = NULL) +
  theme_minimal()+
  theme(axis.text = element_blank(), 
        legend.text = element_text(family = "Georgia", size = 10),
        axis.title = element_text(family = "Georgia"),
        plot.title = element_text(size = 13, face = "bold", family = "Georgia"), 
        plot.subtitle = element_text(size = 10, family = "Georgia"))

