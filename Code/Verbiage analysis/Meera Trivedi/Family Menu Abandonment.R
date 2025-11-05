#FAMILY MENU ABANDONMENT


library(tidyverse)
library(writexl)
library(janitor)
library(readxl)
library(hms)
library(scales)
library(gt)


krish_file_python <- read_csv("/Users/meeratrivedi/Downloads/STAT 390/krish_cleaning.csv") %>% 
  clean_names()


krish_file_python %>% 
  count(contact_session_id)

df_union1 %>% count(contact_session_id) #90,940 contact session ids

##NOTES FROM KRISH - 10/30/25
#eventually check distribution of time when customer is talking to agent

#ex: activity name column: if family was the last activity menu then the call was abandoned, 
#child support means not abandoned

#menus to check: family housing benefits employment immigration HIV
#--------------------------------------------------------------------------------------------

#rows that are in this EP
family <- df_union1 %>% filter(ep_name == "Legal Family Menu Telephony EP")

#all calls that have a row that says legal family menu telephony EP
##aka all calls that pass through the family menu at some point
family_menu <- krish_file_python %>%
  semi_join(family, by = "contact_session_id")

#make call duration variable
family_menu <- family_menu %>% 
  mutate(time = as_hms(activity_start_timestamp)) %>% 
  arrange(activity_start_timestamp) %>% 
  mutate(time_diff = time-lag(time)) %>% 
  mutate(time_diff = as.numeric(time_diff)) %>% 
  mutate(call_duration = sum(time_diff, na.rm = TRUE)) 

#number of family menu calls that went through each activity name
family_menu %>% 
  group_by(activity_name) %>% 
  count(contact_session_id) %>% 
  count(activity_name) %>% 
  arrange(desc(n))

#slice out last row of each family menu call
family_last_rows <- family_menu %>%
  group_by(contact_session_id) %>%
  slice_tail(n = 1) %>%
  ungroup()
#number of rows confirms number of calls that passed through family menu: 9743

#last activities of each call - count
family_last_rows %>% count(activity_name)

#slice out last 2 rows of each family menu call if activity name is na in the last row
family_last2_rows <- family_menu %>%
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
family_last3_rows <- family_menu %>%
  group_by(contact_session_id) %>%
  slice(
    if (is.na(last(activity_name))) {
      (n()-2):n()   # take last two rows if last activity_name is NA
    } else {
      n()           # take only the last row otherwise
    }
  ) %>%
  ungroup()

family_last2_rows %>% count(activity_name) %>% view()

family_last2_rows %>% count(queue_name) %>% view()


### ANALYSIS TABLE
#using last activity of each call to determine if they abandoned the call
family_analysis <- family_menu %>% 
  mutate(activity_analysis = case_when(contact_session_id %in% (family_last2_rows %>% filter(activity_name == "ChildSupportMenu"))$contact_session_id 
                                            ~ "Not Abandoned - Child Support",
                                       contact_session_id %in% (family_last3_rows %>% filter(activity_name == "ChildSupportMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Child Support",
                                       contact_session_id %in% (family_last3_rows %>% filter(activity_name == "MainMenu" & 
                                                                                               (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                       ))$contact_session_id 
                                       ~ "Not Abandoned - Returned to Main Menu",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "FamilyMenu"))$contact_session_id 
                              ~ "Abandoned - Family Menu",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "FamilyMenu" & 
                                                                                      (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                                                                                    ))$contact_session_id 
                              ~ "Abandoned - Family Menu",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "ClosedQueueMenu" |
                                                                                      activity_name == "ClosedMenu" | 
                                                                                      flow_name == "ClosedQueueMenu"|
                                                                                      ep_name == "Closed Queue Menu Telephony EP"))$contact_session_id 
                              ~ "Not Abandoned - Closed Queue Menu",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "SimpleDivorceMenu"))$contact_session_id 
                              ~ "Not Abandoned - Simple Divorce Menu",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "SimpleDivorceMenu" & 
                                                                                      (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                              ))$contact_session_id 
                              ~ "Not Abandoned - Simple Divorce Menu",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "DivorceOrParentingMenu"))$contact_session_id 
                              ~ "Abandoned - Divorce or Parenting Menu",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "DivorceOrParentingMenu" & 
                                                                                      (lead(termination_reason) == "Customer Left" | lead(termination_reason, 2) == "Customer Left")
                              ))$contact_session_id 
                              ~ "Abandoned - Divorce or Parenting Menu",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "IntakePreQueueMessage1"))$contact_session_id 
                              ~ "Not Abandoned - Intake Pre Queue Message 1",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "PreQueueMessage2"))$contact_session_id 
                              ~ "Abandoned - Pre Queue Message 2",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "LegalMenu2" | 
                                                                                      activity_name == "OtherLegalMenu" | 
                                                                                      activity_name == "OtherLegalCriminalCaseMenu" | 
                                                                                      activity_name == "OtherLegalOtherMenu" | 
                                                                                      activity_name == "OtherLegalPersonalInjuryMenu" | 
                                                                                      ep_name == "Legal Menu Telephony EP"|
                                                                                      ep_name == "Other Legal Menu Telephony EP" 
                                                                                      ))$contact_session_id 
                              ~ "Not Abandoned - Exited to a Legal Menu",
                              contact_session_id %in% (family_last2_rows %>% filter(str_detect(queue_name, regex("transfer", ignore_case = TRUE)))
                              )$contact_session_id ~ "Not Abandoned - Transfer",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "PlayCCBConfirmation" | 
                                                                                      activity_name == "ConfirmCallbackNumber"|
                                                                                      ep_name == "Courtesy Callback Telephony EP"))$contact_session_id 
                              ~ "Not Abandoned - Requested Callback",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "PlayMOH300s"))$contact_session_id 
                              ~ "Abandoned - Quit Hold Music",
                              contact_session_id %in% (family_last2_rows %>% filter(activity_name == "QueueMenu1"))$contact_session_id 
                              ~ "Abandoned - Queue Menu 1",
                             contact_session_id %in% (family_last2_rows %>% filter(is.na(activity_name) & !is.na(agent_name)))$contact_session_id 
                             ~ "Not Abandoned - Agent Handled",
                              contact_session_id %in% (family_last3_rows %>% filter(activity_name == "BenefitsMenu" | 
                                                                                      activity_name == "HIVMenu" | 
                                                                                      activity_name == "EmploymentMenu"|
                                                                                      activity_name == "HousingMenu" |
                                                                                      activity_name == "TenantDeterrenceMenu" |
                                                                                      activity_name == "TenantMenu" |
                                                                                      activity_name == "ImmigrationMenu" | 
                                                                                      activity_name == "ImmigrationOtherMenu" | 
                                                                                      ep_name == "Legal Housing Menu Telephony EP"))$contact_session_id 
                              ~ "Not Abandoned - Exited Family Menu to Another Menu",
                             contact_session_id %in% (family_last2_rows %>% filter(str_detect(activity_name, "Senior")))$contact_session_id ~ 
                               "Not Abandoned - Exited Family Menu to Another Menu",
                              .default = "Undetermined")) %>% 
  relocate(activity_analysis, contact_session_id)

#PROPORTION OF ABANDONMENT OR NOT ABANDONMENT REASONS
table <- family_analysis %>% 
  select(contact_session_id, activity_analysis) %>% 
  group_by(contact_session_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>% 
  count(activity_analysis) %>% 
  mutate(proportion = n/9743, 
         abandoned = case_when(
           grepl("Not Abandoned", activity_analysis, ignore.case = TRUE) ~ "Not Abandoned",
           grepl("Abandoned", activity_analysis, ignore.case = TRUE) ~ "Abandoned",
           grepl("Undetermined", activity_analysis, ignore.case = TRUE) ~ "Undetermined",
           TRUE ~ NA_character_
         )) %>% 
  relocate(abandoned) %>% 
  arrange(desc(n))

#contact_session_id %in% (family_last3_rows %>% grepl("Senior", activity_analysis, ignore.case = TRUE))$contact_session_id 
#~ "Not Abandoned - Exited Family Menu to Another Menu",

## SEE TABLE
table %>% 
  mutate(proportion = (percent(n/9743, accuracy = 0.01)))

#GT TABLE - ABANDONMENT/NOT REASONS
table %>% 
  group_by(abandoned) %>% 
  gt(rowname_col = "activity_analysis") %>% 
  tab_header(title = md("**Call End Reasons**"), 
             subtitle = "Family Menu") |>
  cols_label(n = md("Number of Calls"), 
             proportion = md("Proportion of Calls")) |> 
  fmt_percent(
    columns = c(proportion),   
    decimals = 2                 
  ) %>% 
  tab_options(row_group.background.color = "#31356e", 
              table.font.size = px(12),      # Reduce font size for the entire table
              data_row.padding = px(3),      # Adjusts padding for data rows
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
abandoned <- table %>% 
  group_by(abandoned) %>% 
  summarize(calls = sum(n), 
            proportion = sum(proportion))

#gt table
abandoned %>% 
  gt() %>% 
  tab_header(title = md("**Call Abandonment**"), 
                             subtitle = "Family Menu") |>
  cols_label(abandoned = md("Abandonment"), 
             calls = md("Number of Calls"), 
             proportion = md("Proportion of Calls")) |> 
  fmt_percent(
    columns = c(proportion),   
    decimals = 2                 
  ) 

#Check categories
family_analysis %>%
  filter(activity_analysis == "Undetermined") %>% view()

family_analysis %>%
  filter(activity_analysis == "Not Abandoned - Queue Menu 1") %>% view()

#### VISUALIZATION
ggplot(abandoned, aes(x = "", y = proportion, fill = abandoned))+
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
  labs(title = "Proportion of Abandonment Reasons", 
       subtitle = "Family Menu", 
       fill = NULL) +
  theme_minimal()+
  theme(axis.text = element_blank(), 
        legend.text = element_text(family = "Georgia", size = 10),
        axis.title = element_text(family = "Georgia"),
        plot.title = element_text(size = 13, face = "bold", family = "Georgia"), 
        plot.subtitle = element_text(size = 10, family = "Georgia"))

