# load in libraries 
library(tidyverse)

## USER INPUT REQUIRED -- write the data path on your local computer
new_car_data <- read_csv(file = "/Users/loganroever/Desktop/stat390.nosync/new_car_data.csv")
car_data <- read_csv(file = "/Users/loganroever/Desktop/stat390.nosync/car_data.csv")

new_car_data <- new_car_data %>% janitor::clean_names()
car_data <- car_data %>% janitor::clean_names()

# After removal of senior confirmation, find if people are traversing backward after seniors menu
# For convenience, grab all contact ids who went to senior menu
senior_menu_ids <- new_car_data %>% 
  filter(activity_name == "SeniorsMenu") %>% 
  distinct(contact_session_id)

# One scenario: people go to senior menu, leave, and go back to senior menu

navigated_back <- new_car_data %>% 
  arrange(contact_session_id, activity_start_timestamp) %>% 
  group_by(contact_session_id) %>% 
  mutate(
    seniors_first = cumsum(activity_name == "SeniorsMenu") > 0,
    suburb_after = cumsum(activity_name == "SuburbsOrCityMenu" & seniors_first) > 0,
    senior_again = activity_name == "SeniorsMenu" & suburb_after
  ) %>% 
  summarize(returned_to_seniors = any(senior_again)) %>% 
  filter(returned_to_seniors)

navigated_back %>% summarize(proportion = n_distinct(contact_session_id)/ n_distinct(senior_menu_ids),
                             percent = (n_distinct(contact_session_id)/ n_distinct(senior_menu_ids)) * 100)


# NOW compare this to the old proportions
oldsenior_menu_ids <- car_data %>% 
  filter(activity_name == "SeniorsMenu") %>% 
  distinct(contact_session_id)

navigated_back2 <- car_data %>% 
  arrange(contact_session_id, activity_start_timestamp) %>% 
  group_by(contact_session_id) %>% 
  mutate(
    seniors_first = cumsum(activity_name == "SeniorsMenu") > 0,
    confirm_after = cumsum(activity_name == "SeniorsConfirmationMenu" & seniors_first) > 0,
    senior_again = activity_name == "SeniorsMenu" & confirm_after
  ) %>% 
  summarize(returned_to_seniors = any(senior_again)) %>% 
  filter(returned_to_seniors)

navigated_back2 %>% summarize(proportion = n_distinct(contact_session_id)/ n_distinct(oldsenior_menu_ids),
                              percent = proportion * 100)

# find durations for double callers
durations <- new_car_data %>% 
  filter(!(activity_start_timestamp >= ymd("2025-10-01") & activity_start_timestamp <= ymd("2025-10-14"))) %>% 
  group_by(contact_session_id) %>% 
  summarize(start_time = min(activity_start_timestamp, na.rm = TRUE),
            end_time = max(activity_start_timestamp, na.rm =TRUE),
            call_duration = as.numeric(difftime(end_time, start_time, units = "secs"))
            ) %>% 
  ungroup() 

average_durations <- durations %>% 
  filter(contact_session_id %in% navigated_back$contact_session_id) %>% 
  summarize(avg_duration_secs = mean(call_duration, na.rm = TRUE),
            median_duration = median(call_duration, na.rm = TRUE))

# again for old data
durations2 <-car_data %>% 
  group_by(contact_session_id) %>% 
  summarize(start_time = min(activity_start_timestamp, na.rm = TRUE),
            end_time = max(activity_start_timestamp, na.rm =TRUE),
            call_duration = as.numeric(difftime(end_time, start_time, units = "secs"))
  ) %>% 
  ungroup() 

average_durations2 <- durations2 %>% 
  filter(contact_session_id %in% navigated_back2$contact_session_id) %>% 
  summarize(avg_duration_secs = mean(call_duration, na.rm = TRUE),
            median_sec = median(call_duration, na.rm = TRUE))
