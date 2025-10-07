### Data Manipulation for Presentation 1

# load packages and data
library(tidyverse)

## USER INPUT REQUIRED -- put path on local computer
car_data <- read_csv(file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/car_data.csv" )
all_calls_data <- read_csv(file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/all_calls_data.csv" )

# clean variable names
car_data <- car_data %>% 
  janitor::clean_names()

# summarize by contact session id
counted_data <- car_data %>%
  summarize(n = n(), .by = contact_session_id)

counted_data %>% 
  summarize(median_n = median(n))

## summarize the menus accessed
chosen_ids <- car_data %>% 
  count(contact_session_id, name = "n_entries") %>% 
  filter(n_entries >= 10, n_entries <= 75)

# join ids back to original data
avg_activities <- car_data %>% 
  group_by(activity_name) %>% 
  summarize(
    total = n(),
    n_sessions = n_distinct(contact_session_id),
    avg_per_session = total / n_sessions
  ) %>% 
  arrange(desc(avg_per_session))


# save to csvs
write_csv(counted_data, file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/counted_data.csv")
write_csv(avg_activities, file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/avg_activities.csv")



  