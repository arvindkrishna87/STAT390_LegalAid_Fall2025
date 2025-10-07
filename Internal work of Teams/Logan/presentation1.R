### Data Manipulation for Presentation 1

# load packages and data
library(tidyverse)

# clean variable names
car_data <- car_data %>% 
  janitor::clean_names()

# summarize by contact session id
counted_data <- car_data %>%
  group_by(contact_session_id) %>% 
  mutate(n = n()) %>% 
  select(n, everything())

counted_data %>% 
  summarize(median_n = median(n))

write_csv(counted_data, file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/counted_data.csv")

# clean all calls variable names
all_calls_data <- all_calls_data %>% 
  janitor::clean_names()

#
all_calls_data %>% 
  filter(direction == "TERMINATING") %>% 
  mutate(
    duration = as_datetime(release_time)  - as_datetime(start_time)
  ) 
  
  