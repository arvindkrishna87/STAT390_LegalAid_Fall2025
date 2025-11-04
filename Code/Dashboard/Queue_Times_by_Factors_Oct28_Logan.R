# Load libraries
library(tidyverse)


# Read the call data. USER INPUT REQUIRED: CHANGE TO LOCAL PATHWAY
# Data files are created from CarDataImport_Oct7_LoganRoever.R 
# AND Queue_Time_Analysis_Oct20_Jake_Miller.ipynb
queue_times <- readxl::read_xlsx("/Users/loganroever/Desktop/stat390.nosync/filtered_queue_time_analysis.xlsx")
car_data <- read_csv("/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/car_data.csv")

# clean names for ease of use
queue_times <- queue_times %>% janitor::clean_names()
car_data <- car_data %>% janitor::clean_names()

# join queue times back to original car data for filtering information. 
queued_clients <- queue_times %>% 
  left_join(car_data, by = "contact_session_id", relationship = "many-to-many") %>% 
  group_by(contact_session_id) %>% 
  summarize(across(everything(), first), .groups = "drop")

# create month, year, and open hours columns while also adding time in minutes for ease of understanding
queued_clients <- queued_clients %>%
  mutate(month = month(activity_start_timestamp),
         year = year(activity_start_timestamp),
         open_hour = if_else(hour >= 8 & hour <= 17, "Open", "Closed"),
         month_name = month.name[month],
         time_in_queue_min = time_in_queue_s / 60
  )

# save out data
write_csv(queued_clients, file = "/Users/loganroever/Desktop/stat390.nosync/queued_clients.csv")
