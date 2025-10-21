# Load libraries
library(tidyverse)


# Read the call data. USER INPUT REQUIRED: CHANGE TO LOCAL PATHWAY
# Data file is created from CarDataImport_Oct7_LoganRoever.R
df <- read_csv("/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/car_data.csv")
df <- df %>% janitor::clean_names()

# Define target activities
target_activities <- c("PreQueueMessage1", "PreQueueMessage2", "QueueMenu1", "PlayMOH300s")

# Step 1: Find all call IDs that hit one of the target activities
call_ids_to_keep <- df %>% 
  filter(activity_name %in% target_activities) %>% 
  distinct(contact_session_id) %>% 
  pull()

# Step 2: Filter to keep all rows from those calls
filtered_df <- df %>% 
  filter(contact_session_id %in% call_ids_to_keep) %>% 
  arrange(contact_session_id, activity_start_timestamp)

# Step 3: Identify which activities directly precede a queue event
queue_name_df <- filtered_df %>%
  group_by(contact_session_id) %>%
  arrange(activity_start_timestamp, .by_group = TRUE) %>%
  mutate(
    prev_activity = lag(activity_name),
    prev_timestamp = lag(activity_start_timestamp),
    time_diff_sec = as.numeric(activity_start_timestamp - prev_timestamp, units = "secs")
  ) %>%
  ungroup()

# focus on large queues
large_queues <- queue_name_df %>%
  count(queue_name, name = "count") %>%
  filter(count > 500) %>%
  pull(queue_name)

large_queue_df <- queue_name_df %>%
  filter(queue_name %in% large_queues)

# calculate the time spent in each queue for each contact session
time_results_df <- large_queue_df %>%
  group_by(contact_session_id, queue_name) %>%
  summarize(
    avg_time_in_queue_sec = mean(time_diff_sec, na.rm = TRUE),
    total_time_in_queue_sec = sum(time_diff_sec, na.rm = TRUE),
    num_events = n(),
    .groups = "drop"
  )

# now join with original dataframe to get full scope of data
queued_clients <- time_results_df %>%
  left_join(df, by = "contact_session_id", relationship = "many-to-many") %>%
  group_by(contact_session_id) %>%
  summarize(across(everything(), first), .groups = "drop")


# make month and year columns
queued_clients <- queued_clients %>%
  mutate(month = month(activity_start_timestamp),
         year = year(activity_start_timestamp),
         open_hour = if_else(hour >= 8 & hour <= 17, "Open", "Closed"),
         month_name = month.name[month]
         )

# save dataset
write_csv(queued_clients, file = "/Users/loganroever/Desktop/stat390.nosync/STAT390_LegalAid_Fall2025/Internal work of Teams/Logan/queued_clients.csv")

