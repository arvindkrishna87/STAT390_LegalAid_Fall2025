# Presentation 4: "Agent Left" and "Customer Left" Analysis

# --- Load libraries & data ---
library(dplyr)
library(readr)
library(stringr)

# Load original full CAR dataset:
combined_data <- read_csv("data/combined_data.csv") 

# Remove unnecessary "...1" column and save df to be used within Power BI
clean_session_df <- combined_data |>
  dplyr::select(
    contact_session_id,
    ep_name,
    flow_name,
    activity_name,
    activity_start_timestamp,
    queue_name,
    agent_name,
    termination_reason,
    activity_datetime,
    weekday_number,
    weekend_weekday
  )
write_csv(clean_session_df, "data/clean_session_df.csv")

# Step 1: Identify sessions with at least one meaningful termination reason
calls_with_termination_rows <- calls_with_termination_rows %>%
  mutate(is_termination = !is.na(termination_reason) & termination_reason != "N/A")

# Step 2: Calculate session-level info
session_summary <- calls_with_termination_rows %>%
  group_by(contact_session_id) %>%
  summarize(
    termination_count = sum(is_termination),
    termination_reasons = list(termination_reason[is_termination]),
    first_termination_time = min(activity_datetime[is_termination]),
    last_activity_time = max(activity_datetime),
    .groups = "drop"
  )

# Step 3: Classify sessions
session_summary <- session_summary %>%
  mutate(session_type = case_when(
    # 1) Normal if only one termination reason
    termination_count == 1 ~ "Normal",
    
    # 2) Normal if MAX_CALLBACK_RETRY_LIMIT_REACHED appears in that session's reasons
    map_lgl(termination_reasons, ~ "MAX_CALLBACK_RETRY_LIMIT_REACHED" %in% .x) ~ "Normal",
    
    # 3) Normal if first_termination_time - last_activity_time == 0
    (first_termination_time - last_activity_time) == 0 ~ "Normal",
    
    # 4) Everything else is Irregular
    TRUE ~ "Irregular"
  ))

## --- Step 4: Save dataset to be used within Power BI ---
write_csv(session_summary, "data/TerminationReasonAnalysis_18Nov.csv")