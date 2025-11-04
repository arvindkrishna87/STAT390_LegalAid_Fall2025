# Presentation 3: Callback vs. Post-Queue Pickup Analysis

# --- Load libraries & data ---
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
install.packages("writexl")
library(writexl)

# Load original full CAR dataset:
combined_data <- read_csv("data/combined_data.csv")



# ============================================================
# SECTION 1: CALLBACK ANALYSIS
# ============================================================

## --- Step 1a: Filter all calls with a callback attempt ---
callback_df <- combined_data |>
  filter(flow_name == "CourtesyCallback")

## --- Step 1b: Get all contact session IDs that requested a callback ---
callback_ids <- callback_df |>
  distinct(contact_session_id) |>
  pull()

## --- Step 1c: Get all rows for those callback sessions ---
callback_sessions <- combined_data |>
  filter(contact_session_id %in% callback_ids) |>
  arrange(contact_session_id, activity_datetime)

## --- Step 1d: Define unsuccessful termination reasons ---
failure_reasons <- c(
  "Customer Left", "Agent Left", "RONA_TIMER_EXPIRED", "AGENT_ENDS",
  "MEDIA_MANAGER_INTERNAL_ERROR", "NO_ANSWER_FROM_AGENT", "Queue Timeout",
  "USER_BUSY", "USER_DECLINED", "AGENT_BUSY", "USER_UNAVAILABLE",
  "RONA Timer Expired", "NO_ANSWER_USER", "AGENT_UNAVAILABLE",
  "CUSTOMER_BUSY", "MAX_CALLBACK_RETRY_LIMIT_REACHED", 
  "Participant Invite timer expired", "CUSTOMER_UNAVAILABLE", 
  "NO_ANSWER_FROM_CUSTOMER", "CONTACT_CALLBACK_IN_PROGRESS",
  "NO_ANSWER_CUSTOMER"
)

## --- Step 1e: Determine if callback was successful and calculate duration ---
# successful callback = CourtesyCallback -> session eventually reached an agent (last LegalServerScreenPop) & failed termination reason not present
callback_success_df <- callback_sessions |>
  group_by(contact_session_id) |>
  summarize(
    callback_requested_time = min(activity_datetime[flow_name == "CourtesyCallback"], na.rm = TRUE),
    agent_connect_time      = max(activity_datetime[activity_name == "LegalServerScreenPop"], na.rm = TRUE),
    call_end_time           = max(activity_datetime, na.rm = TRUE),
    last_termination_reason = termination_reason[which.max(activity_datetime[activity_name == "LegalServerScreenPop"])],
    reached_agent           = !(last_termination_reason %in% failure_reasons),
    .groups = "drop"
  ) |>
  mutate(
    callback_duration_mins = ifelse(
      reached_agent,
      as.numeric(difftime(call_end_time, agent_connect_time, units = "mins")),
      as.numeric(difftime(agent_connect_time, callback_requested_time, units = "mins"))
    )
  )

# ============================================================
# SECTION 2: QUEUE (NON-CALLBACK) ANALYSIS
# ============================================================

queue_activities <- c("PreQueueMessage1", "PreQueueMessage2", "QueueMenu1", "PlayMOH300s")

## --- Step 2a: Filter sessions without any callback attempt ---
noncallback_df <- combined_data |>
  group_by(contact_session_id) |>
  filter(!any(flow_name == "CourtesyCallback")) |>
  ungroup()

## --- Step 2b: Identify queue-related sessions ---
queue_session_ids <- noncallback_df |>
  filter(activity_name %in% queue_activities) |>
  distinct(contact_session_id) |>
  pull()

## --- Step 2c: Retrieve all rows for those queue sessions ---
queue_sessions <- noncallback_df |>
  filter(contact_session_id %in% queue_session_ids) |>
  arrange(contact_session_id, activity_datetime)

## --- Step 2d: Compute successful post-queue call durations ---
# successful post-queue call = queue activity present -> session eventually reached an agent (last LegalServerScreenPop) & failed termination reason not present
queue_success_df <- queue_sessions |>
  group_by(contact_session_id) |>
  summarize(
    agent_connect_time      = max(activity_datetime[activity_name == "LegalServerScreenPop"], na.rm = TRUE),
    call_end_time           = max(activity_datetime, na.rm = TRUE),
    last_termination_reason = termination_reason[which.max(activity_datetime[activity_name == "LegalServerScreenPop"])],
    reached_agent           = !(last_termination_reason %in% failure_reasons),
    .groups = "drop"
  ) |>
  mutate(
    post_queue_duration_mins = ifelse(
      reached_agent,
      as.numeric(difftime(call_end_time, agent_connect_time, units = "mins")),
      as.numeric(difftime(agent_connect_time, min(activity_datetime), units = "mins"))  # fallback if unsuccessful
    )
  )

# ============================================================
# SECTION 3: STANDARDIZE BOTH DATASETS
# ============================================================

## --- Step 3a: Format callback durations ---
callback_durations <- callback_success_df |>
  filter(reached_agent) |>
  mutate(
    post_agent_start = agent_connect_time,
    post_agent_end   = call_end_time,
    duration_mins    = callback_duration_mins,
    call_type        = "Callback"
  ) |>
  select(contact_session_id, post_agent_start, post_agent_end, duration_mins, call_type) |>
  filter(is.finite(duration_mins))

## --- Step 3b: Format queue durations ---
queue_durations <- queue_call_durations |>
  mutate(
    post_agent_start = agent_connect_time,
    post_agent_end   = call_end_time,
    duration_mins    = post_queue_duration_mins,
    call_type        = "PostQueue"
  ) |>
  select(contact_session_id, post_agent_start, post_agent_end, duration_mins, call_type) |>
  filter(is.finite(duration_mins))

# ============================================================
# SECTION 4: COMBINE AND FORMAT RESULTS
# ============================================================

## --- Step 4a: Combine both datasets ---
all_post_agent_durations <- bind_rows(callback_durations, queue_durations)

## --- Step 4b: Add hour-based time labels ---
all_post_agent_durations <- all_post_agent_durations |>
  mutate(
    hour_start = hour(post_agent_start),
    hour_label = case_when(
      hour_start == 0  ~ "12 AM",
      hour_start < 12  ~ paste0(hour_start, " AM"),
      hour_start == 12 ~ "12 PM",
      TRUE             ~ paste0(hour_start - 12, " PM")
    )
  )

## --- Step 4c: Save combined dataset ---
write_csv(all_post_agent_durations, "data/CallbackAnalysis_3Nov.csv")