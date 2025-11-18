## Immigration Menu Call Outcomes Analysis ----

# load libraries 
library(tidyverse)
library(lubridate)

# read in combined CAR dataset (see separate import scripts for creation)
# set a repository as Internal work of Teams/EDA_simple_2
car <- read_csv('data/combined_CAR.csv') |> janitor::clean_names()

# filter CAR data to calls after March 15, 2025
car <- car |>
  mutate(
    activity_start_timestamp = ymd_hms(activity_start_timestamp),
    hour = hour(activity_start_timestamp)
  ) |>
  filter(activity_start_timestamp >= '2025/03/16 00:00:00')

# get contact session IDs where flow_name == "LegalImmigrationMenu"
immigration_id <- car |>
  filter(flow_name == "LegalImmigrationMenu") |>
  pull(contact_session_id) |>
  unique()

# filter car by all rows belonging to those session IDs
immigration_calls <- car |>
  filter(contact_session_id %in% immigration_id)

# show # of unique contact_session_id
length(unique(immigration_calls$contact_session_id))

# count termination reasons and combine NAs, RONA Timer Expired
immigration_calls_clean <- immigration_calls |>
  mutate(
    termination_reason = trimws(termination_reason),
    termination_reason = case_when(
      is.na(termination_reason) ~ "NA",
      termination_reason %in% c("NA", "N/A") ~ "NA",
      termination_reason %in% c("RONA Timer Expired", "RONA_TIMER_EXPIRED") ~ "RONA Timer Expired",
      TRUE ~ termination_reason
    )
  )

# CASE 1: Trafficking Voicemail Transfer ----
case1 <- immigration_calls_clean |>
  filter(queue_name == "Trafficking Voicemail Transfer" | activity_name == "TraffickingVoicemailTransfer") 

# extract unique ids
case1_id <- case1 |>
  pull(contact_session_id) |>
  unique()

# show # of unique contact_session_id
length(case1_id)

# count termination reasons
case1_termination <- case1 |>
  group_by(contact_session_id) |>
  slice_max(activity_start_timestamp, n = 1, with_ties = FALSE) |>  # latest row only
  ungroup() |>
  count(termination_reason, sort = TRUE)

case1_termination

# CASE 2: Immigration or Immigration SP (all other outcomes, not trafficking voicemail) ----
case2 <- immigration_calls_clean |>
  filter(!(contact_session_id %in% case1_id))

# show # of unique contact_session_id
length(unique(case2$contact_session_id))

# count termination reasons
case2_termination <- case2 |>
  group_by(contact_session_id) |>
  slice_max(activity_start_timestamp, n = 1, with_ties = FALSE) |>  # latest row only
  ungroup() |>
  count(termination_reason, sort = TRUE)

case2_termination

# Classification ----
# prepare final outcomes from Case 1 & Case 2
final_outcomes <- bind_rows(
  # Case 1 (keep all)
  case1 |>
    group_by(contact_session_id) |>
    slice_max(activity_start_timestamp, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(case = "Trafficking Voicemail Transfer"),
  
  # Case 2 (filter out NA termination_reason)
  case2 |>
    group_by(contact_session_id) |>
    slice_max(activity_start_timestamp, n = 1, with_ties = FALSE) |>
    ungroup() |>
    filter(!is.na(termination_reason) & termination_reason != "NA") |>
    mutate(case = "Other Immigration Routes")
) |>
  # add agent involvement indicator
  mutate(connected_to_agent = !is.na(agent_name) & agent_name != "N/A")

# classify call outcomes: POSITIVE/NEUTRAL/NEGATIVE
# POSITIVE: Caller connects with an agent and the call ends normally
# NEGATIVE: Caller does not reach an agent due to abandonment or system/agent failure
# NEUTRAL: Caller receives information without agent support or contact is unreachable/out-of-scope

classified_outcomes <- final_outcomes |>
  mutate(
    outcome = case_when(
      # POSITIVE
      termination_reason == "Agent Left" ~ "Positive",
      termination_reason == "Customer Left" & connected_to_agent ~ "Positive",
      
      # NEGATIVE
      termination_reason %in% c("AGENT_UNAVAILABLE", "AGENT_BUSY",
                                "NO_ANSWER_FROM_AGENT", "MEDIA_MANAGER_INTERNAL_ERROR",
                                "CHANNEL_FAILURE") ~ "Negative",
      termination_reason == "Customer Left" & !connected_to_agent ~ "Negative",
      
      # NEUTRAL
      TRUE ~ "Neutral"
    )
  )

# Visualization ----
# summarize outcome counts + percentages by routing case
plot_data <- classified_outcomes |>
  count(case, outcome) |>
  group_by(case) |>
  mutate(percent = n / sum(n) * 100)

# percentage stacked bar chart
ggplot(plot_data, aes(x = factor(case,
                                 levels = c("Trafficking Voicemail Transfer",
                                            "Other Immigration Routes")),
                      y = percent, fill = outcome)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = n,
        y = percent - 0.02),
    position = position_fill(vjust = 0.5),
    size = 3.5
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Call Outcome Percentage by R",
    subtitle = "Trafficking Voicemail vs Other Routes",
    x = "Routing Case",
    y = "Outcome Distribution",
    fill = "Outcome Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# count comparison chart by outcome category  
ggplot(
  plot_data |>
    mutate(
      case = factor(case,
                    levels = c("Trafficking Voicemail Transfer",
                               "Other Immigration Routes")),
      outcome = factor(outcome,
                       levels = c("Positive", "Neutral", "Negative"))
    ),
  aes(x = outcome, y = n, fill = case)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Call Outcome Counts by Routing Case",
    subtitle = "Trafficking Voicemail vs Other Routes",
    x = "Outcome",
    y = "Number of Calls",
    fill = "Routing Path"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# combined outcomes donut chart
donut_data <- classified_outcomes |>
  count(outcome) |>
  mutate(
    outcome = factor(outcome, levels = c("Positive", "Neutral", "Negative")),
    percent = n / sum(n),
    ypos = cumsum(percent) - 0.5 * percent
  )

ggplot(donut_data, aes(x = 2, y = percent, fill = outcome)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(n, " (", scales::percent(percent, accuracy = 1), ")"),
        y = ypos),
    size = 4
  ) +
  xlim(0.5, 2.5) +  
  labs(
    title = "Overall Immigration Call Outcomes",
    fill = "Outcome Category"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )