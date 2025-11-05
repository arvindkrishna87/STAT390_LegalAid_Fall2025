# Third Presentation
# Nikole Montero 

# Loading libraries 
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

# Reading datasets (Requires accessing Github)
# To read the CAR dataset, please refer to my import code on this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/Importing_CAR_Nikole.R
# To read in the All Calls dataset, please refer to the import code in this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/Importing_AllCallsData_Nikole.ipynb
# Replace with the right path where each of the datasets that you imported are
all_calls <- read_csv("data/combined_All_Call.csv") |> janitor::clean_names()
car <- read_csv("data/car.csv") |> janitor::clean_names()

## Exploring/Finding the queues in Employment Menu
employment <- car |>
  filter(
    flow_name == "LegalEmploymentMenu" | activity_name == "EmploymentMenu",
    activity_start_timestamp >= as.POSIXct("2025-03-16"))

employment |> count(queue_name)
# all rows show "N/A" because after 03/16/2025 all queues are dropped

## Filtering CAR dataset focusing on Employment Menu
car_employment <- car |>
  filter(
    flow_name == "LegalEmploymentMenu" | activity_name == "EmploymentMenu",
    activity_start_timestamp >= as.POSIXct("2025-03-16")
  ) |>
  reframe(
    .by = contact_session_id,
    termination_reason = termination_reason,
    activity_start_timestamp = activity_start_timestamp,
    activity_name = activity_name
  ) |>
  distinct()

## Classifying Employment Menu Calls by positive, neutral and negative outcomes
car_employment <- car_employment |>
  mutate(
    outcome = case_when(
      termination_reason %in% c("Agent Left", "AGENT_ENDS", "Customer Left") ~ "Positive",
      termination_reason %in% c(
        "Queue Timeout", "CUSTOMER_UNAVAILABLE", "MAX_CALLBACK_RETRY_LIMIT_REACHED",
        "NO_ANSWER_FROM_CUSTOMER", "NO_ANSWER_CUSTOMER", "NO_ANSWER_USER",
        "CUSTOMER_BUSY", "USER_UNAVAILABLE", "USER_BUSY", "USER_DECLINED",
        "Participant Invite timer expired", "RONA_TIMER_EXPIRED", "RONA Timer Expired",
        "CONTACT_CALLBACK_IN_PROGRESS"
      ) ~ "Neutral",
      termination_reason %in% c(
        "NO_ANSWER_FROM_AGENT", "AGENT_UNAVAILABLE", "AGENT_BUSY",
        "OUTDIAL_FAILED", "MEDIA_MANAGER_INTERNAL_ERROR", "CHANNEL_FAILURE",
        "System Error", "System disconnected the contact"
      ) ~ "Negative",
      termination_reason %in% c("NA", "N/A") | is.na(termination_reason) ~ "Negative",
      TRUE ~ "Unidentified"
    )
  )

## Housing Menu Calls summarized by outcome category
employment_summary <- car_employment |>
  count(outcome) |>
  mutate(percentage = round(n / sum(n) * 100, 1))

## Plot
employment_plot <- ggplot(employment_summary, aes(x = reorder(outcome, -percentage),
                               y = percentage,
                               fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.4,
            color = "black",
            fontface = "bold",
            size = 4) +
  scale_fill_manual(values = c(
    "Positive" = "#5C8D89",       # muted green
    "Neutral" = "#B1B6BE",        # soft gray
    "Negative" = "#CFA07E",       # muted brown/taupe
    "Unidentified" = "#708090"    # slate gray
  )) +
  labs(
    title = "Employment Menu Outcomes",
    x = "Outcome Category",
    y = "Percentage of Calls (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  )

# Saving plot
ggsave(
  filename = "employment_menu_outcomes.png",
  plot = employment_plot, 
  width = 8, height = 5,
  dpi = 300
)



