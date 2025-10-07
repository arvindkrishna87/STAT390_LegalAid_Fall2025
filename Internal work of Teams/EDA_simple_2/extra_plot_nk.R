### Making a plot - Calls by Hour of Day (Activity Pattern)

library(ggplot2)
library(dplyr)
library(lubridate)

# read in data
all_calls <- read_csv('data/combined_All_Call.csv')
car <- read_csv('data/combined_data.csv')
                
calls_by_hour_all <-
  all_calls %>%
  filter(duration > 0, direction == "TERMINATING", pstn_vendor_name == "CallTower") %>%
  mutate(hour = hour(report_time)) %>%
  count(hour) %>%
  rename(All_Calls = n)

calls_by_hour_car <-
  car %>%
  mutate(
    activity_start_timestamp = ymd_hms(activity_start_timestamp),
    activity_start_timestamp = format(activity_start_timestamp, "%Y-%m-%d %H:%M:%S"),
    hour = hour(activity_start_timestamp)) %>%
  count(hour) %>%
  rename(CAR = n)

calls_hour_comp <-
  full_join(calls_by_hour_all, calls_by_hour_car, by = "hour") %>%
  pivot_longer(cols = c(All_Calls, CAR), names_to = "Dataset", values_to = "Count")

ggplot(calls_hour_comp, aes(x = hour, y = Count, color = Dataset)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Hourly Distribution of Inbound Calls",
    x = "Hour of Day (24-hour)",
    y = "Number of Calls",
    color = "Dataset"
  ) +
  theme_minimal()
