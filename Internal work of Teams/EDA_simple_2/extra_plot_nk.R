### Making a plot - Calls by Hour of Day (Activity Pattern)

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# read in data
all_calls <- read_csv('data/combined_All_Call.csv')
car <- read_csv('data/combined_data.csv')
                
calls_by_hour_all <-
  all_calls %>%
  filter(duration > 0, direction == "TERMINATING", pstn_vendor_name == "CallTower") %>%
  mutate(hour = hour(report_time)) %>%
  count(hour) %>%
  mutate(dataset = 'All Calls')

calls_by_hour_car <-
  car %>%
  mutate(
    activity_start_timestamp = ymd_hms(activity_start_timestamp),
    activity_start_timestamp = format(activity_start_timestamp, "%Y-%m-%d %H:%M:%S"),
    hour = hour(activity_start_timestamp)) %>%
  count(hour) %>%
  mutate(dataset = "CAR")

calls_hour_comp <-
  full_join(calls_by_hour_all, calls_by_hour_car, by = "hour") %>%
  pivot_longer(cols = c(All_Calls, CAR), names_to = "Dataset", values_to = "Count")

hour_share <-
  bind_rows(calls_by_hour_all, calls_by_hour_car) |>
  group_by(dataset) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

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

ggplot(hour_share, aes(hour, pct, color = dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  #scale_x_continuous(breaks = 0.23) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = 'Hourly Share of Inbound Calls',
    subtitle = 'Each line shows distribution within dataset',
    x = 'Hour of Day (24-hour)',
    y = 'Share of Calls',
    color = 'Dataset'
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())
