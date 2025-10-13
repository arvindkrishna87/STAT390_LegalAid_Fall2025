### Making a plot - Calls by Hour of Day (Activity Pattern)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(janitor)

# --- Read & clean data ---
all_calls <- read_csv("data/combined_All_Call.csv") |> clean_names()
car <- read_csv("data/combined_data.csv") |> clean_names()

# --- Define six phone lines and timezone ---
six_lines <- c("13124312299",
               "13123411070",
               "13125068646",
               "13125068647",
               "13122296080",
               "13123478342")
tz_local <- "America/Chicago"

# --- Filter & summarize All Calls ---
calls_by_hour_all <-
  all_calls %>%
  mutate(report_time = with_tz(as_datetime(report_time, tz = "UTC"), tzone = "America/Chicago")) %>%
  filter(duration > 0,
         direction == "TERMINATING",
         pstn_vendor_name == "CallTower",
         called_number %in% six_lines) %>%
  distinct(correlation_id, .keep_all = TRUE) %>%
  mutate(hour = hour(ymd_hms(report_time))) %>%
  count(hour, name = "n") %>%
  mutate(dataset = "All Calls")

# --- Summarize CAR (no tz conversion, to keep consistent) ---
calls_by_hour_car <-
  car %>%
  distinct(contact_session_id, .keep_all = TRUE) %>%
  mutate(hour = hour(ymd_hms(activity_start_timestamp))) %>%
  count(hour, name = "n") %>%
  mutate(dataset = "CAR")

# --- Combine & compute share ---
hour_share <-
  bind_rows(calls_by_hour_all, calls_by_hour_car) %>%
  group_by(dataset) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# --- Plot: Hourly Share ---
ggplot(hour_share, aes(hour, pct, color = dataset)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Hourly Share of Inbound Calls",
    x = "Hour of Day (24-hour)",
    y = "Share of Calls",
    color = "Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

# # --- Read & clean data ---
# all_calls <- read_csv("data/combined_All_Call.csv") |> clean_names()
# car <- read_csv("data/combined_data.csv") |> clean_names()
# 
# # --- Define constants ---
# tz_local <- "America/Chicago"
# six_lines <- c("13124312299", "13123411070", "13125068646",
#                "13125068647", "13122296080", "13123478342")
# 
# # --- Prepare All Calls dataset ---
# calls_by_hour_all <-
#   all_calls |>
#   mutate(report_time = with_tz(as_datetime(report_time, tz = "UTC"), tzone = "America/Chicago")) |>
#   filter(duration > 0,
#          direction == "TERMINATING",
#          pstn_vendor_name == "CallTower",
#          called_number %in% six_lines) |>
#   distinct(correlation_id, .keep_all = TRUE) |>
#   mutate(hour = hour(report_time)) |>
#   count(hour, name = "n") |>
#   mutate(dataset = "All Calls")
# 
# # --- CAR (already local, no tz conversion) ---
# calls_by_hour_car <-
#   car |>
#   mutate(activity_start_timestamp = as_datetime(activity_start_timestamp),  # no need for timezone correction
#          hour = hour(activity_start_timestamp)) |>
#   distinct(contact_session_id, .keep_all = TRUE) |>
#   count(hour, name = "n") |>
#   mutate(dataset = "CAR")
# 
# # --- Combine datasets ---
# calls_by_hour <- bind_rows(calls_by_hour_all, calls_by_hour_car)
# 
# # --- Plot ---
# ggplot(calls_by_hour, aes(x = hour, y = n, color = dataset)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   scale_x_continuous(breaks = 0:23) +
#   labs(
#     title = "Calls by Hour of Day (Activity Pattern)",
#     x = "Hour of Day (24-hour, America/Chicago)",
#     y = "Number of Inbound Calls",
#     color = "Dataset"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(legend.position = "top",
#         panel.grid.minor = element_blank())

# library(ggplot2)
# library(dplyr)
# library(lubridate)
# library(scales)
# 
# # read in data
# all_calls <- read_csv('data/all_calls.csv')
# car <- read_csv('data/CAR.csv')
#                 
# calls_by_hour_all <-
#   all_calls %>%
#   filter(duration > 0, direction == "TERMINATING", pstn_vendor_name == "CallTower") %>%
#   mutate(hour = hour(report_time)) %>%
#   count(hour) %>%
#   mutate(dataset = 'All Calls')
# 
# calls_by_hour_car <-
#   car %>%
#   mutate(
#     activity_start_timestamp = ymd_hms(activity_start_timestamp),
#     activity_start_timestamp = format(activity_start_timestamp, "%Y-%m-%d %H:%M:%S"),
#     hour = hour(activity_start_timestamp)) %>%
#   count(hour) %>%
#   mutate(dataset = "CAR")
# 
# calls_hour_comp <-
#   full_join(calls_by_hour_all, calls_by_hour_car, by = "hour") %>%
#   pivot_longer(cols = c(All_Calls, CAR), names_to = "Dataset", values_to = "Count")
# 
# hour_share <-
#   bind_rows(calls_by_hour_all, calls_by_hour_car) |>
#   group_by(dataset) |>
#   mutate(pct = n / sum(n)) |>
#   ungroup()
# 
# ggplot(calls_hour_comp, aes(x = hour, y = Count, color = Dataset)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   labs(
#     title = "Hourly Distribution of Inbound Calls",
#     x = "Hour of Day (24-hour)",
#     y = "Number of Calls",
#     color = "Dataset"
#   ) +
#   theme_minimal()
# 
# ggplot(hour_share, aes(hour, pct, color = dataset)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   #scale_x_continuous(breaks = 0.23) +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   labs(
#     title = 'Hourly Share of Inbound Calls',
#     subtitle = 'Each line shows distribution within dataset',
#     x = 'Hour of Day (24-hour)',
#     y = 'Share of Calls',
#     color = 'Dataset'
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(legend.position = 'top',
#         panel.grid.minor = element_blank())
