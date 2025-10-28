### Making a plot - Calls by Hour of Day (Activity Pattern)
## Loading packages
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(janitor)

# Reading datasets (Requires accessing Github)
## To read the CAR dataset, please refer to my import code on this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/CarDataImport_Oct21st_Nikole.R
## To read in the All Calls dataset, please refer to the import code in this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/AllCallsDataImport_Oct21st_Nikole.ipynb
all_calls <- read_csv("data/combined_All_Call.csv") |> clean_names()
car <- read_csv("data/car.csv") |> clean_names()

# Defining six phone lines and timezone
six_lines <- c("13124312299",
               "13123411070",
               "13125068646",
               "13125068647",
               "13122296080",
               "13123478342")
tz_local <- "America/Chicago"

# Filtering & summarizing All Calls (doing a timezone conversion)
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

# Summarizing CAR (there is no tz conversion, to keep consistency)
calls_by_hour_car <-
  car %>%
  distinct(contact_session_id, .keep_all = TRUE) %>%
  mutate(hour = hour(ymd_hms(activity_start_timestamp))) %>%
  count(hour, name = "n") %>%
  mutate(dataset = "CAR")

# Combining & computing share
hour_share <-
  bind_rows(calls_by_hour_all, calls_by_hour_car) %>%
  group_by(dataset) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Generating plot: Hourly share
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

