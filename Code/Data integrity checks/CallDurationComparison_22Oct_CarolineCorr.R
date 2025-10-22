## Task 1: Consistency check for call durations after March 15, 2025 ----

# libraries 
library(tidyverse)
library(lubridate)

# read in data
# note for reproducibility: 
# CAR data synthesized through CAR_data_import_R_CarolineCorr.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/CAR_data_import_R_CarolineCorr.R
# All Calls data synthesized through allcallsimport_Oct7_LoganRoever.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/allcallsimport_Oct7_LoganRoever.R
car <- read_csv('data/combined_data.csv') |> janitor::clean_names()
all_calls <- read_csv('data/combined_All_Call.csv') |> janitor::clean_names()

# filter both datasets to only keep calls made after March 15
car <-
  car |>
  mutate(activity_start_timestamp = ymd_hms(activity_start_timestamp),
         hour = hour(activity_start_timestamp),
         ) |>
  filter(activity_start_timestamp >= '2025/03/16 00:00:00')

all_calls <-
  all_calls |>
  mutate(report_time = with_tz(report_time, tzone = 'America/Chicago'),
         release_time = with_tz(release_time, tzone = 'America/Chicago'),
         hour = hour(report_time)) |>
  filter(report_time >= '2025/03/16 00:00:00')

# make duration variables
car_durations <-
car |>
  summarize(
    .by = contact_session_id,
    start_time = min(activity_start_timestamp, na.rm = TRUE),
    end_time = max(activity_start_timestamp, na.rm = TRUE),
    duration = end_time - start_time
  ) |>
  mutate(
    hour = hour(start_time))

car_durations <-
  car_durations |>
  mutate(duration = parse_number(as.character(duration)))

all_calls_durations <-
all_calls |>
  # get inbound calls only for comparison
  filter(duration > 0, 
         direction == "TERMINATING",
         pstn_vendor_name == 'CallTower',
         called_number %in% c('13124312299',
                              '13123411070',
                              '13125068646',
                              '13125068647',
                              '13122296080',
                              '13123478342')
  ) |>
  summarize(
    .by = correlation_id,
    report_time = report_time,
    release_time = release_time,
    duration_custom = release_time - report_time,
    duration = duration
  ) |>
  mutate(hour = hour(report_time))
# report time and release time are the same thing...

# edit datasets to combine into one for plot ----
dur_by_hour_car <-
car_durations |>
  summarize(
    .by = hour,
    avg_duration = mean(duration, na.rm = TRUE)
  ) |>
  mutate(dataset = "CAR")

dur_by_hour_all_calls <-
all_calls_durations |>
  summarize(
    .by = hour,
    avg_duration = mean(duration, na.rm = TRUE)
  ) |>
  mutate(dataset = "All Calls")

dur_by_hour <-
  bind_rows(dur_by_hour_car, dur_by_hour_all_calls)
  
# make plot ----
dur_by_hour |>
  ggplot(aes(hour, avg_duration, color = dataset)) +
  geom_line(linewidth = 1)

