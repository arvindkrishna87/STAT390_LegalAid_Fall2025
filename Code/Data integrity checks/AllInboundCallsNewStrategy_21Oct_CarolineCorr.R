## Presentation 2, Task 1: Using Edwin Rong's strategy for filtering 
## inbound calls in all calls dataset

# libraries
library(tidyverse)
library(lubridate)

# read in data
# note for reproducibility: 
# CAR data synthesized through CAR_data_import_R_CarolineCorr.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/CAR_data_import_R_CarolineCorr.R
# All Calls data synthesized through allcallsimport_Oct7_LoganRoever.R:
# https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/allcallsimport_Oct7_LoganRoever.R
all_calls <- read_csv('data/all_calls.csv') |> janitor::clean_names()
car <- read_csv('data/combined_data.csv') |> janitor::clean_names()

# edit datasets for monthly inbound call comparison ----

# overwrite all calls for correct timezone and separate out month/year columns
all_calls <-
  all_calls |>
  mutate(
    # with_tz function automatically handles daylight saving time adjustments
    report_time = with_tz(start_time, tzone = 'America/Chicago'),
    month = month(start_time, label = TRUE, abbr = FALSE),
    year = year(start_time),
    # create month_year to join and compare between tables
    month_year = paste0(month, ' ', year)
  ) 

# grab all inbound calls from 6 phone lines using Edwin Rong's strategy (see documentation)
calls_by_month_all <-
  all_calls |>
  filter(duration > 0, 
         is.na(inbound_trunk),
         str_starts(outbound_trunk, 'wcc'),
         direction == "TERMINATING",
         call_type == 'SIP_INBOUND',
         client_type == 'WXCC',
         pstn_vendor_name == 'CallTower',
         called_number %in% c('13124312299',
                              '13123411070',
                              '13125068646',
                              '13125068647',
                              '13122296080',
                              '13123478342')
  ) |>
  distinct(correlation_id, .keep_all = TRUE) |>
  summarize(
    .by = month_year,
    n = n()
  )

# get calls by month/year from CAR
calls_by_month_car <-
  car |>
  mutate(
    # with_tz function automatically handles daylight saving time adjustments
    activity_start_timestamp = with_tz(activity_start_timestamp, tzone = 'America/Chicago'),
    month = month(activity_start_timestamp, label = TRUE, abbr = FALSE),
    year = year(activity_start_timestamp),
    # create month_year to join and compare between tables
    month_year = paste0(month, ' ', year)
  ) |>
  distinct(contact_session_id, .keep_all = TRUE) |>
  summarize(
    .by = month_year,
    n = n()
  )

# join month/year breakdowns for both datasets ----
month_comp_data <-
  inner_join(calls_by_month_car, calls_by_month_all, by = join_by(month_year)) |>
  mutate(month_year = factor(month_year, levels =
                               c('April 2024', 'May 2024', 'June 2024', 'July 2024',
                                 'August 2024', 'September 2024', 'October 2024',
                                 'November 2024', 'December 2024', 'January 2025',
                                 'February 2025', 'March 2025', 'April 2025', 'May 2025',
                                 'June 2025', 'July 2025', 'August 2025', 'September 2025'))) |>
  rename(CAR = n.x, `All Calls` = n.y) |>
  pivot_longer(
    cols = c(CAR, `All Calls`),
    names_to = 'dataset',
    values_to = 'count'
  ) 

# plot comparison
month_comp_data |>
  ggplot(aes(month_year, count, color = dataset, group = dataset)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = 'Inbound Call Counts: Comparison May \'24 to September \'25',
    x = 'Month',
    y = 'Number of Inbound Calls',
    color = 'Dataset',
    caption = 'All Calls Filtered with Edwin Rong\'s Recommendation'
  )
