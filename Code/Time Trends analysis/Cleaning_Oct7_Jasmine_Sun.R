library(tidyverse)
library(lubridate)
library(dplyr)

# AllCalls data and CAR data were both aggregated using Professor Krish's ipynb code


# cleaning allcalls data----
allcallsdata = read_csv("AllCallsData.csv")
 
allcallsdata |>
  select(contains("time")) |>
  head()

to_chicago_posix <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  x_utc <- if (is.null(attr(x, "tzone")) || attr(x, "tzone") == "") force_tz(x, "UTC") else x
  with_tz(x_utc, "America/Chicago")
}

allcallsdata <- allcallsdata %>%
  mutate(across(c('Start time', 'Call transfer time', 'Release time', 'Report time', 'Answer time'), to_chicago_posix))


write.csv(allcallsdata, "AllCallsData_cleaned.csv", row.names = FALSE)

# cleaning CAR data-----
car_data = read_csv("CAR.csv")

car_data_transformed <- car_data |>
  mutate(
    year = year(`Activity Start Timestamp`),
    month = month(`Activity Start Timestamp`, label = TRUE, abbr = TRUE),  
    day = day(`Activity Start Timestamp`),
    day_of_week = wday(`Activity Start Timestamp`, label = TRUE, abbr = TRUE),
    hour = hour(`Activity Start Timestamp`),
    minute = minute(`Activity Start Timestamp`),
    second = second(`Activity Start Timestamp`),
    half_hour_nearest = round_date(`Activity Start Timestamp`, unit = "30 minutes"),
    nearest_half_hour_bin = format(half_hour_nearest, "%H:%M"), # binning by half hour
    `Queue Name` = trimws(gsub("\\bSP\\b", "", `Queue Name`, ignore.case = FALSE)) #trimming 'SP' in Queue Names
    )|>
  select(-half_hour_nearest) |>
  mutate(`Queue Name` = recode(`Queue Name`, # synthesizing subsenior queue names
                               "ADAPT SubSeniors" = "SubSenior ADAPT",
                               "Homeowner SubSeniors" = "SubSenior Homeowner",
                               "Family SubSeniors" = "SubSenior Family",
                               "Other SubSeniors" = "SubSenior Other", 
                               "Consumer SubSeniors" = "SubSenior Consumer", 
                               "Employment SubSeniors" = "SubSenior Employment", 
                               "Benefits SubSeniors" = "SubSenior Benefits", 
                               "Housing SubSeniors" = "SubSenior Housing"))
  

  

write.csv(car_data_transformed, "CAR_cleaned.csv", row.names = FALSE)


## creating queue wait times datset-------

call_time_data <- car_data_transformed |>
  arrange(`Contact Session ID`, `Activity Start Timestamp`) |>
  group_by(`Contact Session ID`) |>
  slice_min(`Activity Start Timestamp`, with_ties = FALSE) |>  #keep only earliest entry
  ungroup() |>
  select(`Contact Session ID`, `Activity Start Timestamp`) |>
  rename('call_start_time' = `Activity Start Timestamp`)


enter_queue_time_data <- car_data_transformed |>
  filter(str_detect(str_to_lower(`Activity Name`), "prequeue")) |>
  arrange(`Contact Session ID`, `Activity Start Timestamp`) |>
  group_by(`Contact Session ID`) |>
  slice_min(`Activity Start Timestamp`, with_ties = FALSE) |>  
  ungroup() |>
  select(`Contact Session ID`, `Activity Start Timestamp`) |>
  rename('enter_queue_time' = `Activity Start Timestamp`)


callback_time_data <- car_data_transformed |>
  filter(str_detect(str_to_lower(`Flow Name`), "courtesycallback")) |>
  arrange(`Contact Session ID`, `Activity Start Timestamp`) |>
  group_by(`Contact Session ID`) |>
  slice_min(`Activity Start Timestamp`, with_ties = FALSE) |>  
  ungroup() |>
  select(`Contact Session ID`, `Activity Start Timestamp`) |>
  rename('callback_time' = `Activity Start Timestamp`)

wait_data <- call_time_data |>
  inner_join(enter_queue_time_data, by = "Contact Session ID") |>
  inner_join(callback_time_data, by = "Contact Session ID") |>
  mutate(
    call_start_time        = ymd_hms(call_start_time),
    enter_queue_time = ymd_hms(enter_queue_time),
    callback_time    = ymd_hms(callback_time),
    
    wait_minutes = as.numeric(callback_time - enter_queue_time, units = "secs") / 60,
    wait_bin_15m = floor(wait_minutes / 15) * 15,
    wait_bin_15m = paste0(wait_bin_15m, "–", wait_bin_15m + 15, " min")
  )

write.csv(wait_data, "queue_wait_data.csv", row.names = FALSE)



