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
    nearest_half_hour_bin = format(half_hour_nearest, "%H:%M"),
    `Queue Name` = trimws(gsub("\\bSP\\b", "", `Queue Name`, ignore.case = FALSE))
    )|>
  select(-half_hour_nearest)

write.csv(car_data_transformed, "CAR_cleaned.csv", row.names = FALSE)


