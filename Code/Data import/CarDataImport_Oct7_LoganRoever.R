# load in libraries 
library(tidyverse)


## USER INPUT REQUIRED -- write local path here
data_path <- "/Users/loganroever/Desktop/stat390.nosync/calls_data/CAR_-_EP_Flow_Activity_Queue__Agent_Names"
# --------------------

# Get all CSV and XLSX files 
files <- list.files(path = data_path, pattern = "\\.(csv|xlsx)$", full.names = TRUE) %>%
  sort()

# Define column names ----

desired_cols <- c(
  "Contact Session ID",
  "EP Name",
  "Flow Name",
  "Activity Name",
  "Activity Start Timestamp",
  "Queue Name",
  "Agent Name",
  "Termination Reason"
)

# Read and combine all files ----
# The first 2 rows are blank, so skip = 2
car_data <- map_dfr(files, function(f) {
  
  if (str_detect(f, "\\.csv$")) {
    df <- read_csv(f,
                   skip = 2,                     
                   col_types = cols(.default = "c"),
                   show_col_types = FALSE)
  } else {
    df <- read_excel(f, skip = 2, col_types = "text")
  }
  
  # Keep only desired columns if they exist
  df %>%
    select(any_of(desired_cols))
  
}, .id = "file_id")   # keep track of source file

# Parse "Activity Start Timestamp" as datetime --
car_data <- car_data %>%
  mutate(
    `Activity Start Timestamp` =
      parse_date_time(`Activity Start Timestamp`,
                      orders = "Y/m/d I:M:S p",   # matches e.g. 2025/03/02 02:15:34 PM
                      tz = "UTC")
  )

# Create an hour column for peak calling analysis ----
car_data <- car_data %>%
  mutate(hour = hour(`Activity Start Timestamp`))

# Quick structure check ----
glimpse(car_data)
