# load in libraries 
library(tidyverse)
library(readxl)

## To reproduce this code - write the local path here of the CAR dataset downloaded from Krish's data drive
data_path <- "/Users/nikolemonterocervantes/Documents/stat390/CAR_-_EP_Flow_Activity_Queue__Agent_Names"

# Getting all CSV and XLSX files 
files <- list.files(path = data_path, pattern = "\\.(csv|xlsx)$", full.names = TRUE) %>%
  sort()

# Defining column names

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

# Reading and combining all files
# The first 2 rows are blank, so we skip them (skip = 2)
car_data <- map_dfr(files, function(f) {
  
  if (str_detect(f, "\\.csv$")) {
    df <- read_csv(f,
                   skip = 2,                     
                   col_types = cols(.default = "c"),
                   show_col_types = FALSE)
  } else {
    df <- read_excel(f, skip = 2, col_types = "text")
  }
  
  # Keeping only desired columns if they exist
  df %>%
    select(any_of(desired_cols))
  
}, .id = "file_id")

# Parsing "Activity Start Timestamp" as datetime
car_data <- car_data %>%
  mutate(
    `Activity Start Timestamp` =
      parse_date_time(`Activity Start Timestamp`,
                      orders = "Y/m/d I:M:S p",   # for examples: 2025/03/02 02:15:34 PM
                      tz = "UTC")
  )

# Creating an hour column for peak calling analysis
car_data <- car_data %>%
  mutate(hour = hour(`Activity Start Timestamp`))

# Structure check and glimpsing the dataset
glimpse(car_data)

# Saving the file on destokp -- You should replace the path with the one you want to have the csv dataset saved in
write.csv(car_data, file = 'data/car.csv')
