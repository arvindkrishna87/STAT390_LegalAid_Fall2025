# Load required libraries
library(tidyverse)
library(readxl)   # for reading Excel files
library(lubridate) # for working with dates

# ---- USER INPUT ----
data_path <- "/Users/loganroever/Desktop/stat390.nosync/calls_data/All Calls by Month"
# --------------------

# ---- 1. Get all CSV and XLSX files ----
files <- list.files(path = data_path, pattern = "\\.(csv|xlsx)$", full.names = TRUE) %>%
  sort()

# ---- 2. Peek at the first 5 rows of each file ----
peek_list <- map(files, function(f) {
  if (str_detect(f, "\\.csv$")) {
    read_csv(f, n_max = 5, show_col_types = FALSE)
  } else {
    read_excel(f, n_max = 5)
  }
})

# Print index, filename, and dimensions
walk2(peek_list, seq_along(peek_list), function(df, i) {
  cat(i - 1, basename(files[i]), "->", paste(dim(df), collapse = " x "), "\n")
})

# ---- 3. Find common and missing columns ----
# Get all column names for each file
col_list <- map(peek_list, names)

# Columns present in all files (intersection)
common_cols <- reduce(col_list, intersect)

# Columns missing in some files (union minus intersection)
all_cols <- reduce(col_list, union)
not_in_all <- setdiff(all_cols, common_cols)

cat("Columns missing from at least one dataframe:\n")
print(not_in_all)

cat("Columns present in all dataframes:\n")
print(common_cols)

# ---- 4. Read all data files using only common columns ----

all_calls_data <- map_dfr(files, function(f) {
  if (str_detect(f, "\\.csv$")) {
    read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
      select(any_of(common_cols))
  } else {
    read_excel(f, col_types = "text") %>%   # "text" = character
      select(any_of(common_cols))
  }
}, .id = "file_id")  

# Now convert Start time to datetime
all_calls_data <- all_calls_data %>%
  mutate(`Start time` = ymd_hms(`Start time`, tz = "UTC"))

# ---- 6. Quick check ----
glimpse(all_calls_data)
