library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(tibble)
install.packages("janitor")
library(janitor)

all_calls <- read_csv("all_calls.csv")
glimpse(all_calls)


# --- Step 1: Set open and close hours
open_hr <- "08:45"
close_hr <- "17:00"

# --- Step 2: Map number descriptions
number_descriptions <- tribble(
  ~called_number, ~description,
  "13123478300", "Internal voicemail - not client related",
  "13123411070", "Main number",
  "2302", "Staff Directory English Transfer",
  "13124235938", "legalclinics (Community Legal Clinics)",
  "13122296300", "Direct Line to Front Desk? Who is this given to?",
  "13125068649", "13125068649",
  "1180", "Transfers to English Queue Options (e.g. Legal Menu)",
  "13125068646", "Transfers to the English main menu",
  "13124312299", "Farmworker main number calls; Migrant Legal Assistance Program",
  "13122296346", "13122296346",
  "13123478334", "13123478334",
  "2303", "2303",
  "13122296342", "13122296342",
  "13122296079", "Nursing Home Ombudsman",
  "13122296344", "Bankruptcy Helpdesk VM",
  "13122296071", "Criminal Records",
  "13122296321", "13122296321",
  "13122296341", "13122296341",
  "13124312151", "13124312151",
  "1182", "1182",
  "13125068647", "Transfers to the Spanish main menu",
  "13122296001", "13122296001",
  "13124235913", "13124235913",
  "13123478340", "Veterans Rights Project VM",
  "*11", "*11",
  "13122296386", "13122296386",
  "18002727442", "18002727442",
  "13122296014", "Markham Eviction Help Desk",
  "13122296343", "13122296343",
  "13123478326", "13123478326",
  "13122296336", "13122296336",
  "13123478309", "HIV Intake VM",
  "13123478312", "13123478312",
  "13123478306", "13123478306",
  "13122296023", "13122296023",
  "13122296072", "JEHD (Juvenile Expungement Help Desk)",
  "13122296348", "13122296348",
  "13124235904", "Austin Intake VM",
  "13124506721", "13124506721",
  "13122296358", "13122296358",
  "13122296373", "13122296373",
  "13122296315", "13122296315",
  "13123478387", "13123478387",
  "13123478375", "13123478375",
  "13123478385", "13123478385",
  "13122296365", "13122296365",
  "13122296041", "13122296041",
  "13122296374", "13122296374",
  "13123478336", "13123478336"
)


# --- Step 3: Process & Aggregate Data
presentation2_df <- all_calls |> 
  # Clean column names
  clean_names() |> 
  
  # Select all the columns needed for the analysis
  select(correlation_id, called_number, start_time, 
         duration, direction, pstn_vendor_name) |> 
  
  # Filter out NA rows, convert duration to minutes
  mutate(called_number = as.character(called_number),
         duration_min = duration / 60) |>
  filter(!is.na(`called_number`)) |> 
  
  # Remove duplicate call logs
  distinct(correlation_id, called_number, .keep_all = TRUE) |>
  distinct(correlation_id, start_time, duration, .keep_all = TRUE) |>
  
  # --- Step 4: Create All Filter Columns ---
  mutate(
    
    # A. Create the 'Call_Path' column 
    call_path = case_when(
      is.na(pstn_vendor_name) ~ "Internal",
      !is.na(pstn_vendor_name) & direction == "TERMINATING" ~ "Inbound",
      !is.na(pstn_vendor_name) & direction == "ORIGINATING" ~ "Outbound",
      TRUE ~ "Other"
    ),
    
    # B. Create date column for the date slicer
    date = as.Date(start_time),
    
    # C. Create columns for weekend/weekday & open/closed hours slicers
    call_hour = hour(start_time),
    call_weekday = wday(start_time, label = TRUE, week_start = 1),
    weekday_weekend = if_else(
      call_weekday %in% c("Sat", "Sun"), 
      "Weekend", 
      "Weekday"
    ),
    open_closed = if_else(
      call_hour >= open_hr & call_hour < close_hr,
      "Open Hours",
      "Closed Hours"
    )
  ) |> 
  
  # --- Step 5: Join the Number Descriptions ---
  left_join(
    number_descriptions,
    by = "called_number"
  ) |>
  mutate(
    description = coalesce(description, called_number)
  ) |>
  
  # --- Step 6: Final Cleaning ---
  # Reorder columns 
  select(
    start_time,
    date,
    weekday_weekend,
    correlation_id,
    open_closed,
    call_path,
    called_number,
    description,
    duration,
    duration_min
  )

# --- Step 7: Export to CSV ---
write.csv(
  presentation2_df, 
  "presentation2_df.csv", 
  row.names = FALSE
)