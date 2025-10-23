library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)

CAR_hierarchy_clean <- CAR_combined |> clean_names()

#adding submenu count row
CAR_hierarchy_clean$submenu_count <- ave(
  seq_along(CAR_hierarchy_clean$contact_session_id), 
  CAR_hierarchy_clean$contact_session_id, 
  FUN = length
) - 1

#creating unknown values 
CAR_hierarchy_clean <- CAR_hierarchy_clean |> 
  mutate(
    ep_name = replace_na(ep_name, "Unknown EP"),
    flow_name = replace_na(flow_name, "Unknown Flow"),
    activity_name = replace_na(activity_name, "Unknown Activity")
  )

#ensuring correct data types
CAR_hierarchy_clean <- CAR_hierarchy_clean |> 
  mutate(
    ep_name = as.character(ep_name),
    flow_name = as.character(flow_name),
    activity_name = as.character(activity_name)
  )

CAR_hierarchy_clean |> 
  filter(is.na(ep_name) | ep_name == "" | ep_name == "Unknown EP") %>%
  select(ep_name, flow_name, activity_name) %>%
  head(20)

# updating NAs below:
library(dplyr)
library(tidyr)
library(lubridate)

CAR_hierarchy_clean <- CAR_hierarchy_clean %>%
  # Ensure proper time order within each call
  mutate(activity_start_timestamp = mdy_hm(activity_start_timestamp)) %>%
  arrange(contact_session_id, activity_start_timestamp) %>%
  
  # --- EP fix ---
  # Promote EP when flow is Queues / ClosedQueueMenu
  mutate(
    ep_name = case_when(
      flow_name == "Queues" ~ "Queues",
      flow_name == "ClosedQueueMenu" ~ "ClosedQueueMenu",
      TRUE ~ ep_name
    )
  ) %>%
  group_by(contact_session_id) %>%
  mutate(
    ep_name = na_if(ep_name, "Unknown EP"),
    ep_name = na_if(ep_name, "")
  ) %>%
  fill(ep_name, .direction = "down") %>%   # carry forward
  fill(ep_name, .direction = "up") %>%     # (ok to backfill EPs)
  ungroup() %>%
  mutate(ep_name = replace_na(ep_name, "Unknown EP")) %>%
  
  # --- Flow fix ---
  # Keep the FIRST flow as Unknown if it started that way; only fill DOWN
  group_by(contact_session_id) %>%
  mutate(
    flow_name = na_if(flow_name, "Unknown Flow"),
    flow_name = na_if(flow_name, "")
  ) %>%
  fill(flow_name, .direction = "down") %>% # do NOT fill up; preserves first Unknown
  ungroup() %>%
  mutate(flow_name = replace_na(flow_name, "Unknown Flow"))

write_csv(CAR_hierarchy_clean, "/Users/liviasalituro/Desktop/all/STAT_390/CAR_hierarchy_clean.csv")

