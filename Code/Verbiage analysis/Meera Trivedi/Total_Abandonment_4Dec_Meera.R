# FULL ABANDONMENT

library(tidyverse)
library(writexl)
library(janitor)
library(readxl)
library(hms)

krish_full_file <- read_csv("/Users/meeratrivedi/Downloads/STAT 390/krish_cleaning_full.csv") %>% 
  clean_names()

krish_full_file %>% 
  count(contact_session_id) #248,442 unique contact session ids

df_main_copy <- krish_full_file %>%
  arrange(contact_session_id, activity_start_timestamp) %>%     
  group_by(contact_session_id) %>% 
  fill(activity_name, .direction = "down") %>% 
  fill(ep_name, .direction = "down") %>% 
  ungroup()


all_last_rows <- df_main_copy %>% 
  group_by(contact_session_id) %>% 
  slice_tail(n = 1) %>%
  ungroup()

activity_counts <- all_last_rows %>% 
  count(activity_name) %>% 
  arrange(desc(n))

count_table <- all_last_rows %>% 
  group_by(activity_name) %>% 
  count(activity_name, ep_name) %>% 
  view()


df_main_analysis <- df_main_copy %>% 
  arrange(contact_session_id, activity_start_timestamp) %>%
  group_by(contact_session_id) %>%
  mutate(
    last_ep = lag(ep_name),
    last_ep = zoo::na.locf(last_ep, na.rm = FALSE), 
    # identify runs of constant last_ep
    last_ep_change = last_ep != lag(last_ep, default = first(last_ep)),
    last_ep_run_id = cumsum(last_ep_change),
    
    # collapse runs → distinct sequence of last_ep
    last_ep_distinct_seq = ave(last_ep, last_ep_run_id, FUN = function(x) x[1]),
    
    # FIRST distinct previous EP
    last_distinct_before_last_ep = lag(last_ep_distinct_seq)
  ) %>% 
  select(-last_ep_change, -last_ep_run_id, -last_ep_distinct_seq)


df_main_analysis <- df_main_analysis %>%
  group_by(contact_session_id) %>%
  mutate(
    # last distinct before last_ep (already exists)
    last_distinct_before_last_ep_prev = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      if (length(x) < 2) NA_character_ else rle(x)$values %>% tail(2) %>% .[1]
    },
    # the distinct EP **before** last_distinct_before_last_ep_prev
    last_distinct_before_last_ep_prev2 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 3) {
        NA_character_  # fewer than 3 distinct EPs, nothing to take
      } else {
        tail(runs, 3)[1]  # third-to-last distinct EP
      }
    }, 
    last_distinct_before_last_ep_prev3 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 4) {
        NA_character_  # fewer than 4 distinct EPs, nothing to take
      } else {
        tail(runs, 4)[1]  # fourth-to-last distinct EP (one before prev2)
      }
    }, 
    last_distinct_before_last_ep_prev4 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 5) {
        NA_character_  # fewer than 5 distinct EPs, nothing to take
      } else {
        tail(runs, 5)[1]  # fifth-to-last distinct EP (one before prev3)
      }
    }, 
    
    last_distinct_before_last_ep_prev5 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 6) {
        NA_character_  # fewer than 5 distinct EPs, nothing to take
      } else {
        tail(runs, 6)[1]  # fifth-to-last distinct EP (one before prev3)
      }
    }, 
    last_distinct_before_last_ep_prev6 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 7) {
        NA_character_  # fewer than 5 distinct EPs, nothing to take
      } else {
        tail(runs, 7)[1]  # fifth-to-last distinct EP (one before prev3)
      }
    }, 
    last_distinct_before_last_ep_prev7 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 8) {
        NA_character_  # fewer than 5 distinct EPs, nothing to take
      } else {
        tail(runs, 8)[1]  # fifth-to-last distinct EP (one before prev3)
      }
    }, 
    last_distinct_before_last_ep_prev8 = {
      x <- last_distinct_before_last_ep
      x <- x[!is.na(x)]
      runs <- rle(x)$values
      if (length(runs) < 9) {
        NA_character_  # fewer than 5 distinct EPs, nothing to take
      } else {
        tail(runs, 9)[1]  # fifth-to-last distinct EP (one before prev3)
      }
    }
    
  ) %>%
  ungroup()

all_last_rows2 <- df_main_analysis %>% 
  group_by(contact_session_id) %>% 
  slice_tail(n = 1) %>%
  ungroup()


specific_menus <- c("Legal Family Menu Telephony EP", 
  "Legal Benefits Menu Telephony EP",
  "Legal Employment Menu Telephony EP", 
  "Legal Housing Menu Telephony EP", 
  "Legal HIV Menu Telephony EP", 
  "Legal Immigration Menu Telephony EP", 
  "Pre-Legal Menu Seniors Menu Telephony EP",
  "Legal Menu Telephony EP", 
  "Intake Outdial EP", 
  "Farmworker Main Number Telephony EP",
  "Main Number Telephony EP", 
  "Other Legal Menu Telephony EP")

main_menus <- c("Closed Hours-Holidays Menu Telephony EP", 
                "Closed Queue Menu Telephony EP", 
                "Courtesy Callback Telephony EP", 
                "All LAC Queues Telephony EP")

count_table <- all_last_rows2 %>% 
  group_by(activity_name) %>% 
  count(activity_name, ep_name, last_ep, last_distinct_before_last_ep, 
        last_distinct_before_last_ep_prev, 
        last_distinct_before_last_ep_prev2, 
        last_distinct_before_last_ep_prev3, 
        last_distinct_before_last_ep_prev4, 
        last_distinct_before_last_ep_prev5, 
        last_distinct_before_last_ep_prev6, 
        last_distinct_before_last_ep_prev7, 
        last_distinct_before_last_ep_prev8) %>%
  mutate(analysis = case_when(ep_name %in% specific_menus ~ ep_name, 
                              
                              ep_name %in% main_menus & 
                                last_ep == ep_name &
                                last_distinct_before_last_ep %in% specific_menus 
                              ~ last_distinct_before_last_ep,
                              
                              ep_name %in% main_menus & last_ep %in% specific_menus ~ last_ep,
                              
                              ep_name %in% main_menus & 
                                last_ep == ep_name &
                                last_distinct_before_last_ep == ep_name &
                                last_distinct_before_last_ep_prev %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev,
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev,
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% main_menus &
                                last_distinct_before_last_ep_prev2 %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev2, 
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% main_menus &
                                last_distinct_before_last_ep_prev2 %in% main_menus &
                                last_distinct_before_last_ep_prev3 %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev3, 
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% main_menus &
                                last_distinct_before_last_ep_prev2 %in% main_menus &
                                last_distinct_before_last_ep_prev3 %in% main_menus &
                                last_distinct_before_last_ep_prev4 %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev4, 
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% main_menus &
                                last_distinct_before_last_ep_prev2 %in% main_menus &
                                last_distinct_before_last_ep_prev3 %in% main_menus &
                                last_distinct_before_last_ep_prev4 %in% main_menus &
                                last_distinct_before_last_ep_prev5 %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev5, 
                              
                              ep_name %in% main_menus & last_ep %in% main_menus &
                                last_distinct_before_last_ep %in% main_menus &
                                last_distinct_before_last_ep_prev %in% main_menus &
                                last_distinct_before_last_ep_prev2 %in% main_menus &
                                last_distinct_before_last_ep_prev3 %in% main_menus &
                                last_distinct_before_last_ep_prev4 %in% main_menus &
                                last_distinct_before_last_ep_prev5 %in% main_menus &
                                last_distinct_before_last_ep_prev6 %in% main_menus &
                                last_distinct_before_last_ep_prev7 %in% main_menus &
                                
                                last_distinct_before_last_ep_prev8 %in% specific_menus 
                              ~ last_distinct_before_last_ep_prev8, 
                              
                              .default = "Undetermined")
         ) %>% 
  relocate(analysis) %>% 
  view()


count_table %>% 
  group_by(analysis, activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()

#undetermined: disconnectcallback guy is prelegal seniors

family_counts <- count_table %>%
  filter(analysis == "Legal Family Menu Telephony EP") %>% 
  group_by(activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()

olegal_counts <- count_table %>%
  filter(analysis == "Other Legal Menu Telephony EP") %>% 
  group_by(activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()

legal_counts <- count_table %>%
  filter(analysis == "Legal Menu Telephony EP") %>% 
  group_by(activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()

#verify sana
main_counts <- count_table %>%
  filter(analysis == "Main Number Telephony EP") %>% 
  group_by(activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()

#verify adam (using same code but replacing menu name to check)
count_table %>%
  filter(analysis == "Legal Housing Menu Telephony EP") %>% 
  group_by(activity_name) %>%
  summarize(total_calls = sum(n)) %>%
  arrange(desc(total_calls)) %>% 
  view()


#categorizing activity_name NAs

all_last_rows2 %>% 
  filter(is.na(activity_name)) %>%
  mutate(activity_name = case_when(termination_reason == "NO_ANSWER_FROM_CUSTOMER" ~ termination_reason,
                                   termination_reason == "System Error" ~ termination_reason, 
                                   !is.na(queue_name) ~ queue_name,
                                   termination_reason == "Customer Left" ~ termination_reason,
                                   .default = "No Activity")) %>% 
  group_by(ep_name, activity_name) %>%
  count(activity_name) %>% 
  view()
