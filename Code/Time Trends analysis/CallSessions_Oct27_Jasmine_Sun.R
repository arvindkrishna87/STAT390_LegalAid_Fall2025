library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# using cleaned CAR data 

car_data = read_csv("CAR_cleaned.csv")

#----------------------------------------------------------
# STEP 1 & 2: ENDPOINT AND OPEN INTAKE QUEUE FLAGS
#----------------------------------------------------------
# - We compute these at the Contact Session ID level
# - Substring matching (case-insensitive)
# - ReachedEndpoint = 1 if any of:
#     - PreQueueIntakeMessage1 appears anywhere in Activity Name
#     - ClosedQueue appears anywhere in Activity Name
#     - OR Queue Name is non-NA (meaning caller reached some queue)
# - ReachedOpenIntakeQueue = 1 if PreQueueIntakeMessage1 appears anywhere
#----------------------------------------------------------

session_flags <- car_data %>%
  group_by(`Contact Session ID`) %>%
  summarise(
    ReachedEndpoint = as.integer(
      any(str_detect(`Activity Name`, regex("PreQueueIntakeMessage1", ignore_case = TRUE))) |
        any(str_detect(`Activity Name`, regex("ClosedQueue", ignore_case = TRUE))) |
        any(!is.na(`Queue Name`))
    ),
    ReachedOpenIntakeQueue = as.integer(
      any(str_detect(`Activity Name`, regex("PreQueueIntakeMessage1", ignore_case = TRUE)))
    ),
    .groups = "drop"
  )

#----------------------------------------------------------
# STEP 3: ORIGINATION FROM "LAC"
#----------------------------------------------------------
# - If EP Name == "Courtesy Callback Telephony EP" => OriginationLAC = 1
#   else 0
# - This determines if call ORIGINATED at LAC
#----------------------------------------------------------

lac_flag <- car_data %>%
  arrange(`Contact Session ID`, `Activity Start Timestamp`) %>%  # chronological ordering
  group_by(`Contact Session ID`) %>%
  summarise(
    OriginationLAC = as.integer(
      any(str_detect(`EP Name`, regex("Courtesy Callback Telephony EP", ignore_case = TRUE)))
    ),
    .groups = "drop"
  )

# Join OriginationLAC into session-level flags
session_flags <- session_flags %>%
  left_join(lac_flag, by = "Contact Session ID")

session_start <- car_data %>%
  group_by(`Contact Session ID`) %>%
  summarise(
    SessionStartTime = min(`Activity Start Timestamp`, na.rm = TRUE),
    .groups = "drop"
  )

#----------------------------------------------------------
# STEP 4: CALLER QUEUE MENU
#----------------------------------------------------------
# - Only assign a menu IF the session:
#       1) ReachedEndpoint == 1
#       2) OriginationLAC == 0  (excluded if LAC-originated)
#
# - Menu logic uses PRIORITY order:
#     1 > 2 > 3 > 4 > 5
#
# - Match style: PREFIX match, case-insensitive
#       - Must start with the menu string
#       - Extra text is allowed *after* (suffix ok)
#       - No extra text BEFORE
#
# - We select the MOST RECENT such menu (latest timestamp),
#   This is the menu that the queued caller came from
#   then break ties by highest menu priority
#----------------------------------------------------------

# Priority labels (no spaces), case-insensitive prefix match
priority1 <- c("HIVMenu","FamilyMenu","HousingMenu","BenefitsMenu",
               "EmploymentMenu","ImmigrationMenu","OtherLegalMenu")
priority2 <- c("LegalMenu")
priority3 <- c("SuburbanSeniorsMenu","SeniorsADAPTMenu")
priority4 <- c("ClosedMenu")
priority5 <- c("MainMenu")

# Helper to build a single prefix-regex from a vector (case-insensitive)
prefix_rx <- function(vec) {
  paste0("^(", paste(vec, collapse = "|"), ")")
}


eligible_sessions <- session_flags %>%
  filter(ReachedEndpoint == 1, is.na(OriginationLAC)) %>%
  select(`Contact Session ID`)

caller_issue_menu <- car_data %>%
  # Only sessions that reached endpoint and not LAC-originated
  semi_join(eligible_sessions, by = "Contact Session ID") %>%
  mutate(
    # Priority by CASE-INSENSITIVE PREFIX match
    menu_priority = case_when(
      str_detect(`Activity Name`, regex(prefix_rx(priority1), ignore_case = TRUE)) ~ 1L,
      str_detect(`Activity Name`, regex(prefix_rx(priority2), ignore_case = TRUE)) ~ 2L,
      str_detect(`Activity Name`, regex(prefix_rx(priority3), ignore_case = TRUE)) ~ 3L,
      str_detect(`Activity Name`, regex(prefix_rx(priority4), ignore_case = TRUE)) ~ 4L,
      str_detect(`Activity Name`, regex(prefix_rx(priority5), ignore_case = TRUE)) ~ 5L,
      # ---- FALLBACK (priority 6) ----
      # latest entry that CONTAINS "menu" and does NOT contain "queue" (anywhere),
      # case-insensitive, not restricted to prefix
      str_detect(`Activity Name`, regex("menu",  ignore_case = TRUE)) &
        !str_detect(`Activity Name`, regex("queue", ignore_case = TRUE)) ~ 6L,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Keep only rows that matched either a priority or fallback
  filter(!is.na(menu_priority)) %>%
  # Sort: latest timestamp first, then highest priority (1 is highest)
  arrange(`Contact Session ID`,
          desc(`Activity Start Timestamp`),
          menu_priority) %>%
  group_by(`Contact Session ID`) %>%
  summarise(
    PrimaryMenu = first(`Activity Name`),
    .groups = "drop"
  )

caller_issue_menu <- caller_issue_menu %>%
  mutate(
    CallerIssueMenu = case_when(
      str_detect(PrimaryMenu, regex("senior",  ignore_case = TRUE)) ~ "Senior Menu",
      str_detect(PrimaryMenu, regex("divorce|custody|family", ignore_case = TRUE)) ~ "Family Menu",
      str_detect(PrimaryMenu, regex("legal",   ignore_case = TRUE)) ~ "Legal Menu",
      TRUE ~ PrimaryMenu
    )
  )


first_queue <- car_data %>%
  arrange(`Contact Session ID`, `Activity Start Timestamp`) %>%
  group_by(`Contact Session ID`) %>%
  summarise(
    FirstQueueName = {
      x <- `Queue Name`[!is.na(`Queue Name`)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    .groups = "drop"
  )
#----------------------------------------------------------
# FINAL SESSION-LEVEL OUTPUT
#----------------------------------------------------------
car_sessions <- session_flags %>%
  left_join(caller_issue_menu, by = "Contact Session ID") %>%
  left_join(session_start,    by = "Contact Session ID") %>%
  left_join(first_queue,      by = "Contact Session ID") |>
  select(-CallerIssueMenu)



write.csv(car_sessions, "CAR_sessions.csv",row.names = FALSE)
