## ALL CALLS

library(tidyverse)
library(writexl)
library(janitor)
library(readxl)

#MY QMD FILE HAS THE CODE AND CLEANER REPORT OF RESULTS. 
#THIS IS JUST MY ROUGH WORK  

allcalls825 <- read_excel("/Users/meeratrivedi/Downloads/STAT 390
/August 2025.xlsx")

legalmenusummary <- read_excel("/Users/meeratrivedi/Downloads/STAT 390
/August 2025 (1).xlsx", skip = 1)


#Cleaned
lms <- legalmenusummary %>% 
  fill(everything(), .direction = "down") %>% 
  clean_names() %>% 
  mutate(total_calls = as.double(total_calls), 
         live_queued_calls = as.double(live_queued_calls), 
         callback_queued_calls = as.double(callback_queued_calls),
         closed_queue_calls = as.double(closed_queue_calls),
         abandoned_calls = as.double(abandoned_calls)) %>% 
  filter(legal_menu_option != "Summary")
  

#write_xlsx(lms, "Downloads/STAT 390/legal_menu_summary.xlsx")
#read_excel("Downloads/STAT 390/legal_menu_summary.xlsx")

#Total Calls per Legal Menu Option
lms %>% 
  group_by(legal_menu_option) %>% 
  summarize(sum_calls = sum(total_calls))

#Total Calls per Menu Selection
lms %>% 
  group_by(menu_selection) %>% 
  summarize(sum_calls = sum(total_calls))    

#Redundancies:
#adapt and subsenior adapt, maybe subsenior adapt can be within adapt
#family and subsenior family
#housing and subsenior homeowner
#consumer and subsenior consumer
#employment and subsenior employment

lms %>% 
  filter(menu_selection != "N/A") %>% 
  group_by(legal_menu_option, menu_selection) %>% 
  summarize(sum_calls = sum(total_calls)) %>% 
  arrange(legal_menu_option, desc(sum_calls))
#Redundancies:
#housing other, immigration other, all other issues
#housing to clinics, simple divorce to clinics, other legal issues - clinic
#should transfer to clinic be combined?

lms %>% 
  filter(queue_selection != "N/A") %>% 
  group_by(legal_menu_option, menu_selection, queue_selection) %>% 
  summarize(sum_calls = sum(total_calls)) %>% 
  arrange(legal_menu_option, menu_selection, queue_selection)


lms %>% 
  filter(queue_selection != "N/A") %>% 
  group_by(menu_selection) %>% 
  count(queue_selection) %>% 
  arrange(menu_selection, desc(n))


lms %>% 
  filter(group_suboption != "N/A") %>% 
  group_by(queue_selection) %>% 
  count(group_suboption) %>% 
  arrange(queue_selection, desc(n))

#Redundancies:
#consumer consumer
#education education
#adapt adapt

lms %>% 
  filter(first_queue_name != "N/A") %>% 
  group_by(group_suboption) %>% 
  summarize(sum_calls = sum(total_calls)) %>%
  arrange(group_suboption)
#Redundancies:
#consumer 
#adapt

lms %>% 
  filter(first_queue_name != "N/A") %>% 
  group_by(queue_selection, group_suboption) %>% 
  summarize(sum_calls = sum(total_calls)) %>%
  arrange(queue_selection, group_suboption)

#is each subsequent column a different stage of the call? 
#if so why do people have to go through adapt at each stage

lms %>% 
  filter(final_queue_name != "N/A") %>% 
  group_by(queue_selection, first_queue_name, final_queue_name) %>% 
  summarize(sum_calls = sum(total_calls)) %>% 
  arrange(queue_selection, first_queue_name, final_queue_name)

lms %>% 
  filter(legal_menu_option == "Pre-Legal Seniors", 
         queue_selection != "ADAPT", 
         queue_selection != "N/A", 
         first_queue_name != "N/A") %>% 
  select(queue_selection, group_suboption, first_queue_name, final_queue_name, total_calls)
#Redundancies:
#adapt subseniors, subsenior adapt, adapt subseniors
#benefits benefits benefits
