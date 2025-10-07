## Presentation 1 Task 1: "Compare the number of calls in the CAR data with 
## the relevant Inbound calls in the All calls dataset. 
## Visualize the number of calls vs calling hour in both cases"

# libraries
library(tidyverse)
library(lubridate)

# read in data
all_calls <- read_csv('data/combined_All_Call.csv')
car <- read_csv('data/combined_data.csv')

all_calls <-
all_calls |> janitor::clean_names() |>
  mutate(report_time = with_tz(report_time, tzone = 'America/Chicago'),
         month = month(report_time, label = TRUE, abbr = FALSE),
         year = year(report_time),
         month_year = paste0(month, ' ', year),
         ) 

car <-
car |> janitor::clean_names()

# filter out 0 second calls in all calls and grab inbound calls only
calls_by_month_all <-
all_calls |>
  filter(duration > 0, 
         direction == "TERMINATING",
         pstn_vendor_name == 'CallTower') |>
  distinct(correlation_id, .keep_all = TRUE) |>
  summarize(
    .by = month_year,
    n = n()
  )
  
# get calls by month/year with CAR
calls_by_month_car <-
car |>
  distinct(contact_session_id, .keep_all = TRUE) |>
  summarize(
    .by = month_year,
    n = n()
  )

# make table/plots
month_comp <-
inner_join(calls_by_month_car, calls_by_month_all, by = join_by(month_year)) |>
  mutate(difference = n.x - n.y) |>
  knitr::kable(
    caption = 'Total Inbound Calls: Month by Month Comparison',
    col.names = c('Month/Year', 'CAR Calls', 'All Calls', 'Difference')
  )

save(month_comp, file = 'graphics/month_comp.rda')

month_comp_data <-
inner_join(calls_by_month_car, calls_by_month_all, by = join_by(month_year)) |>
  mutate(month_year = factor(month_year, levels =
                               c('April 2024', 'May 2024', 'June 2024', 'July 2024',
                                 'August 2024', 'September 2024', 'October 2024', 'November 2024',
                                 'December 2024', 'January 2025', 'February 2025',
                                 'March 2025'))) |>
  rename(CAR = n.x, `All Calls` = n.y) |>
  pivot_longer(
    cols = c(CAR, `All Calls`),
    names_to = 'dataset',
    values_to = 'count'
  ) 

month_comp_graph <-
month_comp_data |>
  ggplot(aes(count, month_year, fill = dataset)) +
  geom_col(position = 'dodge') +
  labs(
    title = 'Inbound Call Counts: Month-by-Month Comparison',
    x = 'Number of Inbound Calls',
    y = 'Month',
    fill = 'Dataset'
  )

ggsave(month_comp_graph, 'graphics/month_comp_graph.png')
