# Second Presentation
# Nikole Montero 

# Loading libraries 
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

# Reading datasets (Requires accessing Github)
# To read the CAR dataset, please refer to my import code on this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/Importing_CAR_Nikole.R
# To read in the All Calls dataset, please refer to the import code in this link: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Data%20import/Importing_AllCallsData_Nikole.ipynb

all_calls <- read_csv("data/combined_All_Call.csv") |> janitor::clean_names()
car <- read_csv("data/car.csv") |> janitor::clean_names()

# Task 3:  how many callers actually self-terminate (abandon?) after listening to the pre-tenant menu.
# If most of them continue through to the next set of tenant options, 
# it seems like we should remove the pre-tenant menu because it’s not doing any good

## 1. Filtering CAR dataset to only call sessions that reached the PreTenant Menu
pre_tenant_calls <- car_data %>%
  filter(`Activity Name` == "PreTenantMenu") %>%
  distinct(`Contact Session ID`, .keep_all = TRUE)

### Why don't we keep the duplicates
duplicates <- car_data %>%
  filter(`Activity Name` == "PreTenantMenu") %>%
  count(`Contact Session ID`, sort = TRUE) %>%
  filter(n > 1)

print(duplicates)

call_reentries <- car_data %>%
  filter(`Activity Name` == "PreTenantMenu") %>%
  count(`Contact Session ID`, sort = TRUE) %>%
  summarise(
    total_sessions = n(),
    repeated_sessions = sum(n > 1),
    percent_repeated = round(100 * mean(n > 1), 1)
  )

### Conclusion: 
#### This shows us that the same session may have multiple PreTenantMenu rows — same ID, same activity name, different timestamps.
#### That's why the difference between thos repeteated sessions are from seconds to minutes

# 2. Joining with All Calls to check how those calls ended
pre_tenant_behavior <- all_calls %>%
  semi_join(pre_tenant_calls, by = c("correlation_id" = "Contact Session ID")) %>%
  mutate(
    call_status = case_when(
      releasing_party == "Remote" & call_outcome == "Success" ~ "self_terminated",
      releasing_party == "Remote" & call_outcome == "Refusal" ~ "abandoned",
      releasing_party == "Remote" & call_outcome == "Failure" ~ "dropped",
      releasing_party == "Local"  ~ "agent_ended",
      TRUE ~ "unknown"
    )
  )

# 3. Summarizing the results to percentage
pre_tenant_behavior_table <- pre_tenant_behavior %>%
  count(call_status) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  rename(amount_of_calls = n)

# Part 2: How many callers continued from PreTenantMenu → TenantMenu
## 1. Filtering CAR dataset to only call sessions that reached the TenantMenu
tenant_calls <- car_data %>%
  filter(`Activity Name` == "TenantMenu") %>%
  distinct(`Contact Session ID`, .keep_all = TRUE)

## 2. Joining with All Calls to check how those calls ended
continued_calls <- tenant_calls %>%
  semi_join(tenant_calls, by = "Contact Session ID")

## 3. Comparing totals
total_pretenant <- nrow(tenant_calls)
continued <- nrow(continued_calls)
continued_percent <- round(100 * continued / total_pretenant, 1)

data.frame(
  total_pretenant = total_pretenant,
  continued = continued,
  percent_continued = continued_percent
)

## Which calls continued from PreTenant Menu to TenantMenu and how those ended
pre_tenant_calls_categorized <- pre_tenant_calls %>%
  mutate(path_status = if_else(`Contact Session ID` %in% tenant_calls$`Contact Session ID`,
                               "continued_to_tenant", "stopped_after_pretenant"))

pre_tenant_unique <- pre_tenant_calls_categorized %>%
  distinct(`Contact Session ID`, .keep_all = TRUE)

pre_tenant_behavior <- all_calls %>%
  semi_join(pre_tenant_unique, by = c("correlation_id" = "Contact Session ID")) %>%
  left_join(pre_tenant_unique, by = c("correlation_id" = "Contact Session ID")) %>%
  mutate(
    call_status = case_when(
      releasing_party == "Remote" & call_outcome == "Success" ~ "self_terminated",
      releasing_party == "Remote" & call_outcome == "Refusal" ~ "abandoned",
      releasing_party == "Remote" & call_outcome == "Failure" ~ "dropped",
      releasing_party == "Local"  ~ "agent_ended",
      TRUE ~ "unknown"
    )
  )

# Calls Continuation vs Call Outcome
## Visualizing results
### summarizing data
summary_df <- pre_tenant_behavior %>%
  count(path_status, call_status) %>%
  group_by(path_status) %>%
  mutate(percent = round(100 * n / sum(n), 1))

### Plot
p <- ggplot(summary_df,
            aes(x = path_status,
                y = percent,
                fill = call_status,
                text = paste0(
                  "<b>Path:</b> ", path_status,
                  "<br><b>Status:</b> ", call_status,
                  "<br><b>Count:</b> ", n,
                  "<br><b>Percent:</b> ", percent, "%"
                ))) +
  geom_col(color = "white", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Caller Outcomes After Pre-Tenant Menu",
    subtitle = "Comparing the calls who continued to Tenant Menu vs. stopped after Pre-Tenant",
    x = NULL,
    y = "Percentage of Calls",
    fill = "Call Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "bottom",
    axis.text.x = element_text(face = "bold")
  )

# make it interactive
p_interactive <- ggplotly(p, tooltip = "text")

saveWidget(p_interactive, "pre_tenant_behavior_plot.html", selfcontained = TRUE)


