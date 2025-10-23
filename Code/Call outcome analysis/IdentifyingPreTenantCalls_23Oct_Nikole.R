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
# Replace with the right path where each of the datasets that you imported are
all_calls <- read_csv("data/combined_All_Call.csv") |> janitor::clean_names()
car <- read_csv("data/car.csv") |> janitor::clean_names()

# Task 3:  how many callers actually self-terminate (abandon?) after listening to the pre-tenant menu.
# If most of them continue through to the next set of tenant options, 
# it seems like we should remove the pre-tenant menu because it’s not doing any good

# All sessions that reached PreTenantMenu
pre_tenant_calls <- car %>%
  filter(activity_name == "PreTenantMenu") %>%
  distinct(contact_session_id, .keep_all = TRUE)

# All sessions that reached TenantMenu
tenant_calls <- car %>%
  filter(activity_name == "TenantMenu") %>%
  distinct(contact_session_id, .keep_all = TRUE)

### Exploring possible termination teasons
unique(pre_tenant_calls$termination_reason)
unique(tenant_calls$termination_reason)

## unfortunately, the dataset doesn't count with that information, they are only NAs

### Why don't we keep the duplicates
duplicates <- car %>%
  filter(activity_name == "PreTenantMenu") %>%
  count(contact_session_id, sort = TRUE) %>%
  filter(n > 1)

print(duplicates)

call_reentries <- car %>%
  filter(`Activity Name` == "PreTenantMenu") %>%
  count(`Contact Session ID`, sort = TRUE) %>%
  summarise(
    total_sessions = n(),
    repeated_sessions = sum(n > 1),
    percent_repeated = round(100 * mean(n > 1), 1)
  )

### Thus: 
#### This shows us that the same session may have multiple PreTenantMenu rows — same ID, same activity name, different timestamps.
#### That's why the difference between thos repeteated sessions are from seconds to minutes

# Tag each PreTenant session by whether it later reached TenantMenu
pre_tenant_calls_categorized <- pre_tenant_calls %>%
  mutate(path_status = if_else(contact_session_id %in% tenant_calls$contact_session_id,
                               "continued_to_tenant",
                               "stopped_after_pretenant"))

# Quick summary
pre_tenant_summary <- pre_tenant_calls_categorized %>%
  count(path_status) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(pre_tenant_summary)

# plot
plot <- ggplot(pre_tenant_summary, aes(x = path_status, y = percent, fill = path_status)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c(
    "continued_to_tenant" = "#8FD694",
    "stopped_after_pretenant" = "#F28E8E")) +
  scale_x_discrete(
    labels = c(
      "continued_to_tenant" = "Continued to Tenant",
      "stopped_after_pretenant" = "Stopped after Pre-Tenant"
    )
  ) +
  labs(
    title = "Caller Flow After Pre-Tenant Menu",
    x = NULL,
    y = "Percentage of Callers",
    fill = "Path Status"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(face = "bold", color = "black")
  )

ggsave("my_plot.png", plot = plot, width = 7, height = 5, units = "in", dpi = 300)
