## Tagging Call Outcomes ----

# load packages
library(tidyverse)
library(ggplot2)

## Before running the code below, please run the Outcome Tagging code found here: https://github.com/arvindkrishna87/STAT390_LegalAid_Fall2025/blob/main/Code/Call%20outcome%20analysis/OutcomeTagging_Nov17_CarolineCorr.R

### Task metric 1:
# The proportion of positive, negative, and neutral outcomes in the data, and their split into the above sub-categories
metric_1_plot <-
  outcomes_types |>
  filter(!is.na(sub_type), !is.na(outcome)) |>
  count(outcome, sub_type) |>
  group_by(outcome) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = outcome, y = prop, fill = sub_type)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.1))  # extra space above labels
  ) +
  
  labs(
    title = "Sub-Category Breakdown Within Each Outcome Type",
    x = NULL,
    y = "Percentage",
    fill = "Sub-Category"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right"
  )

# This plot will save in the current working directory you're working on
ggsave(
  filename = "metric_1_plot.png",
  plot = metric_1_plot,
  width = 10,
  height = 7,
  dpi = 300
)


