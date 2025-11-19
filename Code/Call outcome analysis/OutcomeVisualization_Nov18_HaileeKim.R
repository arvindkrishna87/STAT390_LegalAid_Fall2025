## Visualizing call outcomes and sub-categories for specific menus ----
# must run this file after running "OutcomeTagging_Nov17_CarolineCorr" in the same file location

# additional libraries
library(ggplot2)
library(patchwork)

# define the mapping only with the selected 10 menu sub-categories
menu_map <- c(
  "LACMain" = "Main",
  "PreLegalMenuSeniorsMenu" = "Pre-legal Seniors",
  "LegalMenu" = "Legal",
  "LegalFamilyMenu" = "Family",
  "LegalHousingMenu" = "Housing",
  "LegalBenefitsMenu" = "Benefits",
  "LegalEmploymentMenu" = "Employment",
  "LegalImmigrationMenu" = "Immigration",
  "OtherLegalMenu" = "Other legal",
  "LegalHIVMenu" = "HIV"
)

## outcome analysis (positive/neutral/negative) ----
# define the desired order of outcomes for consistent display
outcome_levels <- c("Positive", "Neutral", "Negative")

# prepare the summary table
outcomes_by_menu <- outcomes_types |>
  mutate(
    menu_group = recode(
      last_menu,
      !!!menu_map,
      # ensure unmatched become NA
      .default = NA_character_  
    ),
    # enforce outcome order
    outcome = factor(outcome, levels = outcome_levels, ordered = TRUE),
    # enforce menu order
    menu_group = factor(menu_group, levels = unname(menu_map), ordered = TRUE)
  ) |>
  # drop everything not in map
  filter(!is.na(menu_group)) |>   
  count(menu_group, outcome) |>
  group_by(menu_group) |>
  mutate(
    percent = round(n / sum(n) * 100, 2)
  ) |>
  ungroup() |>
  arrange(menu_group, outcome) 

# visualization 2.1 from the presentation 
ggplot(outcomes_by_menu, aes(x = 2, y = percent, fill = outcome)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 2.3,
            # check_overlap = TRUE
  ) +  
  coord_polar(theta = "y") +  
  xlim(0.5, 2.5) +            
  facet_wrap(~menu_group) +
  scale_fill_manual(
    values = c("Positive" = "#4CAF50",
               "Neutral"  = "#FFC107",
               "Negative" = "#F44336")
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 11, margin = margin(b = 10)),  
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  ) +
  labs(fill = "Outcome", title = "Outcome Percentage per Menu")

## sub-outcome analysis ----
sub_outcomes_by_menu <- outcomes_types |>
  mutate(
    menu_group = recode(
      last_menu,
      !!!menu_map,
      .default = NA_character_
    ),
    outcome = factor(outcome, levels = outcome_levels, ordered = TRUE),
    menu_group = factor(menu_group, levels = unname(menu_map), ordered = TRUE)
  ) |>
  filter(!is.na(menu_group)) |>
  count(menu_group, outcome, sub_type) |>
  group_by(menu_group, outcome) |>
  mutate(
    percent = round(n / sum(n) * 100, 2)
  ) |>
  ungroup() |>
  arrange(menu_group, outcome, sub_type)

# custom sub-type palettes (3 shades each)
pal_positive <- c("#2E7D32", "#66BB6A", "#A5D6A7")   
pal_neutral  <- c("#F9A825", "#FFCA28", "#FFE082")   
pal_negative <- c("#C62828", "#EF5350", "#FFCDD2")   

menus <- unique(sub_outcomes_by_menu$menu_group)

# create a loop for individual graph per menu
# visualization 2.2-2.11 from the presentation
for (m in menus) {
  
  df_menu <- sub_outcomes_by_menu |> 
    filter(menu_group == m)
  
  # function to build one donut per outcome
  build_donut <- function(outcome_name, palette) {
    df_out <- df_menu |> filter(outcome == outcome_name)
    
    ggplot(df_out, aes(x = 2, y = percent, fill = sub_type)) +
      geom_col(width = 1, color = "white") +
      scale_fill_manual(values = palette) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      geom_text(
        aes(
          label = paste0(sub_type, " ", sprintf("%.1f%%", percent))
        ),
        position = position_stack(vjust = 0.5),
        size = 3.6,
        fontface = "bold",
        check_overlap = TRUE
      ) +
      coord_polar(theta = "y", clip = "off") +
      theme_void() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      ggtitle(outcome_name)
  }
  # build the 3 donuts with distinct palettes
  p_pos <- build_donut("Positive", pal_positive)
  p_neu <- build_donut("Neutral",  pal_neutral)
  p_neg <- build_donut("Negative", pal_negative)
  # combine horizontally
  row_plot <- p_pos + p_neu + p_neg + plot_layout(ncol = 3)
  # add title
  final_plot <- (
    row_plot +
      plot_annotation(
        title = paste("Menu:", m),
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )
      )
  )
  # save down the graphs if needed
  # ggsave(
  #   filename = paste0("graphics/menu_", m, "_donuts.png"),
  #   plot = final_plot,      
  #   width = 12,    
  #   height = 4,
  #   dpi = 300
  # )
}
