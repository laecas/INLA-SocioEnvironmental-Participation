# =========================================================
# FINAL FOREST PLOT - TWO COLUMNS WITH EQUAL BLOCK SPACING
# =========================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

# ---------------------------------------------------------
# 1. Prepare data and rename labels shown in the plot
# ---------------------------------------------------------
summary_orPCMA2 <- summary_orPCMA %>%
  mutate(
    clean_label = rotulo,
    clean_label = str_replace(clean_label, "^APP:", "Riparian APP:"),
    clean_label = str_replace(clean_label, "^APP deficit:", "Riparian APP deficit:"),
    clean_label = str_replace(clean_label, "^Farm size:", "Property size:"),
    group = case_when(
      str_detect(clean_label, "^Riparian APP:") ~ "Riparian APP",
      str_detect(clean_label, "^Riparian APP deficit:") ~ "Riparian APP deficit",
      str_detect(clean_label, "^Protected area distance:") ~ "Protected area distance",
      str_detect(clean_label, "^Aspect:") ~ "Aspect",
      str_detect(clean_label, "^Elevation:") ~ "Elevation",
      str_detect(clean_label, "^Property size:") ~ "Property size",
      str_detect(clean_label, "^Native vegetation \\(2017\\):") ~ "Native vegetation (2017)",
      str_detect(clean_label, "^Pasture quality \\(2017\\):") ~ "Pasture quality (2017)",
      str_detect(clean_label, "^Slope:") ~ "Slope",
      str_detect(clean_label, "^Transportation cost \\(2017\\):") ~ "Transportation cost",
      str_detect(clean_label, "^Precipitation:") ~ "Precipitation",
      TRUE ~ NA_character_
    ),
    category = sub("^.*?:\\s*", "", clean_label)
  )

# ---------------------------------------------------------
# 2. Define exact group order
# ---------------------------------------------------------
left_groups <- c(
  "Riparian APP",
  "Riparian APP deficit",
  "Protected area distance",
  "Aspect",
  "Elevation",
  "Property size"
)

right_groups <- c(
  "Native vegetation (2017)",
  "Pasture quality (2017)",
  "Slope",
  "Transportation cost",
  "Precipitation"
)

# ---------------------------------------------------------
# 3. Define within-group order
# ---------------------------------------------------------
quartile_order <- c("Lowest", "Low", "High", "Highest")
aspect_order   <- c("North to East", "East to South", "South to West", "West to North")
slope_order    <- c("Flat", "Gentle", "Steep", "Very steep")
size_order     <- c("Very small", "Small", "Large", "Very large")

summary_orPCMA2 <- summary_orPCMA2 %>%
  mutate(
    category_order = case_when(
      group %in% c(
        "Riparian APP",
        "Riparian APP deficit",
        "Protected area distance",
        "Native vegetation (2017)",
        "Pasture quality (2017)",
        "Transportation cost",
        "Precipitation",
        "Elevation"
      ) ~ match(category, quartile_order),
      group == "Aspect" ~ match(category, aspect_order),
      group == "Slope" ~ match(category, slope_order),
      group == "Property size" ~ match(category, size_order),
      TRUE ~ NA_real_
    )
  )

# ---------------------------------------------------------
# 4. Function to create manual y positions with equal spacing
# ---------------------------------------------------------
build_panel_positions <- function(data, groups_order, add_blank_group = FALSE,
                                  items_per_group = 4, gap = 1.4) {
  
  out <- list()
  current_top <- 0
  
  for (g in groups_order) {
    df_g <- data %>%
      filter(group == g) %>%
      arrange(category_order)
    
    n <- nrow(df_g)
    
    # positions inside each group: equally spaced
    # first item at top, then descending
    y_vals <- current_top - seq(0, by = 1, length.out = n)
    
    df_g$y_pos <- y_vals
    out[[g]] <- df_g
    
    # move downward by a fixed full block size, regardless of actual group label length
    current_top <- current_top - ((items_per_group - 1) + gap)
  }
  
  result <- bind_rows(out)
  
  # optional blank block at the end to balance the shorter column
  if (add_blank_group) {
    current_top <- current_top - ((items_per_group - 1) + gap)
  }
  
  result
}

# Left column = 6 real groups
df_left <- summary_orPCMA2 %>%
  filter(group %in% left_groups)

df_left <- build_panel_positions(
  data = df_left,
  groups_order = left_groups,
  add_blank_group = FALSE,
  items_per_group = 4,
  gap = 1.4
)

# Right column = 5 real groups + 1 empty block for visual balance
df_right <- summary_orPCMA2 %>%
  filter(group %in% right_groups)

df_right <- build_panel_positions(
  data = df_right,
  groups_order = right_groups,
  add_blank_group = TRUE,
  items_per_group = 4,
  gap = 1.4
)

# ---------------------------------------------------------
# 5. Common x-axis limits
# ---------------------------------------------------------
x_min <- min(summary_orPCMA2$lower, na.rm = TRUE)
x_max <- max(summary_orPCMA2$upper, na.rm = TRUE)
x_limits <- c(min(0.8, x_min - 0.05), x_max + 0.15)

# Common y-axis limits so both columns occupy the same vertical space
y_min <- min(c(df_left$y_pos, df_right$y_pos), na.rm = TRUE) - 0.5
y_max <- max(c(df_left$y_pos, df_right$y_pos), na.rm = TRUE) + 0.5

# ---------------------------------------------------------
# 6. Plot function using manual y positions
# ---------------------------------------------------------
make_forest_panel <- function(data, show_y_title = FALSE) {
  
  ggplot(data, aes(x = OR, y = y_pos, color = significativo)) +
    geom_point(
      aes(shape = shape),
      size = 2.8,
      stroke = ifelse(data$shape == "hollow", 1.5, 1.0),
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper),
      height = 0.18,
      na.rm = TRUE
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_color_manual(
      values = c("TRUE" = "darkred", "FALSE" = "gray40"),
      name = "Posterior evidence"
    ) +
    scale_shape_manual(
      values = c("solid" = 16, "hollow" = 1),
      labels = c("Q2–Q4", "Q1 (reference)"),
      name = "Quartile"
    ) +
    scale_x_continuous(
      limits = x_limits,
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = data$y_pos,
      labels = data$clean_label,
      expand = c(0, 0)
    ) +
    labs(
      x = "Odds Ratio",
      y = if (show_y_title) "Variable (Category)" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.title.y = if (show_y_title) element_text(size = 12) else element_blank(),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

# ---------------------------------------------------------
# 7. Build panels
# ---------------------------------------------------------
p_left <- make_forest_panel(df_left, show_y_title = TRUE)
p_right <- make_forest_panel(df_right, show_y_title = FALSE)

# ---------------------------------------------------------
# 8. Combine panels
# ---------------------------------------------------------
p_final <- (p_left + p_right) +
  plot_layout(
    ncol = 2,
    widths = c(1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    title = "Odds Ratios with 95% Credible Intervals",
    subtitle = "Reference quartile (Q1) shown as hollow circles"
  ) &
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Show plot
p_final

# ---------------------------------------------------------
# 9. Save high resolution
# ---------------------------------------------------------
ggsave(
  filename = "forest_plot_PCMA_2columns_equal_spacing.png",
  plot = p_final,
  width = 16,
  height = 9,
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "forest_plot_PCMA_2columns_equal_spacing.pdf",
  plot = p_final,
  width = 16,
  height = 9,
  device = cairo_pdf,
  bg = "white"
)