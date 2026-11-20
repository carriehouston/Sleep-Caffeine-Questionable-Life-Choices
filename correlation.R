library(tidyverse)
library(ggplot2)

# Load cleaned dataset
sleep_data_cleaned <- read_csv(
  "cleaned_sleep_data.csv",
  show_col_types = FALSE
)

# Independent variables
variables <- c(
  "ambient_noise_db",
  "caffeine_intake_mg",
  "physical_activity_min",
  "screen_time_before_bed_min"
)

# Calculate correlations for both outcomes
correlation_data <- data.frame(
  variable = rep(
    c(
      "Ambient Noise",
      "Caffeine Intake",
      "Physical Activity",
      "Screen Time"
    ),
    each = 2
  ),
  outcome = rep(
    c("Next-Day Fatigue", "Sleep Efficiency"),
    times = 4
  ),
  correlation = c(
    cor(sleep_data_cleaned$ambient_noise_db,
        sleep_data_cleaned$next_day_fatigue_score,
        use = "complete.obs"),
    cor(sleep_data_cleaned$ambient_noise_db,
        sleep_data_cleaned$sleep_efficiency_pct,
        use = "complete.obs"),
    
    cor(sleep_data_cleaned$caffeine_intake_mg,
        sleep_data_cleaned$next_day_fatigue_score,
        use = "complete.obs"),
    cor(sleep_data_cleaned$caffeine_intake_mg,
        sleep_data_cleaned$sleep_efficiency_pct,
        use = "complete.obs"),
    
    cor(sleep_data_cleaned$physical_activity_min,
        sleep_data_cleaned$next_day_fatigue_score,
        use = "complete.obs"),
    cor(sleep_data_cleaned$physical_activity_min,
        sleep_data_cleaned$sleep_efficiency_pct,
        use = "complete.obs"),
    
    cor(sleep_data_cleaned$screen_time_before_bed_min,
        sleep_data_cleaned$next_day_fatigue_score,
        use = "complete.obs"),
    cor(sleep_data_cleaned$screen_time_before_bed_min,
        sleep_data_cleaned$sleep_efficiency_pct,
        use = "complete.obs")
  )
)

# Set display order
correlation_data$variable <- factor(
  correlation_data$variable,
  levels = rev(c(
    "Ambient Noise",
    "Caffeine Intake",
    "Physical Activity",
    "Screen Time"
  ))
)

# Create heatmap
heatmap_graph <- ggplot(
  correlation_data,
  aes(x = outcome, y = variable, fill = correlation)
) +
  geom_tile(
    color = "black",
    linewidth = 0.6,
    width = 0.95,
    height = 0.95
  ) +
  geom_text(
    aes(label = round(correlation, 3)),
    size = 4.5,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "pink",
    mid = "white",
    high = "salmon",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  labs(
    title = "Correlations with Sleep Outcomes",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    plot.title.position="plot",
    panel.grid = element_blank(),
    axis.text = element_text(size = 11)
  )

ggsave(
  "sleep_correlation_heatmap.png",
  plot = heatmap_graph,
  width = 8,
  height = 4,
  dpi = 300
)
# Display
print(heatmap_graph)

# Save for report
ggsave(
  "sleep_correlation_heatmap.png",
  plot = heatmap_graph,
  width = 8,
  height = 4,
  dpi = 300
)
