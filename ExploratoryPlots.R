library(tidyverse)

sleep_data <- read.csv("sleep_debt_screen_time.csv")

# Select variables to visualize
variables <- c(
  "caffeine_intake_mg",
  "physical_activity_min",
  "ambient_noise_db",
  "screen_time_before_bed_min",
  "sleep_efficiency_pct",
  "next_day_fatigue_score"
)

# Create a separate histogram for each variable
for (var in variables) {
  p <- ggplot(sleep_data, aes(x = .data[[var]])) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "white"
    ) +
    labs(
      title = paste("Distribution of", var),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal()
  # Display plot
  print(p)
  # Save plot as PNG
  ggsave(
    filename = paste0(var, "_histogram.png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}