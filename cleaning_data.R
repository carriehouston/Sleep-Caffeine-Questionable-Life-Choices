library(tidyverse)
library(ggplot2)
library(tidymodels)

sleep_data <- read.csv("sleep_debt_screen_time.csv")

# Preserve original observations
sleep_data_cleaned <- sleep_data

# Identify potential outliers without modifying values
count_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
}

# Examine potential outliers
variables <- c(
  "caffeine_intake_mg",
  "physical_activity_min",
  "screen_time_before_bed_min",
  "ambient_noise_db",
  "sleep_efficiency_pct",
  "next_day_fatigue_score"
)

sapply(sleep_data_cleaned[variables], count_outliers)

# Convert blue light filter to categorical variable
sleep_data_cleaned$blue_light_filter_on <- factor(
  sleep_data_cleaned$blue_light_filter_on,
  levels = c(FALSE, TRUE),
  labels = c("Filter Off", "Filter On")
)
#verifying that screen time and caffeine intake are skewed now
ggplot(sleep_data_cleaned,
       aes(x = caffeine_intake_mg)) +
  geom_histogram(bins = 40) +
  labs(title = "Caffeine Intake Distribution")
ggsave("caffeine_distribution.png", width = 8, height = 6, dpi = 300)
ggplot(sleep_data_cleaned,
       aes(x = screen_time_before_bed_min)) +
  geom_histogram(bins = 40) +
  labs(title = "Screen Time Distribution")
ggsave("screen_time.png", width = 8, height = 6, dpi = 300)

sleep_data_cleaned <- sleep_data_cleaned |>
  select(
    screen_time_before_bed_min,
    caffeine_intake_mg,
    physical_activity_min,
    ambient_noise_db,
    blue_light_filter_on,
    sleep_efficiency_pct,
    next_day_fatigue_score
  ) |>
  na.omit()
write_csv(sleep_data_cleaned, "cleaned_sleep_data.csv")
