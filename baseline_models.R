library(tidyverse)
library(tidymodels)
library(ggplot2)

# Load shared datasets
shared_data <- readRDS("shared_datasets.rds")

train_data <- shared_data$train_data
validation_data <- shared_data$validation_data
test_data <- shared_data$test_data

# Sleep efficiency mean baseline
sleep_mean <- mean(train_data$sleep_efficiency_pct)

# Next-day fatigue mean baseline
fatigue_mean <- mean(train_data$next_day_fatigue_score)

# Display baseline predictions
sleep_mean
fatigue_mean

# Create baseline predictions for testing data
test_data$sleep_baseline <- sleep_mean
test_data$fatigue_baseline <- fatigue_mean

# Calculate sleep efficiency baseline RMSE
sleep_rmse <- rmse_vec(
  truth = test_data$sleep_efficiency_pct,
  estimate = test_data$sleep_baseline
)

# Calculate next-day fatigue baseline RMSE
fatigue_rmse <- rmse_vec(
  truth = test_data$next_day_fatigue_score,
  estimate = test_data$fatigue_baseline
)

# Display baseline results
baseline_results <- data.frame(
  Outcome = c("Sleep Efficiency", "Next-Day Fatigue"),
  Mean_Prediction = c(sleep_mean, fatigue_mean),
  RMSE = c(sleep_rmse, fatigue_rmse)
)

print(baseline_results)
# Sleep efficiency baseline visualization
ggplot(test_data,
       aes(x = sleep_efficiency_pct, y = sleep_baseline)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "pink"
  ) +
  labs(
    title = "Mean Baseline: Actual vs Predicted Sleep Efficiency",
    x = "Actual Sleep Efficiency (%)",
    y = "Predicted Sleep Efficiency (%)"
  ) +
  theme_minimal()

ggsave(
  "sleep_efficiency_baseline_predictions.png",
  width = 8,
  height = 6,
  dpi = 300
)
# Next-day fatigue baseline visualization
ggplot(test_data,
       aes(x = next_day_fatigue_score, y = fatigue_baseline)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "pink"
  ) +
  labs(
    title = "Mean Baseline: Actual vs Predicted Next-Day Fatigue",
    x = "Actual Next-Day Fatigue Score",
    y = "Predicted Next-Day Fatigue Score"
  ) +
  theme_minimal()

ggsave(
  "next_day_fatigue_baseline_predictions.png",
  width = 8,
  height = 6,
  dpi = 300
)