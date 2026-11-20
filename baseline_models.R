
library(tidyverse)
library(tidymodels)

# Load cleaned dataset
sleep_data_cleaned <- read_csv(
  "cleaned_sleep_data.csv",
  show_col_types = FALSE
)

# Set random seed for reproducibility
set.seed(123)

# Split data into 60% training and 40% remaining
data_split <- initial_split(
  sleep_data_cleaned,
  prop = 0.60
)

train_data <- training(data_split)
remaining_data <- testing(data_split)

# Split remaining data into validation and testing
remaining_split <- initial_split(
  remaining_data,
  prop = 0.50
)

validation_data <- training(remaining_split)
test_data <- testing(remaining_split)

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
