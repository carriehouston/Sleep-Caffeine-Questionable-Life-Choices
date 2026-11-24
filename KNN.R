library(tidyverse)
library(ggplot2)
library(tidymodels)
library(kknn)
# Load shared datasets
shared_data <- readRDS("shared_datasets.rds")

train_data <- shared_data$train_data
validation_data <- shared_data$validation_data
test_data <- shared_data$test_data

# Use existing KNN variable names
train <- train_data
val <- validation_data
test <- test_data
# create recipe
rec <- recipe(
  sleep_efficiency_pct ~
    screen_time_before_bed_min +
    caffeine_intake_mg +
    physical_activity_min +
    ambient_noise_db +
    blue_light_filter_on,
  data = train
) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
# choosing best k
val_rmse <- function(k) {
  
  model <- nearest_neighbor(
    mode = "regression",
    neighbors = k,
    weight_func = "rectangular"
  ) |>
    set_engine("kknn")
  
  fitted <- workflow() |>
    add_recipe(rec) |>
    add_model(model) |>
    fit(data = train)
  
  preds <- augment(fitted, new_data = val)
  
  rmse(
    preds,
    truth = sleep_efficiency_pct,
    estimate = .pred
  )$.estimate
}

ks <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)

errors <- sapply(ks, val_rmse)

best_k <- ks[which.min(errors)]

best_k
k_results <- data.frame(
  K = ks,
  RMSE = errors
)

ggplot(k_results, aes(x = K, y = RMSE)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +
  geom_vline(
    xintercept = best_k,
    linetype = "dashed",
    color = "pink"
  ) +
  labs(
    title = "Sleep Efficiency: K vs Validation RMSE",
    x = "Number of Neighbors (K)",
    y = "Validation RMSE"
  ) +
  theme_minimal()

ggsave(
  "sleep_efficiency_k_selection.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Fit final model and test
final_model <- nearest_neighbor(
  mode = "regression",
  neighbors = best_k,
  weight_func = "rectangular"
) |>
  set_engine("kknn")

final_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(final_model) |>
  fit(data = train)

test_preds <- augment(
  final_fit,
  new_data = test
)

rmse(
  test_preds,
  truth = sleep_efficiency_pct,
  estimate = .pred
)

ggplot(test_preds,
       aes(x = sleep_efficiency_pct, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "pink"
  ) +
  labs(
    title = "KNN: Actual vs Predicted Sleep Efficiency",
    x = "Actual Sleep Efficiency (%)",
    y = "Predicted Sleep Efficiency (%)"
  ) +
  theme_minimal()

ggsave(
  "sleep_efficiency_predictions.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Baseline prediction for Sleep efficiency
sleep_efficiency_pct_baseline_prediction <- mean(
  train$sleep_efficiency_pct
)

# Baseline RMSE on test data for sleep efficiency
sleep_efficiency_pct_baseline_rmse <- sqrt(mean(
  (test$sleep_efficiency_pct - sleep_efficiency_pct_baseline_prediction)^2
))

sleep_efficiency_pct_baseline_rmse


# create recipe
rec <- recipe(
  next_day_fatigue_score ~
    screen_time_before_bed_min +
    caffeine_intake_mg +
    physical_activity_min +
    ambient_noise_db +
    blue_light_filter_on,
  data = train
) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
# choosing best k
val_rmse <- function(k) {
  
  model <- nearest_neighbor(
    mode = "regression",
    neighbors = k,
    weight_func = "rectangular"
  ) |>
    set_engine("kknn")
  
  fitted <- workflow() |>
    add_recipe(rec) |>
    add_model(model) |>
    fit(data = train)
  
  preds <- augment(fitted, new_data = val)
  
  rmse(
    preds,
    truth = next_day_fatigue_score,
    estimate = .pred
  )$.estimate
}

ks <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)

errors <- sapply(ks, val_rmse)

best_k <- ks[which.min(errors)]

best_k
k_results <- data.frame(
  K = ks,
  RMSE = errors
)
ggplot(k_results, aes(x = K, y = RMSE)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +
  geom_vline(
    xintercept = best_k,
    linetype = "dashed",
    color = "pink"
  ) +
  labs(
    title = "Next Day Fatigue score: K vs Validation RMSE",
    x = "Number of Neighbors (K)",
    y = "Validation RMSE"
  ) +
  theme_minimal()

ggsave(
  "next_day_fatigue_score_k_selection.png",
  width = 8,
  height = 6,
  dpi = 300
)
# Fit final model and test
final_model <- nearest_neighbor(
  mode = "regression",
  neighbors = best_k,
  weight_func = "rectangular"
) |>
  set_engine("kknn")

final_fit <- workflow() |>
  add_recipe(rec) |>
  add_model(final_model) |>
  fit(data = train)

test_preds <- augment(
  final_fit,
  new_data = test
)

rmse(
  test_preds,
  truth = next_day_fatigue_score,
  estimate = .pred
)
ggplot(test_preds,
       aes(x = next_day_fatigue_score, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "purple"
  ) +
  labs(
    title = "KNN: Actual vs Predicted Next Day Fatigue Score",
    x = "Actual Next Day Fatigue Score",
    y = "Predicted Next Day Fatigue Score"
  ) +
  theme_minimal()

ggsave(
  "next_day_fatigue_predictions.png",
  width = 8,
  height = 6,
  dpi = 300
)
# Next-day fatigue baseline
fatigue_baseline_prediction <- mean(
  train$next_day_fatigue_score
)

fatigue_baseline_rmse <- sqrt(mean(
  (test$next_day_fatigue_score -
     fatigue_baseline_prediction)^2
))
fatigue_baseline_rmse
