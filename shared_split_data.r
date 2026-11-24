library(tidyverse)
library(tidymodels)

# Load cleaned dataset
sleep_data_cleaned <- read_csv(
  "cleaned_sleep_data.csv",
  show_col_types = FALSE
)

# Convert blue light filter into a categorical variable
sleep_data_cleaned$blue_light_filter_on <- factor(
  sleep_data_cleaned$blue_light_filter_on,
  levels = c("Filter Off", "Filter On")
)

# Set random seed for reproducibility
set.seed(2026)

# Split data into 60% training, 20% validation,
# and 20% testing
data_split <- initial_validation_split(
  sleep_data_cleaned,
  prop = c(0.6, 0.2)
)

# Create shared datasets
train_data <- training(data_split)
validation_data <- validation(data_split)
test_data <- testing(data_split)

# Save shared datasets
saveRDS(
  list(
    train_data = train_data,
    validation_data = validation_data,
    test_data = test_data
  ),
  "shared_datasets.rds"
)

# in other models use this code to access
# Load shared datasets
#shared_data <- readRDS("shared_datasets.rds")

#train_data <- shared_data$train_data
#validation_data <- shared_data$validation_data
#test_data <- shared_data$test_data