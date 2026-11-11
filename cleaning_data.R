library(tidyverse)
# Function to replace outliers with the median
replace_outliers <- function(x) {
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - (1.5 * IQR_value)
  upper_bound <- Q3 + (1.5 * IQR_value)
  
  median_value <- median(x, na.rm = TRUE)
  
  x <- replace(
    x,
    !is.na(x) & (x < lower_bound | x > upper_bound),
    median_value
  )
  
  return(x)
}
sleep_data_cleaned <- sleep_data %>%
  mutate(
    caffeine_intake_mg = replace_outliers(caffeine_intake_mg),
    physical_activity_min = replace_outliers(physical_activity_min),
    screen_time_before_bed_min = replace_outliers(screen_time_before_bed_min),
    ambient_noise_db = replace_outliers(ambient_noise_db),
    sleep_efficiency_pct = replace_outliers(sleep_efficiency_pct),
    next_day_fatigue_score = replace_outliers(next_day_fatigue_score)
  )
check_cleaning <- function(original, cleaned) {
  
  Q1 <- quantile(original, 0.25, na.rm = TRUE)
  Q3 <- quantile(original, 0.75, na.rm = TRUE)
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - (1.5 * IQR_value)
  upper_bound <- Q3 + (1.5 * IQR_value)
  
  return(sum(
    cleaned < lower_bound | cleaned > upper_bound,
    na.rm = TRUE
  ))
}
# Caffeine
check_cleaning(sleep_data$caffeine_intake_mg,
               sleep_data_cleaned$caffeine_intake_mg)

# Physical activity
check_cleaning(sleep_data$physical_activity_min,
               sleep_data_cleaned$physical_activity_min)

# Screen time
check_cleaning(sleep_data$screen_time_before_bed_min,
               sleep_data_cleaned$screen_time_before_bed_min)

# Ambient noise
check_cleaning(sleep_data$ambient_noise_db,
               sleep_data_cleaned$ambient_noise_db)

# Sleep efficiency
check_cleaning(sleep_data$sleep_efficiency_pct,
               sleep_data_cleaned$sleep_efficiency_pct)

# Fatigue score
check_cleaning(sleep_data$next_day_fatigue_score,
               sleep_data_cleaned$next_day_fatigue_score)