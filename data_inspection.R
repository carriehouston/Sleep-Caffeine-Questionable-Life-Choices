library(tidyverse)

# Load dataset
sleep_data <- read_csv("sleep_debt_screen_time.csv")

# Examine dataset
glimpse(sleep_data)

# View first six observations
head(sleep_data)

#last six observations
tail(sleep_data)

# Check dimensions
dim(sleep_data)

# Count missing values in each column
colSums(is.na(sleep_data))

# Count total missing values
sum(is.na(sleep_data))

# Count duplicate rows
sum(duplicated(sleep_data))

# Check duplicate user IDs
sum(duplicated(sleep_data$user_id))

#making r recognize these variables as categorical
sleep_data <- sleep_data %>%
  mutate(
    gender = as.factor(gender),
    chronotype = as.factor(chronotype),
    occupation = as.factor(occupation),
    primary_app_category = as.factor(primary_app_category),
    blue_light_filter_on = as.factor(blue_light_filter_on),
    sleep_debt_category = as.factor(sleep_debt_category)
  )


#summary statistics
# Summary statistics
summary_stats <- sleep_data %>%
  summarise(
    across(
      c(
        caffeine_intake_mg,
        physical_activity_min,
        screen_time_before_bed_min,
        ambient_noise_db,
        sleep_efficiency_pct,
        next_day_fatigue_score
      ),
      list(
        min = min,
        max = max,
        mean = mean,
        median = median,
        sd = sd,
        IQR = IQR
      )
    )
  )

summary_stats
print(summary_stats)
min_max_table <- data.frame(
  Variable = c(
    "Ambient Noise (dB)",
    "Caffeine Intake (mg)",
    "Next-Day Fatigue Score",
    "Physical Activity (min)",
    "Screen Time Before Bed (min)",
    "Sleep Efficiency (%)"
  ),
  
  Minimum = c(
    min(sleep_data_cleaned$ambient_noise_db),
    min(sleep_data_cleaned$caffeine_intake_mg),
    min(sleep_data_cleaned$next_day_fatigue_score),
    min(sleep_data_cleaned$physical_activity_min),
    min(sleep_data_cleaned$screen_time_before_bed_min),
    min(sleep_data_cleaned$sleep_efficiency_pct)
  ),
  
  Maximum = c(
    max(sleep_data_cleaned$ambient_noise_db),
    max(sleep_data_cleaned$caffeine_intake_mg),
    max(sleep_data_cleaned$next_day_fatigue_score),
    max(sleep_data_cleaned$physical_activity_min),
    max(sleep_data_cleaned$screen_time_before_bed_min),
    max(sleep_data_cleaned$sleep_efficiency_pct)
  )
)

print(min_max_table)
view(min_max_table)
write.csv(min_max_table, "min_max_table.csv", row.names = FALSE)