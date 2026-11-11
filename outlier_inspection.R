# Load data set
sleep_data <- read_csv("sleep_debt_screen_time.csv")

# Outliers for caffeine intake
Q1 <- quantile(sleep_data$caffeine_intake_mg, 0.25)

Q3 <- quantile(sleep_data$caffeine_intake_mg, 0.75)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - (1.5 * IQR_value)

upper_bound <- Q3 + (1.5 * IQR_value)

sum(sleep_data$caffeine_intake_mg < lower_bound |
      sleep_data$caffeine_intake_mg > upper_bound)
# Display caffeine intake outliers
caffeine_outliers <- sleep_data %>%
  filter(
    caffeine_intake_mg < lower_bound |
      caffeine_intake_mg > upper_bound
  )
caffeine_outliers
# Physical activity
Q1 <- quantile(sleep_data$physical_activity_min, 0.25)
Q3 <- quantile(sleep_data$physical_activity_min, 0.75)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

sum(sleep_data$physical_activity_min < lower_bound |
      sleep_data$physical_activity_min > upper_bound)

# Screen Time Before Bed Minutes
Q1 <- quantile(sleep_data$screen_time_before_bed_min, 0.25)
Q3 <- quantile(sleep_data$screen_time_before_bed_min, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

sum(sleep_data$screen_time_before_bed_min < lower_bound |
      sleep_data$screen_time_before_bed_min > upper_bound)

# Enviroment Noise
Q1 <- quantile(sleep_data$ambient_noise_db, 0.25)
Q3 <- quantile(sleep_data$ambient_noise_db, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

sum(sleep_data$ambient_noise_db < lower_bound |
      sleep_data$ambient_noise_db > upper_bound)
#Sleep efficiency
Q1 <- quantile(sleep_data$sleep_efficiency_pct, 0.25)
Q3 <- quantile(sleep_data$sleep_efficiency_pct, 0.75)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

sum(sleep_data$sleep_efficiency_pct < lower_bound |
      sleep_data$sleep_efficiency_pct > upper_bound)

#Fatigue score
Q1 <- quantile(sleep_data$next_day_fatigue_score, 0.25)
Q3 <- quantile(sleep_data$next_day_fatigue_score, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

sum(sleep_data$next_day_fatigue_score < lower_bound |
      sleep_data$next_day_fatigue_score > upper_bound)

#dataframe
