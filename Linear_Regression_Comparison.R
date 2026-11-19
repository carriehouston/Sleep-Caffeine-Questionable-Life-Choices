
library(tidyverse)
sleep_data <- read.csv("sleep_debt_screen_time.csv")

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


# Convert blue light filter variable into categories
sleep_data_cleaned$blue_light_filter_on <- factor(
  sleep_data_cleaned$blue_light_filter_on,
  levels = c(FALSE, TRUE),
  labels = c("Filter Off", "Filter On")
)

caffiene <- sleep_data_cleaned$caffeine_intake_mg
ambient_noise <- sleep_data_cleaned$ambient_noise_db
blue_light <- sleep_data_cleaned$blue_light_filter_on
screen_time <- sleep_data_cleaned$screen_time_before_bed_min
physical_activity <- sleep_data_cleaned$physical_activity_min

next_day_fatigue <- sleep_data_cleaned$next_day_fatigue_score
sleep_efficiency <- sleep_data_cleaned$sleep_efficiency_pct

new_df <- data.frame(
  BlueLight = blue_light,
  ScreenTime = screen_time,
  Ambient_Noise = ambient_noise,
  Physical_Activity = physical_activity,
  Caffiene = caffiene,
  Next_Day_Fatigue = next_day_fatigue,
  Sleep_Efficiency = sleep_efficiency
)


caffieneEfficiencyModel <- lm(
  Sleep_Efficiency ~ Caffiene,
  data = new_df
)

caffieneFatigueModel <- lm(
  Next_Day_Fatigue ~ Caffiene,
  data = new_df
)

summary(caffieneEfficiencyModel)
summary(caffieneFatigueModel)

ambientEfficiencyModel <- lm(
  Sleep_Efficiency ~ Ambient_Noise,
  data = new_df
)

ambientFatigueModel <- lm(
  Next_Day_Fatigue ~ Ambient_Noise,
  data = new_df
)

summary(ambientEfficiencyModel)
summary(ambientFatigueModel)


physicalEfficiencyModel <- lm(
  Sleep_Efficiency ~ Physical_Activity,
  data = new_df
)

physicalFatigueModel <- lm(
  Next_Day_Fatigue ~ Physical_Activity,
  data = new_df
)

summary(physicalEfficiencyModel)
summary(physicalFatigueModel)


screenEfficiencyModel <- lm(
  Sleep_Efficiency ~ ScreenTime,
  data = new_df
)

screenFatigueModel <- lm(
  Next_Day_Fatigue ~ ScreenTime,
  data = new_df
)

summary(screenEfficiencyModel)
summary(screenFatigueModel)


blueLightEfficiencyModel <- lm(
  Sleep_Efficiency ~ BlueLight,
  data = new_df
)

blueLightFatigueModel <- lm(
  Next_Day_Fatigue ~ BlueLight,
  data = new_df
)

summary(blueLightEfficiencyModel)
summary(blueLightFatigueModel)


efficiency_comparison <- data.frame(
  Variable = c(
    "Caffeine",
    "Ambient Noise",
    "Physical Activity",
    "Screen Time",
    "Blue Light Filter"
  ),
  
  R_Squared = c(
    summary(caffieneEfficiencyModel)$r.squared,
    summary(ambientEfficiencyModel)$r.squared,
    summary(physicalEfficiencyModel)$r.squared,
    summary(screenEfficiencyModel)$r.squared,
    summary(blueLightEfficiencyModel)$r.squared
  )
)

efficiency_comparison <- efficiency_comparison %>%
  arrange(desc(R_Squared))

print(efficiency_comparison)


fatigue_comparison <- data.frame(
  Variable = c(
    "Caffeine",
    "Ambient Noise",
    "Physical Activity",
    "Screen Time",
    "Blue Light Filter"
  ),
  
  R_Squared = c(
    summary(caffieneFatigueModel)$r.squared,
    summary(ambientFatigueModel)$r.squared,
    summary(physicalFatigueModel)$r.squared,
    summary(screenFatigueModel)$r.squared,
    summary(blueLightFatigueModel)$r.squared
  )
)

fatigue_comparison <- fatigue_comparison %>%
  arrange(desc(R_Squared))

print(fatigue_comparison)

# Blue light filter and sleep efficiency
ggplot(new_df, aes(x = BlueLight, y = Sleep_Efficiency)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Blue Light Filter vs. Sleep Efficiency",
    x = "Blue Light Filter",
    y = "Sleep Efficiency"
  ) +
  theme_minimal()

# Blue light filter and next day fatigue
ggplot(new_df, aes(x = BlueLight, y = Next_Day_Fatigue)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Blue Light Filter vs. Next-Day Fatigue",
    x = "Blue Light Filter",
    y = "Next-Day Fatigue Score"
  ) +
  theme_minimal()


# Sleep Efficiency
ggplot(efficiency_comparison,
       aes(x = reorder(Variable, R_Squared),
           y = R_Squared)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Explanatory Power of Sleep Efficiency Predictors",
    x = "Variable",
    y = "R-Squared"
  ) +
  theme_minimal()

# Next_day_fatigue
ggplot(fatigue_comparison,
       aes(x = reorder(Variable, R_Squared),
           y = R_Squared)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Explanatory Power of Next-Day Fatigue Predictors",
    x = "Variable",
    y = "R-Squared"
  ) +
  theme_minimal()

# R-Squared Comparison for next_day_fatigue and sleep_efficiency

efficiency_comparison <- efficiency_comparison %>%
  mutate(
    R_Squared = round(R_Squared, 5),
    Percent_Explained = round(R_Squared * 100, 2)
  ) %>%
  arrange(desc(R_Squared))

print(efficiency_comparison, row.names = FALSE)

fatigue_comparison <- fatigue_comparison %>%
  mutate(
    R_Squared = round(R_Squared, 5),
    Percent_Explained = round(R_Squared * 100, 2)
  ) %>%
  arrange(desc(R_Squared))

print(fatigue_comparison, row.names = FALSE)

