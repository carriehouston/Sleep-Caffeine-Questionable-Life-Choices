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

min_max_plot <- data.frame(
  Variable = rep(min_max$Variable, 2),
  Statistic = c(
    rep("Minimum", 6),
    rep("Maximum", 6)
  ),
  Value = c(min_max$Minimum, min_max$Maximum)
)
ggplot(min_max_plot, aes(x = Variable, y = Value, fill = Statistic)) +
  geom_col(position = "dodge") + 
  geom_text(
    aes(label = round(Value, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Minimum and Maximum Values by Variable",
    x = "Variable",
    y = "Value",
    fill = "Statistic"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  "minimum_maximum_plot.png",
  width = 10,
  height = 7,
  dpi = 300
)