#Exploring dataset
library(tidyverse)
library(ggplot2)
sleep_data <- read.csv("sleep_debt_screen_time.csv")
#Age vs Sleep efficiency
ggplot(sleep_data, aes(x = age, y = sleep_efficiency_pct)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Sleep Efficiency Across Ages",
    x = "Age",
    y = "Sleep Efficiency (%)"
  ) +
  theme_minimal()
ggsave("age_sleep_debt_dataset.png", width=8, height =6, dpi=300)
#Gender
ggplot(sleep_data, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Number of Individuals"
  ) +
  theme_minimal()
ggsave("gender_sleep_debt_dataset.png", width=8, height =6, dpi=300)

#Chronotype
ggplot(sleep_data, aes(x = chronotype,
                       y = sleep_efficiency_pct,
                       fill = chronotype)) +
  geom_boxplot() +
  labs(
    title = "Sleep Efficiency by Chronotype",
    x = "Chronotype",
    y = "Sleep Efficiency (%)"
  ) +
  theme_minimal()
ggsave("chronotype_sleep_debt_dataset.png", width=8, height =6, dpi=300)
