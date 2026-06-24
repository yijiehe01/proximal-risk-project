
library(dplyr)
library(ggplot2)

#Age-IPV curve (incidence)
ggplot(
  addhealth_long2 %>%
    filter(!is.na(age_imputed),
           !is.na(ipv_frequency)),
  aes(x = age_imputed, y = ipv_frequency)
) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Age",
    y = "Mean IPV frequency",
    title = "Smoothed Age-IPV Curve"
  ) +
  theme_minimal()

#Age-IPV curve (prevalence)
ggplot(
  addhealth_long2 %>%
    filter(!is.na(age_imputed),
           !is.na(ipv_bin)),
  aes(x = age_imputed, y = ipv_bin)
) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Age",
    y = "Prevalence IPV",
    title = "Smoothed Age-IPV Curve"
  ) +
  theme_minimal()

#Time use age curves
addhealth_long2 %>%
  select(age_imputed, ust_24, sst_24, ct_24, sport_24) %>%
  pivot_longer(
    cols = c(ust_24, sst_24, ct_24, sport_24),
    names_to = "activity",
    values_to = "hours"
  ) %>%
  filter(
    !is.na(age_imputed),
    !is.na(hours)
  ) %>%
  ggplot(aes(x = age_imputed, y = hours, colour = activity)) +
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    x = "Age",
    y = "Average hours per day",
    colour = "Activity",
    title = "Age curves for daily activities"
  ) +
  theme_minimal()


