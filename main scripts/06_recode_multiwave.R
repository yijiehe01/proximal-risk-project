
library(tidyverse)
library(zoo)

# standardize household income
addhealth_long2 <- addhealth_long %>%
  mutate(
    household_income_equiv = household_income / sqrt(household_size)
  ) %>%
  group_by(wave) %>%
  mutate(
    deprivation_rank = 1 - percent_rank(household_income_equiv)
  ) %>%
  ungroup() %>%
  select(-household_income_equiv)

# impute missing data based on linear interpolation considering survey year
# (only when valid data for previous and next wave)
addhealth_long2 <- addhealth_long2 %>%
  arrange(AID, year) %>%
  group_by(AID) %>%
  mutate(
    deprivation_rank_interp =
      na.approx(
        deprivation_rank,
        x = year,
        na.rm = FALSE,
        maxgap = 2
      )
  ) %>%
  ungroup()

tibble(
  original_NA = sum(is.na(addhealth_long2$deprivation_rank)),
  interpolated_NA = sum(is.na(addhealth_long2$deprivation_rank_interp)),
  filled = sum(is.na(addhealth_long2$deprivation_rank) &
                 !is.na(addhealth_long2$deprivation_rank_interp))
)

# impute missing and changing race based on mode
get_mode <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) return(NA_character_)

  tab <- table(x)

  # no unique mode
  if (sum(tab == max(tab)) > 1) {
    return(NA_character_)
  }

  names(tab)[which.max(tab)]
}

addhealth_long2 <- addhealth_long2 %>%
  group_by(AID) %>%
  mutate(
    n_valid_race = n_distinct(race, na.rm = TRUE),
    race_mode = get_mode(race),
    race_imputed = case_when(
      !is.na(race_mode) ~ race_mode,
      TRUE ~ "Missing_race"
    )
  ) %>%
  ungroup() %>%
  mutate(
    race_imputed = factor(race_imputed),
    race_imputed = relevel(race_imputed, ref = "White")
  ) %>%
  select(-race_mode, -n_valid_race)

# impute missing education as time-invariant so 2 bachelor, 1 high school, 0 below
education_lookup <- addhealth_long2 %>%
  group_by(AID) %>%
  summarize(
    education_harmonized = ifelse( all(is.na(education)), NA,
        max(education, na.rm = TRUE)
      ),
    .groups = "drop"
  )

addhealth_long2 <- addhealth_long2 %>%
  left_join(education_lookup, by = "AID")

# impute mother and father imprisonment as time-invariant so 1 yes and 0 no
addhealth_long2 <- addhealth_long2 %>%
  group_by(AID) %>%
  mutate(
    mother_imprisonment_harmonized = case_when(
      any(mother_criminal == 1, na.rm = TRUE) ~ 1,
      any(mother_criminal == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    father_imprisonment_harmonized = case_when(
      any(father_criminal == 1, na.rm = TRUE) ~ 1,
      any(father_criminal == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# 1 missing value for sex, recode based on mode
sex_lookup <- addhealth_long2 %>%
  group_by(AID) %>%
  summarize(
    sex_harmonized = first(na.omit(sex)),
    .groups = "drop"
  )

addhealth_long2 <- addhealth_long2 %>%
  left_join(sex_lookup, by = "AID") %>%
  mutate(
    sex_imputed = ifelse(is.na(sex), sex_harmonized, sex)
  ) %>%
  select(-sex_harmonized)

# 3 missing values for age, recode based on previous known age
addhealth_long2 %>%
  filter(is.na(age)) %>%
  select(AID, wave, year)

addhealth_long2 <- addhealth_long2 %>%
  arrange(AID, wave) %>%
  group_by(AID) %>%
  mutate(
    age_imputed = case_when(
      is.na(age) & !is.na(lead(age)) ~
        lead(age) - (lead(year) - year),
      TRUE ~ age
    )
  ) %>%
  ungroup()

# impute imprisonment as time-invariant
addhealth_long2 <- addhealth_long2 %>%
  group_by(AID) %>%
  mutate(
    imprisonment_harmonized = case_when(
      any(imprisonment == 1, na.rm = TRUE) ~ 1,
      any(imprisonment == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# impute social learning as time-invariant
social_learning_lookup <- addhealth_long2 %>%
  group_by(AID) %>%
  summarise(social_learning_max_ti = ifelse(all(is.na(social_learning_max)), NA,
        max(social_learning_max, na.rm = TRUE)),
    .groups = "drop")

addhealth_long2 <- addhealth_long2 %>%
  left_join(social_learning_lookup, by = "AID")

