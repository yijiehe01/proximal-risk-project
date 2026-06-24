
library(glmmTMB)
library(modelsummary)
library(tibble)

#centre age
addhealth_long2 <- addhealth_long2 %>%
  mutate(age_c = age_imputed - mean(age_imputed, na.rm = TRUE))

#reference category White
addhealth_long2 <- addhealth_long2 %>%
  mutate(
    race_imputed = fct_relevel(race_imputed, "White")
  )

# Mundlak decomposition
addhealth_long2 <- addhealth_long2 %>%
  group_by(AID) %>%
  mutate(
    deprivation_rank_interp_mean = mean(deprivation_rank_interp, na.rm = TRUE),
    deprivation_rank_interp_wp = deprivation_rank_interp - deprivation_rank_interp_mean,

    substance_count_mean = mean(substance_count, na.rm = TRUE),
    substance_count_wp = substance_count - substance_count_mean,

    ust_24_mean = mean(ust_24, na.rm = TRUE),
    ust_24_wp = ust_24 - ust_24_mean,

    sst_24_mean = mean(sst_24, na.rm = TRUE),
    sst_24_wp = sst_24 - sst_24_mean,

    ct_24_mean = mean(ct_24, na.rm = TRUE),
    ct_24_wp = ct_24 - ct_24_mean,

    sport_24_mean = mean(sport_24, na.rm = TRUE),
    sport_24_wp = sport_24 - sport_24_mean,

    sst_sport_24_mean = mean(sst_sport_24, na.rm = TRUE),
    sst_sport_24_wp = sst_sport_24 - sport_24_mean,

    religion_mean = mean(religion, na.rm = TRUE),
    religion_wp = religion - religion_mean,

    marriage_mean = mean(marriage, na.rm = TRUE),
    marriage_wp = marriage - marriage_mean,

    relationship_mean = mean(relationship, na.rm = TRUE),
    relationship_wp = relationship - relationship_mean,

    children_mean = mean(children, na.rm = TRUE),
    children_wp = children - children_mean
  ) %>%
  ungroup()

#model <- glmmTMB(
#  ipv_frequency ~ age_imputed + I(age_imputed^2) + deprivation_rank_interp + race_imputed +
#    education_harmonized + mother_imprisonment_harmonized + father_imprisonment_harmonized +
#    sex_imputed + (age_imputed | AID),
#  family = nbinom2,
#  data = addhealth_long2
#)

model1 <- glmmTMB(
  ipv_frequency ~ age_c + I(age_c^2) +
    deprivation_rank_interp_mean +
    deprivation_rank_interp_wp +
    substance_count_mean +
    substance_count_wp +
    ust_24_mean +
    ust_24_wp +
    sst_sport_24_mean +
    sst_sport_24_wp +
    social_learning_max_ti +
    race_imputed +
    education_harmonized +
    imprisonment_harmonized +
    mother_imprisonment_harmonized +
    father_imprisonment_harmonized +
    religion_mean +
    religion_wp +
    marriage_mean +
    marriage_wp +
    relationship_mean +
    relationship_wp +
    children_mean +
    children_wp +
    sex_imputed +
    (1 | AID),
  family = nbinom2,
  data = addhealth_long2
)

summary(model1)

coef_labels <- c(
  "age_c" = "Age (centered)",
  "I(age_c^2)" = "Age²",
  "deprivation_rank_interp_mean" = "Household deprivation (between)",
  "deprivation_rank_interp_wp" = "Household deprivation (within)",
  "substance_count_mean" = "Substance use count (between)",
  "substance_count_wp" = "Substance use count (within)",
  "ust_24_mean" = "Unstructured spare time (between)",
  "ust_24_wp" = "Unstructured spare time (within)",
  "sst_sport_24_mean" = "Structured spare time and sports (between)",
  "sst_sport_24_wp" = "Structured spare time and sports (within)",
  "social_learning_max_ti" = "Social learning",
  "race_imputedAmerican Indian or Native American" = "American Indian/Native American",
  "race_imputedAsian or Pacific Islander" = "Asian/Pacific Islander",
  "race_imputedBlack" = "Black",
  "race_imputedMixed" = "Mixed race",
  "race_imputedOthers" = "Other race",
  "race_imputedMissing_race" = "Missing race",
  "education_harmonized" = "Education",
  "imprisonment_harmonized" = "Imprisonment",
  "mother_imprisonment_harmonized" = "Mother imprisoned",
  "father_imprisonment_harmonized" = "Father imprisoned",
  "religion_mean" = "Religion (between)",
  "religion_wp" = "Religion (within)",
  "marriage_mean" = "Marriage (between)",
  "marriage_wp" = "Marriage (within)",
  "relationship_mean" = "Relationship (between)",
  "relationship_wp" = "Relationship (within)",
  "children_mean" = "Having children (between)",
  "children_wp" = "Having children (within)",
  "sex_imputed" = "Male"
)

modelsummary(
  list("IPV frequency" = model1),
  exponentiate = TRUE,
  coef_map = coef_labels,
  statistic = "conf.int",
  stars = c("†" = .10, "*" = .05, "**" = .01, "***" = .001),
  add_rows = tibble::tribble(
    ~term, ~`IPV frequency`,
    "Respondents", "4,434",
    "Person-wave observations", "12,612",
    "Waves", "III–VI",
    "Random intercept", "Yes"
  ),
  output = "markdown"
)









