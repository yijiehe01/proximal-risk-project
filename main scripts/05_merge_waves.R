# merge coded variables across waves

coded_vars <- c(
  "AID",
  "wave",
  "wave_label",
  "age",
  "sex",
  "race",
  "mother_criminal",
  "father_criminal",
  "imprisonment",
  "religion",
  "education",
  "marriage",
  "relationship",
  "household_size",
  "children",
  "social_learning_bin",
  "social_learning_max",
  "substance_bin",
  "substance_count",
  "substance_frequency",
  "household_income",
  "year",
  "violence_bin",
  "violence_frequency",
  "ipv_bin",
  "ipv_frequency"
)

w1_coded <- w1_data %>%
  mutate(wave = 1,
         wave_label = "Wave 1") %>%
  select(all_of(coded_vars))

w2_coded <- w2_data %>%
  mutate(wave = 2,
         wave_label = "Wave 2") %>%
  select(all_of(coded_vars))

w3_coded <- w3_data %>%
  mutate(wave = 3,
         wave_label = "Wave 3") %>%
  select(all_of(coded_vars))

w4_coded <- w4_data %>%
  mutate(wave = 4,
         wave_label = "Wave 4") %>%
  select(all_of(coded_vars))

w5_coded <- w5_data %>%
  mutate(wave = 5,
         wave_label = "Wave 5") %>%
  select(all_of(coded_vars))

w6_coded <- w6_data %>%
  mutate(wave = 6,
         wave_label = "Wave 6") %>%
  select(all_of(coded_vars))

addhealth_long <- bind_rows(
  w1_coded,
  w2_coded,
  w3_coded,
  w4_coded,
  w5_coded,
  w6_coded
)

# some checks
sum(duplicated(addhealth_long[, c("AID", "wave")]))
dim(addhealth_long)
names(addhealth_long)
table(addhealth_long$wave)
summary(addhealth_long)

