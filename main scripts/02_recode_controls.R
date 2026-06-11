# ----------------------------
# AGE / BIRTH YEAR
# birth_year = respondent birth year
# age = interview year - birth year
# ----------------------------
# w1
# IYEAR is interview year variable
table(w1_data$H1GI1Y, useNA = "always")
w1_data <- w1_data %>%
  mutate(
    birth_year = case_when(
      H1GI1Y == 96 ~ NA_real_, # refused to answer
      TRUE ~ H1GI1Y
    ),
    age = IYEAR - birth_year
  )
table(w1_data$age, useNA = "always")

# w2
table(w2_data$H2GI1Y, useNA = "always")
w2_data <- w2_data %>%
  mutate(
    birth_year = H2GI1Y,
    age = IYEAR2 - birth_year
  )
table(w2_data$age, useNA = "always")

# w3
table(w3_data$H3OD1Y, useNA = "always")
w3_data <- w3_data %>%
  mutate(
    birth_year = H3OD1Y,
    age = IYEAR3 - birth_year
  )
table(w3_data$age, useNA = "always")

# w4
table(w4_data$H4OD1Y, useNA = "always")
w4_data <- w4_data %>%
  mutate(
    birth_year = H4OD1Y,
    age = IYEAR4 - birth_year
  )
table(w4_data$age, useNA = "always")

# w5
table(w5_data$H5OD1Y, useNA = "always")
w5_data <- w5_data %>%
  mutate(
    birth_year = H5OD1Y,
    age = IYEAR5 - birth_year
  )
table(w5_data$age, useNA = "always")

# w6
table(w6_data$H6OD1Y, useNA = "always")
w6_data <- w6_data %>%
  mutate(
    birth_year = H6OD1Y,
    age = IYEAR6 - birth_year
  )
table(w6_data$age, useNA = "always")

### Optional: w1 school questionnaire age variable
w1_data <- w1_data %>%
  mutate(
    school_age = S1
  )
table(w1_data$school_age, useNA = "always")

# ----------------------------
# SEX
# 0 = female, 1 = male
# ----------------------------
val_labels(w1_data$BIO_SEX)
# w1
w1_data <- w1_data %>%
  mutate(
    sex = case_when(
      BIO_SEX == 1 ~ 1,         # Male
      BIO_SEX == 2 ~ 0,         # Female
      BIO_SEX == 6 ~ NA_real_   # refused
    )
  )

# w2
val_labels(w2_data$BIO_SEX2)
table(w2_data$BIO_SEX2, useNA = "always")
w2_data <- w2_data %>%
  mutate(
    sex = case_when(
      BIO_SEX2 == 1 ~ 1,
      BIO_SEX2 == 2 ~ 0
    )
  )

# w3
val_labels(w3_data$BIO_SEX3)
table(w3_data$BIO_SEX3, useNA = "always")
w3_data <- w3_data %>%
  mutate(
    sex = case_when(
      BIO_SEX3 == 1 ~ 1,
      BIO_SEX3 == 2 ~ 0
    )
  )

# w4
val_labels(w4_data$BIO_SEX4)
table(w4_data$BIO_SEX4, useNA = "always")
w4_data <- w4_data %>%
  mutate(
    sex = case_when(
      BIO_SEX4 == 1 ~ 1,
      BIO_SEX4 == 2 ~ 0
    )
  )

# w5
w5_data <- w5_data %>%
  mutate(
    sex = case_when(
      H5OD2A == 1 ~ 1,
      H5OD2A == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# w6
w6_data <- w6_data %>%
  mutate(
    sex = case_when(
      H6OD2 == 1 ~ 1,
      H6OD2 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

### Optional: w1 school questionnaire for sex
w1_data <- w1_data %>%
  mutate(
    school_sex = case_when(
      S2 == 1 ~ 1,        # Male
      S2 == 2 ~ 0,        # Female
      S2 == 9 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# ----------------------------
# ETHNICITY
# race is not available in w2 and w4, so set to NA
# Respondents selecting more than one race are coded as mixed
# ----------------------------
# w1
val_labels(w1_data$H1GI6A)

# recode don't know and refused as NA
w1_data <- w1_data %>%
  mutate(
    across(
      c(H1GI6A, H1GI6B, H1GI6C, H1GI6D, H1GI6E),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    race_count = rowSums(
      across(c(H1GI6A_rec, H1GI6B_rec, H1GI6C_rec, H1GI6D_rec, H1GI6E_rec)),
      na.rm = TRUE
    ),

    race = case_when(
      race_count > 1 ~ 6,
      race_count == 0 ~ NA_real_,
      H1GI6A_rec == 1 ~ 1,
      H1GI6B_rec == 1 ~ 2,
      H1GI6C_rec == 1 ~ 3,
      H1GI6D_rec == 1 ~ 4,
      H1GI6E_rec == 1 ~ 5,
      TRUE ~ NA_real_
    ),

    race = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w1_data$race_count, useNA = "always")
table(w1_data$race, useNA = "always")

# w2 and w4: no race items available
w2_data <- w2_data %>%
  mutate(
    race = factor(
      NA,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian/Native American",
        "Asian/Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

w4_data <- w4_data %>%
  mutate(
    race = factor(
      NA,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian/Native American",
        "Asian/Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

# w3
val_labels(w3_data$H3OD4A)

w3_data <- w3_data %>%
  mutate(
    across(
      c(H3OD4A, H3OD4B, H3OD4C, H3OD4D),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    race_count = rowSums(
      across(c(H3OD4A_rec, H3OD4B_rec, H3OD4C_rec, H3OD4D_rec)),
      na.rm = TRUE
    ),

    race = case_when(
      race_count > 1 ~ 6,
      H3OD4A_rec == 1 ~ 1,
      H3OD4B_rec == 1 ~ 2,
      H3OD4C_rec == 1 ~ 3,
      H3OD4D_rec == 1 ~ 4,
      TRUE ~ NA_real_
    ),

    race = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w3_data$race_count, useNA = "always")

# others category is not included in w3
table(w3_data$race, useNA = "always")

# w5
table(w5_data$H5OD4A, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    race_count = rowSums(
      across(c(H5OD4A, H5OD4B, H5OD4C, H5OD4D, H5OD4E, H5OD4F, H5OD4G)),
      na.rm = TRUE
    ),

    race = case_when(
      race_count > 1 ~ 6,                          # Mixed
      race_count == 0 ~ NA_real_,
      H5OD4A == 1 ~ 1,                             # White
      H5OD4B == 1 ~ 2,                             # Black
      H5OD4F == 1 ~ 3,                             # American Indian / Alaska Native
      H5OD4D == 1 | H5OD4E == 1 ~ 4,               # Asian or Pacific Islander
      H5OD4C == 1 | H5OD4G == 1 ~ 5,               # Other
      TRUE ~ NA_real_
    ),
    race = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w5_data$race_count, useNA = "always")
table(w5_data$race, useNA = "always")

# w6
table(w6_data$H6OD3A, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    race_count = rowSums(
      across(c(H6OD3A, H6OD3B, H6OD3C, H6OD3D, H6OD3E, H6OD3F, H6OD3G, H6OD3H)),
      na.rm = TRUE
    ),

    race = case_when(
      race_count > 1 ~ 6,                          # Mixed
      race_count == 0 ~ NA_real_,
      H6OD3G == 1 ~ 1,                             # White
      H6OD3A == 1 ~ 2,                             # Black
      H6OD3E == 1 ~ 3,                             # American Indian / Alaska Native
      H6OD3C == 1 | H6OD3D == 1 ~ 4,               # Asian or Pacific Islander
      H6OD3B == 1 | H6OD3F == 1 | H6OD3H == 1 ~ 5, # Hispanic, MENA, or Other
      TRUE ~ NA_real_
    ),
    race = factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w6_data$race_count, useNA = "always")
table(w6_data$race, useNA = "always")

### Optional: w1 school questionnaire race variable
table(w1_data$S6A, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    school_race_count = rowSums(
      across(c(S6A, S6B, S6C, S6D, S6E)),
      na.rm = TRUE
    ),

    school_race = case_when(
      school_race_count > 1 ~ 6,
      school_race_count == 0 ~ NA_real_,
      S6A == 1 ~ 1,   # White
      S6B == 1 ~ 2,   # Black
      S6D == 1 ~ 3,   # Native American
      S6C == 1 ~ 4,   # Asian/Pacific Islander
      S6E == 1 ~ 5,   # Other
      TRUE ~ NA_real_
    ),

    school_race = factor(
      school_race,
      levels = c(1,2,3,4,5,6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w1_data$school_race_count, useNA = "always")
table(w1_data$school_race, useNA = "always")

### Optional: w1 parent questionnaire race variable
table(w1_data$PA6_1, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    across(
      c(PA6_1, PA6_2, PA6_3, PA6_4, PA6_5),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    parent_race_count = rowSums(
      across(c(PA6_1_rec, PA6_2_rec, PA6_3_rec, PA6_4_rec, PA6_5_rec)),
      na.rm = TRUE
    ),

    parent_race = case_when(
      parent_race_count > 1 ~ 6,
      PA6_1_rec == 1 ~ 1,   # White
      PA6_2_rec == 1 ~ 2,   # Black
      PA6_3_rec == 1 ~ 3,   # American Indian / Native American
      PA6_4_rec == 1 ~ 4,   # Asian / Pacific Islander
      PA6_5_rec == 1 ~ 5,   # Other
      TRUE ~ NA_real_
    ),

    parent_race = factor(
      parent_race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "White",
        "Black",
        "American Indian or Native American",
        "Asian or Pacific Islander",
        "Others",
        "Mixed"
      )
    )
  )

table(w1_data$parent_race_count, useNA = "always")
table(w1_data$parent_race, useNA = "always")

# ----------------------------
# PARENTS' CRIMINAL RECORDS
# BINARY
# ----------------------------
# w1, w2, w6: not measured
w1_data <- w1_data %>%
  mutate(
    mother_criminal = NA_real_,
    father_criminal = NA_real_
  )

w2_data <- w2_data %>%
  mutate(
    mother_criminal = NA_real_,
    father_criminal = NA_real_
  )

w6_data <- w6_data %>%
  mutate(
    mother_criminal = NA_real_,
    father_criminal = NA_real_
  )

# w3: biological father only
val_labels(w3_data$H3CJ160)
table(w3_data$H3CJ160, useNA = "always")

w3_data <- w3_data %>%
  mutate(
    mother_criminal = NA_real_,

    father_criminal = case_when(
      H3CJ160 == 1 ~ 1,
      H3CJ160 == 0 ~ 0,
      H3CJ160 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )
table(w3_data$father_criminal, useNA = "always")

# w4
# figure questions not asked for those have biological parents
# so recode these skips as NA

table(w4_data$H4WP16, useNA = "always")
table(w4_data$H4WP30, useNA = "always")

# w4
w4_data <- w4_data %>%
  mutate(
    bio_mother_jail = case_when(
      H4WP3 == 1 ~ 1,
      H4WP3 == 0 ~ 0,
      H4WP3 %in% c(6, 8) ~ NA_real_
    ),

    mother_figure_jail = case_when(
      H4WP16 == 1 ~ 1,
      H4WP16 == 0 ~ 0,
      H4WP16 %in% c(7, 8) ~ NA_real_
    ),

    bio_father_jail = case_when(
      H4WP9 == 1 ~ 1,
      H4WP9 == 0 ~ 0,
      H4WP9 %in% c(6, 8) ~ NA_real_
    ),

    father_figure_jail = case_when(
      H4WP30 == 1 ~ 1,
      H4WP30 == 0 ~ 0,
      H4WP30 %in% c(7, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    mother_criminal = case_when(
      bio_mother_jail == 1 | mother_figure_jail == 1 ~ 1,
      bio_mother_jail == 0 | mother_figure_jail == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    father_criminal = case_when(
      bio_father_jail == 1 | father_figure_jail == 1 ~ 1,
      bio_father_jail == 0 | father_figure_jail == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w4_data$mother_criminal, useNA = "always")
table(w4_data$father_criminal, useNA = "always")

# w5
# figure questions not asked for those have biological parents
table(w5_data$H5WP8, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    bio_mother_jail = case_when(
      H5WP1 == 1 ~ 1,
      H5WP1 == 0 ~ 0,
      H5WP1 == 98 ~ NA_real_
    ),

    mother_figure_jail = case_when(
      H5WP8 == 1 ~ 1,
      H5WP8 == 0 ~ 0,
      H5WP8 %in% c(97, 98) ~ NA_real_
    ),

    bio_father_jail = case_when(
      H5WP15 == 1 ~ 1,
      H5WP15 == 0 ~ 0,
      H5WP15 == 98 ~ NA_real_
    ),

    father_figure_jail = case_when(
      H5WP22 == 1 ~ 1,
      H5WP22 == 0 ~ 0,
      H5WP22 %in% c(97, 98) ~ NA_real_
    ),

    mother_criminal = case_when(
      bio_mother_jail == 1 | mother_figure_jail == 1 ~ 1,
      bio_mother_jail == 0 | mother_figure_jail == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    father_criminal = case_when(
      bio_father_jail == 1 | father_figure_jail == 1 ~ 1,
      bio_father_jail == 0 | father_figure_jail == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w5_data$mother_criminal, useNA = "always")
table(w5_data$father_criminal, useNA = "always")

# ----------------------------
# IMPRISONMENT
# BINARY
# ----------------------------
# W4
val_labels(w4_data$H4CJ17)
table(w4_data$H4CJ17, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    imprisonment = case_when(
      PRISON4 == 1 ~ 1,
      H4CJ17 == 1 ~ 1,
      H4CJ17 == 0 ~ 0,
      H4CJ17 == 7 ~ 0,
      H4CJ17 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w4_data$imprisonment, useNA = "always")

# W5
table(w5_data$H5CJ5, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    imprisonment = case_when(
      H5CJ5 == 1 ~ 1,
      H5CJ5 %in% c(0, 97) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w5_data$imprisonment, useNA = "always")

# W6
table(w6_data$H6CJ30, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    imprisonment = H6CJ30
  )

table(w6_data$imprisonment, useNA = "always")

# imprisonment only available in w4-6
w1_data <- w1_data %>% mutate(imprisonment = NA_real_)
w2_data <- w2_data %>% mutate(imprisonment = NA_real_)
w3_data <- w3_data %>% mutate(imprisonment = NA_real_)

# ----------------------------
# RELIGION
# BINARY
# ----------------------------
# w1
val_labels(w1_data$H1RE1)
table(w1_data$H1RE1, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    religion = case_when(
      H1RE1 == 0 ~ 0,
      H1RE1 %in% 1:28 ~ 1,
      H1RE1 %in% c(96,98,99) ~ NA_real_
    )
  )
table(w1_data$religion, useNA = "always")

# w2
val_labels(w2_data$H2RE1)
table(w2_data$H2RE1, useNA = "always")

w2_data <- w2_data %>%
  mutate(
    religion = case_when(
      H2RE1 %in% c(0,29) ~ 0,
      H2RE1 %in% 1:28 ~ 1,
      H2RE1 %in% c(96,98,99) ~ NA_real_
    )
  )
table(w2_data$religion, useNA = "always")

# w3
val_labels(w3_data$H3RE1)
table(w3_data$H3RE1, useNA = "always")

w3_data <- w3_data %>%
  mutate(
    religion = case_when(
      H3RE1 == 0 ~ 0,
      H3RE1 %in% 1:8 ~ 1,
      H3RE1 %in% c(96,98,99) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )
table(w3_data$religion, useNA = "always")

# w4
val_labels(w4_data$H4RE1)
table(w4_data$H4RE1, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    religion = case_when(
      H4RE1 == 1 ~ 0,
      H4RE1 %in% 2:9 ~ 1,
      H4RE1 %in% c(96,98) ~ NA_real_
    )
  )
table(w4_data$religion, useNA = "always")

# w5
table(w5_data$H5RE1, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    religion = case_when(
      H5RE1 == 1 ~ 0,
      H5RE1 %in% c(2,3,4,5,6,9) ~ 1,
      TRUE ~ NA_real_
    )
  )
table(w5_data$religion, useNA = "always")

# w6
table(w6_data$H6RE1, useNA = "always")
w6_data <- w6_data %>%
  mutate(
    religion = case_when(
      H6RE1 == 1 ~ 0,
      H6RE1 %in% c(2,3,4,5,6,8,9) ~ 1,
      TRUE ~ NA_real_
    )
  )
table(w6_data$religion, useNA = "always")

### Optional: w1 parent questionnaire for religion
val_labels(w1_data$PA22)
table(w1_data$PA22, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    parent_religion = case_when(
      PA22 == 28 ~ 0,          # None
      PA22 %in% c(1:21, 23:27) ~ 1,
      PA22 == 96 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w1_data$parent_religion, useNA = "always")
# ----------------------------
# EDUCATION
# ----------------------------
# w5
table(w5_data$H5OD11, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    education = case_when(
      H5OD11 == 2 ~ 0,          # Below high school
      H5OD11 %in% c(3:9) ~ 1,   # High school/GED/some post-secondary but no bachelor's
      H5OD11 %in% 10:16 ~ 2,    # Bachelor's degree or above
      TRUE ~ NA_real_
    )
  )
table(w5_data$education, useNA = "always")

# w6
table(w6_data$H6OD9, useNA = "always")
w6_data <- w6_data %>%
  mutate(
    education = case_when(
      H6OD9 %in% c(1,2) ~ 0,  # Below high school
      H6OD9 %in% 3:9 ~ 1,     # High school/GED/some post-secondary but no bachelor's
      H6OD9 %in% 10:16 ~ 2,   # Bachelor's degree or above
      TRUE ~ NA_real_
    )
  )

table(w6_data$education, useNA = "always")

# education only available in w5,w6
w1_data <- w1_data %>% mutate(education = NA_real_)
w2_data <- w2_data %>% mutate(education = NA_real_)
w3_data <- w3_data %>% mutate(education = NA_real_)
w4_data <- w4_data %>% mutate(education = NA_real_)

# ----------------------------
# MARRIAGE/RELATIONSHIP
# BINARY
# RELATIONSHIP = 1 if MARRIAGE = 1
# ----------------------------
# w1 marriage
val_labels(w1_data$H1GI15)
table(w1_data$H1GI15, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    marriage = case_when(
      H1GI15 == 1 ~ 1,
      H1GI15 == 0 ~ 0,
      H1GI15 == 7 & !is.na(age) ~ 0,        # legitimate skip age<15
      H1GI15 == 7 & is.na(age) ~ NA_real_,
      H1GI15 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w1_data$marriage, useNA = "always")

# w1 relationship
val_labels(w1_data$H1RR1)
table(w1_data$H1RR1, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    relationship = case_when(
      H1RR1 == 1 ~ 1,
      H1RR1 == 0 ~ 0,
      H1RR1 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    relationship = case_when(
      marriage == 1 ~ 1,
      TRUE ~ relationship
    )
  )

table(w1_data$relationship, useNA = "always")

# w2 marriage
val_labels(w2_data$H2GI5)
table(w2_data$H2GI5, useNA = "always")

w2_data <- w2_data %>%
  mutate(
    marriage = case_when(
      H2GI5 == 2 ~ 1,
      H2GI5 == 1 ~ 0,
      H2GI5 == 7 & !is.na(age) ~ 0,
      H2GI5 == 7 & is.na(age) ~ NA_real_,
      H2GI5 == 8 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w2_data$marriage, useNA = "always")

# w2 relationship
val_labels(w2_data$H2RR2A)
table(w2_data$H2RR2A, useNA = "always")

w2_data <- w2_data %>%
  mutate(
    relationship = case_when(
      H2RR2A == 1 ~ 1,
      H2RR2A == 0 ~ 0,
      H2RR2A %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    relationship = case_when(
      marriage == 1 ~ 1,
      TRUE ~ relationship
    )
  )

table(w2_data$relationship, useNA = "always")

# w3 marriage
val_labels(w3_partner$H3TR12)
table(w3_partner$H3TR12, useNA = "always")

w3_marriage <- w3_partner %>%
  group_by(AID) %>%
  summarise(
    marriage = case_when(
      any(H3TR12 == 1, na.rm = TRUE) ~ 1,
      any(H3TR12 %in% c(0, 2), na.rm = TRUE) ~ 0,
      all(is.na(H3TR12) | H3TR12 %in% c(6, 8, 9)) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

table(w3_marriage$marriage, useNA = "always")

# merge with the main dataset
# respondents not in relationship subset had no listed recent relationship, so relationship = 0
w3_data <- w3_data %>%
  left_join(w3_marriage, by = "AID") %>%
  mutate(
    marriage = case_when(
      !is.na(marriage) ~ marriage,
      AID %in% w3_marriage$AID ~ NA_real_,
      TRUE ~ 0
    )
  )
table(w3_data$marriage, useNA = "always")

# w3 relationship
val_labels(w3_partner$H3TR1)
table(w3_partner$H3TR1, useNA = "always")

w3_relationship <- w3_partner %>%
  group_by(AID) %>%
  summarise(
    relationship = case_when(
      any(H3TR1 == 1, na.rm = TRUE) ~ 1,
      any(H3TR1 == 0, na.rm = TRUE) ~ 0,
      all(is.na(H3TR1) | H3TR1 %in% c(6, 8, 9)) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

table(w3_relationship$relationship, useNA = "always")

# merge back into main W3 data
# respondents not in relationship subset had no listed recent relationship, so relationship = 0
w3_data <- w3_data %>%
  left_join(w3_relationship, by = "AID") %>%
  mutate(
    relationship = case_when(
      marriage == 1 ~ 1,              # marriage implies relationship
      !is.na(relationship) ~ relationship,
      AID %in% w3_relationship$AID ~ NA_real_,
      TRUE ~ 0
    )
  )

table(w3_data$relationship, useNA = "always")

# w4 marriage
val_labels(w4_partner$H4TR13)
table(w4_partner$H4TR13, useNA = "always")

w4_marriage <- w4_partner %>%
  group_by(AID) %>%
  summarise(
    marriage = case_when(
      any(H4TR13 == 1, na.rm = TRUE) ~ 1,
      any(H4TR13 %in% c(2, 3, 4, 5), na.rm = TRUE) ~ 0,
      all(is.na(H4TR13)) ~ NA_real_
    ),
    .groups = "drop"
  )

table(w4_marriage$marriage, useNA = "always")

# merge back into main W4 data
w4_data <- w4_data %>%
  left_join(w4_marriage, by = "AID") %>%
  mutate(
    marriage = case_when(
      !is.na(marriage) ~ marriage,
      AID %in% w4_marriage$AID ~ NA_real_,
      TRUE ~ 0
    )
  )
table(w4_data$marriage, useNA = "always")

# w4 relationship
val_labels(w4_partner$H4TR13)
table(w4_partner$H4TR13, useNA = "always")

val_labels(w4_partner$H4TR14)
table(w4_partner$H4TR14, useNA = "always")

w4_relationship <- w4_partner %>%
  group_by(AID) %>%
  summarise(
    relationship_subset = case_when(
      any(H4TR13 %in% c(1, 2, 4), na.rm = TRUE) ~ 1,
      any(H4TR13 == 3 & H4TR14 == 1, na.rm = TRUE) ~ 1,
      any(H4TR13 %in% c(3, 5), na.rm = TRUE) |
        any(H4TR14 == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

table(w4_relationship$relationship_subset, useNA = "always")

# not counting partners,how many others are you currently having a relationship
table(w4_data$H4TR4, useNA = "always")

# merge with W4 main data
w4_data <- w4_data %>%
  left_join(w4_relationship, by = "AID") %>%
  mutate(
    relationship = case_when(
      marriage == 1 ~ 1,
      relationship_subset == 1 ~ 1,
      H4TR4 > 0 & H4TR4 < 96 ~ 1,
      relationship_subset == 0 & H4TR4 == 0 ~ 0,
      H4TR4 %in% c(96, 98) ~ NA_real_,
      AID %in% w4_relationship$AID & is.na(relationship_subset) ~ NA_real_,
      TRUE ~ 0
    )
  )
table(w4_data$relationship, useNA = "always")

# w5 marriage
# current marriage status (binary)
table(w5_data$H5TR4, useNA = "always")

w5_data <- w5_data %>%
  mutate(marriage = H5TR4)

# w5 relationship
# number of romantic/sexual relationship
table(w5_data$H5TR3, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    relationship = case_when(
      marriage == 1 ~ 1,
      H5TR3 > 0 ~ 1,
      H5TR3 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(w5_data$relationship, useNA = "always")

# w6 marriage
# current marriage status (category)
table(w6_data$H6HR1, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    marriage = case_when(
      H6HR1 == 1 ~ 1,
      H6HR1 %in% c(2,3,4,5) ~ 0, # single/widowed/divorced/separated
      TRUE ~ NA_real_
    )
  )
table(w6_data$marriage, useNA = "always")

# w6 relationship
# number of romantic/sexual relationship
table(w6_data$H6TR3, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    relationship = case_when(
      marriage == 1 ~ 1,
      H6TR3 > 0 ~ 1,
      H6TR3 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w6_data$relationship, useNA = "always")

### Optional: w1 parent questionnaire for marriage
val_labels(w1_data$PA10)
table(w1_data$PA10, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    parent_marriage = case_when(
      PA10 == 2 ~ 1,                  # married
      PA10 %in% c(1, 3, 4, 5) ~ 0,    # single/widowed/divorced/separated
      PA10 == 6 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w1_data$parent_marriage, useNA = "always")

# ----------------------------
# HOUSEHOLD SIZE
# household_size = number of people in household, including respondent
# ----------------------------
w1_hh_vars <- paste0("H1HR2", LETTERS[1:20])

# check distributions, sex of the household number
lapply(
  w1_data %>% select(all_of(w1_hh_vars)),
  function(x) table(x, useNA = "always")
)

w1_data <- w1_data %>%
  mutate(
    across(
      all_of(w1_hh_vars),
      ~ case_when(
        .x %in% c(1, 2) ~ 1,          # valid household member (female/male)
        .x == 7 ~ 0,                  # legitimate skip / no member in slot
        .x %in% c(6, 8, 9) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    household_members = rowSums(
      across(all_of(paste0(w1_hh_vars, "_rec"))),
      na.rm = TRUE
    ),

    household_size = case_when(
      H1HR2A %in% c(6, 8, 9) ~ NA_real_, # unclear first household member response
      TRUE ~ household_members + 1
    )
  )

table(w1_data$household_members, useNA = "always")
table(w1_data$household_size, useNA = "always")

# HOUSEHOLD SIZE wave 2
# anyone else living in the household in multiple items
w2_hh_vars <- paste0("H2HR2", LETTERS[1:17])

lapply(
  w2_data %>% select(all_of(w2_hh_vars)),
  function(x) table(x, useNA = "always")
)

# living alone = 2
table(w2_data$H2HR1, useNA = "always")

w2_data <- w2_data %>%
  mutate(
    across(
      all_of(w2_hh_vars),
      ~ case_when(
        .x == 1 ~ 1,                  # valid listed household member
        .x %in% c(0, 7) ~ 0,          # no member / legitimate skip
        .x %in% c(6, 8, 9) ~ NA_real_,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    household_members = rowSums(
      across(all_of(paste0(w2_hh_vars, "_rec"))),
      na.rm = TRUE
    ),

    household_size = case_when(
      H2HR1 == 2 ~ 1,                 # respondent lives alone
      H2HR1 == 1 & H2HR2A %in% c(6, 8, 9) ~ NA_real_,
      H2HR1 == 1 ~ household_members + 1,
      TRUE ~ NA_real_
    )
  )

table(w2_data$household_members, useNA = "always")
table(w2_data$household_size, useNA = "always")

# HOUSEHOLD SIZE wave 3

# where do you live
val_labels(w3_data$H3HR2)
table(w3_data$H3HR2, useNA = "always")

# do you live alone or with others
val_labels(w3_data$H3HR5)
table(w3_data$H3HR5, useNA = "always")

# how many people live with you
table(w3_data$H3HR6, useNA = "always")

w3_data <- w3_data %>%
  mutate(
    household_size = case_when(
      H3HR2 %in% c(4, 5, 99) ~ NA_real_,        # group quarters / homeless (skip-logic)
      H3HR6 == 97 & H3HR5 == 1 ~ 1,         # lives alone
      H3HR6 >= 1 & H3HR6 < 96 ~ H3HR6 + 1,  # add respondent
      H3HR5 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w3_data$household_size, useNA = "always")

# HOUSEHOLD SIZE wave 4
# where do you live
val_labels(w4_data$H4HR1)
table(w4_data$H4HR1, useNA = "always")

# do you live alone or with others
val_labels(w4_data$H4HR3)
table(w4_data$H4HR3, useNA = "always")

# how many people live with you
val_labels(w4_data$H4HR4)
table(w4_data$H4HR4, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    household_size = case_when(
      H4HR1 %in% c(4, 5, 96) ~ NA_real_,        # group quarters / homeless (skip-logic)
      H4HR4 == 97 & H4HR3 == 1 ~ 1,         # lives alone
      H4HR4 >= 1 & H4HR4 < 96 ~ H4HR4 + 1,  # add respondent
      H4HR3 %in% c(6,7,8) | H4HR4 %in% c(96, 98) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w4_data$household_size, useNA = "always")

# HOUSEHOLD SIZE wave 5
# where do you live
table(w5_data$H5HR2, useNA = "always")

# how many other people live in your household
table(w5_data$H5HR3, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    household_size = case_when(

      H5HR2 %in% c(5, 6) ~ NA_real_,       # homeless / other
      H5HR3 >= 0 & H5HR3 <= 9 ~ H5HR3 + 1, # valid count of other household members
      TRUE ~ NA_real_
    )
  )

table(w5_data$household_size, useNA = "always")

# HOUSEHOLD SIZE wave 6

# where do you live
table(w6_data$H6HR2, useNA = "always")

# how many other people live in your household
table(w6_data$H6HR3, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    household_size = case_when(

      H6HR2 %in% c(4, 5) ~ NA_real_,       # homeless / other
      H6HR3 >= 0 & H6HR3 <= 8 ~ H6HR3 + 1, # valid count of other household members
      TRUE ~ NA_real_
    )
  )

table(w6_data$household_size, useNA = "always")

# ----------------------------
# Have Living Children
# BINARY
# ----------------------------
# w1
w1_data <- w1_data %>%
  mutate(
    children = case_when(
      H1FP24A1 == 1 | H1FP24A2 == 1 ~ 1,

      H1FP24A1 %in% c(0, 7) &
        H1FP24A2 %in% c(0, 7) ~ 0,

      is.na(H1FP24A1) &
        is.na(H1FP24A2) ~ NA_real_
    )
  )

table(w1_data$children, useNA = "always")

# w2
w2_data <- w2_data %>%
  mutate(
    children = case_when(
      H2FP28A1 == 1 |
        H2FP28A2 == 1 |
        H2FP28A3 == 1 ~ 1,

      H2FP28A1 %in% c(0, 7) &
        H2FP28A2 %in% c(0, 7) &
        H2FP28A3 %in% c(0, 7) ~ 0,

      is.na(H2FP28A1) &
        is.na(H2FP28A2) &
        is.na(H2FP28A3) ~ NA_real_
    )
  )
table(w2_data$children, useNA = "always")

# w3
# use the birth-level subset and then merge with w3 main dataset
val_labels(w3_birth$H3LB11)
table(w3_birth$H3LB11, useNA = "always")

# Collapse live-birth subset to respondent level
w3_children <- w3_birth %>%
  group_by(AID) %>%
  summarise(
    children = case_when(
      any(H3LB11 == 1, na.rm = TRUE) ~ 1,
      any(H3LB11 == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

table(w3_children$children, useNA = "always")

# Merge back into main Wave 3 data
w3_data <- w3_data %>%
  select(-any_of("children")) %>%
  left_join(w3_children, by = "AID") %>%
  mutate(
    children = case_when(
      children == 1 ~ 1,
      children == 0 ~ 0,
      is.na(children) & AID %in% w3_children$AID ~ NA_real_,
      is.na(children) & !(AID %in% w3_children$AID) ~ 0
    )
  )

table(w3_data$children, useNA = "always")

# w4
# use the birth-level subset and then merge with w4 main dataset
val_labels(w4_birth$H4LB10)
table(w4_birth$H4LB10, useNA = "always")

# Collapse birth-level file to respondent level
w4_children <- w4_birth %>%
  group_by(AID) %>%
  summarise(
    children = case_when(
      any(H4LB10 == 1, na.rm = TRUE) ~ 1,
      any(H4LB10 == 0, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    ),
    .groups = "drop"
  )

table(w4_children$children, useNA = "always")

# Merge back into main Wave 4 data
w4_data <- w4_data %>%
  select(-any_of("children")) %>%
  left_join(w4_children, by = "AID") %>%
  mutate(
    children = case_when(
      children == 1 ~ 1,
      children == 0 ~ 0,
      is.na(children) & AID %in% w4_children$AID ~ NA_real_,
      is.na(children) & !(AID %in% w4_children$AID) ~ 0
    )
  )

table(w4_data$children, useNA = "always")

# w5
w5_child_vars <- c("H5PG261", "H5PG262", "H5PG263",
                  "H5PG264", "H5PG265", "H5PG266")

w5_data <- w5_data %>%
  mutate(
    across(
      all_of(w5_child_vars),
      ~ case_when(
        .x == 1 ~ 1,
        .x %in% c(0, 97) ~ 0,
        .x == 98 | is.na(.x) ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    children = case_when(
      if_any(all_of(paste0(w5_child_vars, "_rec")), ~ .x == 1) ~ 1,
      if_any(all_of(paste0(w5_child_vars, "_rec")), ~ .x == 0) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w5_data$children, useNA = "always")

# w6
w6_child_vars <- c("H6PG22A", "H6PG22B", "H6PG22C",
                   "H6PG22D", "H6PG22E", "H6PG22F")

w6_data <- w6_data %>%
  mutate(
    across(
      all_of(w6_child_vars),
      ~ case_when(
        .x == 1 ~ 1,
        .x %in% c(0, -9997) ~ 0,
        .x == -9998 | is.na(.x) ~ NA_real_
      ),
      .names = "{.col}_rec"
    ),

    children = case_when(
      if_any(all_of(paste0(w6_child_vars, "_rec")), ~ .x == 1) ~ 1,
      if_any(all_of(paste0(w6_child_vars, "_rec")), ~ .x == 0) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w6_data$children, useNA = "always")

# ----------------------------
# SOCIAL LEARNING
# one binary; one maximum value
# ----------------------------
# w1
# Drinking monthly
val_labels(w1_data$H1TO29)
table(w1_data$H1TO29, useNA = "always")

# Smoking daily
val_labels(w1_data$H1TO9)
table(w1_data$H1TO9, useNA = "always")

# Marijuana monthly
val_labels(w1_data$H1TO33)
table(w1_data$H1TO33, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    social_learning_max = pmax(
      ifelse(H1TO29 %in% 0:3, H1TO29, NA),
      ifelse(H1TO9  %in% 0:3, H1TO9,  NA),
      ifelse(H1TO33 %in% 0:3, H1TO33, NA),
      na.rm = TRUE
    ),

    social_learning_bin = case_when(
      social_learning_max > 0 ~ 1,
      social_learning_max == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w1_data$social_learning_max, useNA = "always")
table(w1_data$social_learning_bin, useNA = "always")

# w2
# Gang initiation
val_labels(w2_data$H2DS14)
table(w2_data$H2DS14, useNA = "always")

# Drinking monthly
val_labels(w2_data$H2TO41)
table(w2_data$H2TO41, useNA = "always")

# Smoking daily
val_labels(w2_data$H2TO10)
table(w2_data$H2TO10, useNA = "always")

# Marijuana monthly
val_labels(w2_data$H2TO48)
table(w2_data$H2TO48, useNA = "always")

w2_data <- w2_data %>%
  mutate(
    gang_member = case_when(
      H2DS14 == 1 ~ 1,
      H2DS14 == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    social_learning_max = pmax(
      ifelse(H2TO41 %in% 0:3, H2TO41, NA),
      ifelse(H2TO10 %in% 0:3, H2TO10, NA),
      ifelse(H2TO48 %in% 0:3, H2TO48, NA),
      na.rm = TRUE
    ),

    social_learning_bin = case_when(
      gang_member == 1 ~ 1,
      social_learning_max > 0 ~ 1,
      social_learning_max == 0 & gang_member == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w2_data$social_learning_max, useNA = "always")
table(w2_data$social_learning_bin, useNA = "always")

# w3
# Gang initiation
val_labels(w3_data$H3DS13)
table(w3_data$H3DS13, useNA = "always")

# Drinking monthly
val_labels(w3_data$H3TO103)
table(w3_data$H3TO103, useNA = "always")

# Binge drinking monthly (asked if drinking monthly=1,2,3)
val_labels(w3_data$H3TO104)
table(w3_data$H3TO104, useNA = "always")

# w3
w3_data <- w3_data %>%
  mutate(
    gang_member = case_when(
      H3DS13 == 1 ~ 1,
      H3DS13 == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    friend_drink = case_when(
      H3TO103 %in% 0:3 ~ H3TO103,
      TRUE ~ NA_real_
    ),

    friend_binge = case_when(
      H3TO104 %in% 0:3 ~ H3TO104,
      H3TO104 == 7 & H3TO103 == 0 ~ 0, # legitimate skip
      H3TO104 == 7 & H3TO103 %in% c(6, 8, 9) ~ NA_real_,
      H3TO104 == 7 & is.na(H3TO103) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    social_learning_max = pmax(
      friend_drink,
      friend_binge,
      na.rm = TRUE
    ),

    social_learning_bin = case_when(
      gang_member == 1 ~ 1,
      social_learning_max > 0 ~ 1,
      social_learning_max == 0 & gang_member == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w3_data$friend_drink, useNA = "always")
table(w3_data$friend_binge, useNA = "always")
table(w3_data$social_learning_max, useNA = "always")
table(w3_data$social_learning_bin, useNA = "always")


# w4, w5, w6: not measured
w4_data <- w4_data %>%
  mutate(
    social_learning_bin = NA_real_,
    social_learning_max = NA_real_
  )

w5_data <- w5_data %>%
  mutate(
    social_learning_bin = NA_real_,
    social_learning_max = NA_real_
  )

w6_data <- w6_data %>%
  mutate(
    social_learning_bin = NA_real_,
    social_learning_max = NA_real_
  )
