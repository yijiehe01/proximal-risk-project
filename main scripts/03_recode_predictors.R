# ----------------------------
# SUBSTANCE USE
# ----------------------------
# w1
# substance_bin = any past 30 day substance use, including needle injection
# substance_count = number of substance types used, excluding needle injection
# substance_frequency = summed 0-3 intensity score, including needle injection

val_labels(w1_data$H1TO32)
table(w1_data$H1TO32, useNA = "always") # marijuana, past 30 days

val_labels(w1_data$H1TO36)
table(w1_data$H1TO36, useNA = "always") # cocaine, past 30 days

val_labels(w1_data$H1TO42)
table(w1_data$H1TO42, useNA = "always") # other illegal drugs, past 30 days

table(w1_data$H1TO44, useNA = "always") # ever injected illegal drug

val_labels(w1_data$H1TO46)
table(w1_data$H1TO46, useNA = "always") # injected illegal drug frequency, past 30 days

w1_data <- w1_data %>%
  mutate(
    # Substance-specific binaries
    marijuana_bin = case_when(
      H1TO32 > 0 & H1TO32 < 996 ~ 1,
      H1TO32 == 0 | H1TO32 == 997 ~ 0,   # 997 = has not tried marijuana
      H1TO32 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    cocaine_bin = case_when(
      H1TO36 > 0 & H1TO36 < 996 ~ 1,
      H1TO36 == 0 | H1TO36 == 997 ~ 0,   # 997 = never tried cocaine
      H1TO36 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H1TO42 > 0 & H1TO42 < 996 ~ 1,
      H1TO42 == 0 | H1TO42 == 997 ~ 0,   # 997 = never tried other illegal drugs
      H1TO42 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    injection_drug_bin = case_when(
      H1TO46 %in% c(1, 2) ~ 1,
      H1TO46 == 0 ~ 0,
      H1TO46 == 7 & H1TO44 == 7 ~ 0,     # never injected illegal drug
      H1TO46 == 7 & H1TO44 %in% c(1, 6) ~ NA_real_,
      is.na(H1TO46) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # Frequency versions
    marijuana_freq = case_when(
      H1TO32 == 0 | H1TO32 == 997 ~ 0,
      H1TO32 %in% c(1, 2) ~ 1,
      H1TO32 >= 3 & H1TO32 <= 10 ~ 2,
      H1TO32 > 10 & H1TO32 < 996 ~ 3,
      H1TO32 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    cocaine_freq = case_when(
      H1TO36 == 0 | H1TO36 == 997 ~ 0,
      H1TO36 %in% c(1, 2) ~ 1,
      H1TO36 >= 3 & H1TO36 <= 10 ~ 2,
      H1TO36 > 10 & H1TO36 < 996 ~ 3,
      H1TO36 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_freq = case_when(
      H1TO42 == 0 | H1TO42 == 997 ~ 0,
      H1TO42 %in% c(1, 2) ~ 1,
      H1TO42 >= 3 & H1TO42 <= 10 ~ 2,
      H1TO42 > 10 & H1TO42 < 996 ~ 3,
      H1TO42 %in% c(996, 998, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    injection_drug_freq = case_when(
      H1TO46 == 0 ~ 0,
      H1TO46 == 1 ~ 1,
      H1TO46 == 2 ~ 2,  # H1 only says 3+ times, so code as 2 conservatively
      H1TO46 == 7 & H1TO44 == 7 ~ 0,
      H1TO46 == 7 & H1TO44 %in% c(1, 6) ~ NA_real_,
      is.na(H1TO46) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # final variables

    substance_count = case_when(
      if_all(c(marijuana_bin, cocaine_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(marijuana_bin, cocaine_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    substance_bin = case_when(
      if_any(c(marijuana_bin, cocaine_bin, other_drug_bin, injection_drug_bin), ~ .x == 1) ~ 1,
      substance_count == 0 & injection_drug_bin == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    substance_frequency = case_when(
      if_all(c(marijuana_freq, cocaine_freq, other_drug_freq, injection_drug_freq), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(marijuana_freq, cocaine_freq, other_drug_freq, injection_drug_freq)),
        na.rm = TRUE
      )
    )
  )

table(w1_data$substance_bin, useNA = "always")
table(w1_data$substance_count, useNA = "always")
table(w1_data$substance_frequency, useNA = "always")

# w2
val_labels(w2_data$H2TO52)
table(w2_data$H2TO52, useNA = "always")

val_labels(w2_data$H2TO60)
table(w2_data$H2TO60, useNA = "always")

val_labels(w2_data$H2TO64)
table(w2_data$H2TO64, useNA = "always")

w2_data <- w2_data %>%
  mutate(

    # Substance-specific binaries
    cocaine_bin = case_when(
      H2TO52 > 0 & H2TO52 < 996 ~ 1,
      H2TO52 == 0 | H2TO52 == 997 ~ 0,
      H2TO52 %in% c(996, 998) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H2TO60 > 0 & H2TO60 < 9996 ~ 1,
      H2TO60 == 0 | H2TO60 == 9997 ~ 0,
      H2TO60 %in% c(9996, 9998) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    injection_drug_bin = case_when(
      H2TO64 %in% c(1, 2, 3) ~ 1,
      H2TO64 == 0 ~ 0,
      H2TO64 == 7 & H2TO62 == 7 ~ 0,
      H2TO64 == 7 & H2TO62 == 1 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # Frequency versions
    cocaine_freq = case_when(
      H2TO52 == 0 | H2TO52 == 997 ~ 0,
      H2TO52 %in% c(1, 2) ~ 1,
      H2TO52 >= 3 & H2TO52 <= 10 ~ 2,
      H2TO52 > 10 & H2TO52 < 996 ~ 3,
      H2TO52 %in% c(996, 998) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_freq = case_when(
      H2TO60 == 0 | H2TO60 == 9997 ~ 0,
      H2TO60 %in% c(1, 2) ~ 1,
      H2TO60 >= 3 & H2TO60 <= 10 ~ 2,
      H2TO60 > 10 & H2TO60 < 9996 ~ 3,
      H2TO60 %in% c(9996, 9998) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    injection_drug_freq = case_when(
      H2TO64 == 0 ~ 0,
      H2TO64 == 1 ~ 1,
      H2TO64 == 2 ~ 2,
      H2TO64 == 3 ~ 3,
      H2TO64 == 7 & H2TO62 == 7 ~ 0,
      H2TO64 == 7 & H2TO62 == 1 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # final variables (binary, count, frequency)
    substance_count = case_when(
      if_all(c(cocaine_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(cocaine_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    substance_bin = case_when(
      if_any(c(cocaine_bin, other_drug_bin, injection_drug_bin), ~ .x == 1) ~ 1,
      substance_count == 0 & injection_drug_bin == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    substance_frequency = case_when(
      if_all(c(cocaine_freq, other_drug_freq, injection_drug_freq), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(cocaine_freq, other_drug_freq, injection_drug_freq)),
        na.rm = TRUE
      )
    )
  )

table(w2_data$substance_bin, useNA = "always")
table(w2_data$substance_count, useNA = "always")
table(w2_data$substance_frequency, useNA = "always")

# w3
w3_data <- w3_data %>%
  mutate(
    marijuana_bin = case_when(
      H3TO110 > 0 & H3TO110 < 9996 ~ 1,
      H3TO110 == 0 ~ 0,
      H3TO110 == 9997 & H3TO108 == 0 ~ 0,
      H3TO110 == 9997 & H3TO109 == 0 ~ 0,
      H3TO108 %in% c(6, 8, 9) | H3TO109 == 9 | H3TO110 %in% c(9996, 9998, 9999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    cocaine_bin = case_when(
      H3TO113 > 0 & H3TO113 < 996 ~ 1,
      H3TO113 == 0 ~ 0,
      H3TO113 == 997 & H3TO111 == 0 ~ 0,
      H3TO113 == 997 & H3TO112 == 0 ~ 0,
      H3TO111 %in% c(6, 8, 9) | H3TO113 %in% c(996, 998) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    meth_bin = case_when(
      H3TO116 > 0 & H3TO116 < 997 ~ 1,
      H3TO116 == 0 ~ 0,
      H3TO116 == 997 & H3TO114 == 0 ~ 0,
      H3TO116 == 997 & H3TO115 == 0 ~ 0,
      H3TO114 %in% c(6, 8, 9) | H3TO115 == 9 | H3TO116 == 998 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H3TO119 > 0 & H3TO119 < 997 ~ 1,
      H3TO119 == 0 ~ 0,
      H3TO119 == 997 & H3TO117 == 0 ~ 0,
      H3TO119 == 997 & H3TO118 == 0 ~ 0,
      H3TO117 %in% c(6, 8, 9) | H3TO119 == 998 ~ NA_real_,
      TRUE ~ NA_real_
    ),

    injection_drug_bin = case_when(
      H3TO122 > 0 & H3TO122 < 997 ~ 1,
      H3TO122 == 0 ~ 0,
      H3TO122 == 997 & H3TO120 == 0 ~ 0,
      H3TO122 == 997 & H3TO121 == 0 ~ 0,
      H3TO120 %in% c(6, 8, 9) | H3TO121 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # frequency
    marijuana_freq = case_when(
      marijuana_bin == 0 ~ 0,
      H3TO110 %in% c(1, 2) ~ 1,
      H3TO110 >= 3 & H3TO110 <= 10 ~ 2,
      H3TO110 > 10 & H3TO110 < 9996 ~ 3,
      TRUE ~ NA_real_
    ),

    cocaine_freq = case_when(
      cocaine_bin == 0 ~ 0,
      H3TO113 %in% c(1, 2) ~ 1,
      H3TO113 >= 3 & H3TO113 <= 10 ~ 2,
      H3TO113 > 10 & H3TO113 < 996 ~ 3,
      TRUE ~ NA_real_
    ),
    meth_freq = case_when(
      meth_bin == 0 ~ 0,
      H3TO116 %in% c(1, 2) ~ 1,
      H3TO116 >= 3 & H3TO116 <= 10 ~ 2,
      H3TO116 > 10 & H3TO116 < 997 ~ 3,
      TRUE ~ NA_real_
    ),

    other_drug_freq = case_when(
      other_drug_bin == 0 ~ 0,
      H3TO119 %in% c(1, 2) ~ 1,
      H3TO119 >= 3 & H3TO119 <= 10 ~ 2,
      H3TO119 > 10 & H3TO119 < 997 ~ 3,
      TRUE ~ NA_real_
    ),

    injection_drug_freq = case_when(
      injection_drug_bin == 0 ~ 0,
      H3TO122 %in% c(1, 2) ~ 1,
      H3TO122 >= 3 & H3TO122 <= 10 ~ 2,
      H3TO122 > 10 & H3TO122 < 997 ~ 3,
      TRUE ~ NA_real_
    ),

    substance_count = case_when(
      if_all(c(marijuana_bin, cocaine_bin, meth_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(marijuana_bin, cocaine_bin, meth_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    substance_bin = case_when(
      if_any(c(marijuana_bin, cocaine_bin, meth_bin, other_drug_bin, injection_drug_bin), ~ .x == 1) ~ 1,
      substance_count == 0 & injection_drug_bin == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    substance_frequency = case_when(
      if_all(c(marijuana_freq, cocaine_freq, meth_freq, other_drug_freq, injection_drug_freq), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(marijuana_freq, cocaine_freq, meth_freq, other_drug_freq, injection_drug_freq)),
        na.rm = TRUE
      )
    )
  )

table(w3_data$substance_bin, useNA = "always")
table(w3_data$substance_count, useNA = "always")
table(w3_data$substance_frequency, useNA = "always")

# w4 (different from wave 1-3)
# H4TO65A-E measure lifetime/ever substance use
# substance_bin = any reported substance use
# substance_count = number of substance types ever used

val_labels(w4_data$H4TO65A)
table(w4_data$H4TO65A, useNA = "always")

val_labels(w4_data$H4TO65B)
table(w4_data$H4TO65B, useNA = "always")

val_labels(w4_data$H4TO65C)
table(w4_data$H4TO65C, useNA = "always")

val_labels(w4_data$H4TO65D)
table(w4_data$H4TO65D, useNA = "always")

val_labels(w4_data$H4TO65E)
table(w4_data$H4TO65E, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    steroid_bin = case_when(
      H4TO65A == 1 ~ 1,
      H4TO65A == 0 ~ 0,
      H4TO65A %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    marijuana_bin = case_when(
      H4TO65B == 1 ~ 1,
      H4TO65B == 0 ~ 0,
      H4TO65B %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    cocaine_bin = case_when(
      H4TO65C == 1 ~ 1,
      H4TO65C == 0 ~ 0,
      H4TO65C %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    meth_bin = case_when(
      H4TO65D == 1 ~ 1,
      H4TO65D == 0 ~ 0,
      H4TO65D %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H4TO65E == 1 ~ 1,
      H4TO65E == 0 ~ 0,
      H4TO65E %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # lifetime count
    substance_count = case_when(
      if_all(c(steroid_bin, marijuana_bin, cocaine_bin, meth_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(steroid_bin, marijuana_bin, cocaine_bin, meth_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    # lifetime binary
    substance_bin = case_when(
      if_any(c(steroid_bin, marijuana_bin, cocaine_bin, meth_bin, other_drug_bin), ~ .x == 1) ~ 1,
      substance_count == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    substance_frequency = NA_real_
  )

table(w4_data$substance_bin, useNA = "always")
table(w4_data$substance_count, useNA = "always")

# w5
# substance_bin = any past 30 day substance use
# substance_count = number of substance types used in past 30 days
# No substance_frequency items available

table(w5_data$H5TO27A, useNA = "always")
table(w5_data$H5TO27B, useNA = "always")
table(w5_data$H5TO27C, useNA = "always")
table(w5_data$H5TO27D, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    cocaine_bin = case_when(
      H5TO27A == 1 ~ 1,
      H5TO27A == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    meth_bin = case_when(
      H5TO27B == 1 ~ 1,
      H5TO27B == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    heroin_bin = case_when(
      H5TO27C == 1 ~ 1,
      H5TO27C == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H5TO27D == 1 ~ 1,
      H5TO27D == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    substance_count = case_when(
      if_all(c(cocaine_bin, meth_bin, heroin_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(cocaine_bin, meth_bin, heroin_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    substance_bin = case_when(
      substance_count > 0 ~ 1,
      substance_count == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    substance_frequency = NA_real_
  )

table(w5_data$substance_bin, useNA = "always")
table(w5_data$substance_count, useNA = "always")

# w6
# substance_bin = any past 30 day substance use
# substance_count = number of substance types used in past 30 days
# No substance_frequency items available

table(w6_data$H6TO44A, useNA = "always")
table(w6_data$H6TO44B, useNA = "always")
table(w6_data$H6TO44C, useNA = "always")
table(w6_data$H6TO44D, useNA = "always")
table(w6_data$H6TO44E, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    cocaine_bin = case_when(
      H6TO44A == 1 ~ 1,
      H6TO44A == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    meth_bin = case_when(
      H6TO44B == 1 ~ 1,
      H6TO44B == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    heroin_bin = case_when(
      H6TO44C == 1 ~ 1,
      H6TO44C == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    fentanyl_bin = case_when(
      H6TO44D == 1 ~ 1,
      H6TO44D == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    other_drug_bin = case_when(
      H6TO44E == 1 ~ 1,
      H6TO44E == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    substance_count = case_when(
      if_all(c(cocaine_bin, meth_bin, heroin_bin, fentanyl_bin, other_drug_bin), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(cocaine_bin, meth_bin, heroin_bin, fentanyl_bin, other_drug_bin)),
        na.rm = TRUE
      )
    ),

    substance_bin = case_when(
      substance_count > 0 ~ 1,
      substance_count == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    substance_frequency = NA_real_
  )

table(w6_data$substance_bin, useNA = "always")
table(w6_data$substance_count, useNA = "always")

# ----------------------------
# DISADVANTAGE
# HOUSEHOLD INCOME
# ----------------------------
# w1 parents questionnaire for household income
val_labels(w1_data$PA55)
table(w1_data$PA55, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    household_income = case_when(
      PA55 >= 0 & PA55 <= 999 ~ PA55 * 1000, # values are reported in thousands of dollars
      PA55 == 9996 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w1_data$household_income, useNA = "always")
summary(w1_data$household_income)

# w2
w2_data <- w2_data %>%
  mutate(household_income = NA_real_)

# w3
# household_income_raw = exact dollar values only
# household_income_all = exact dollar values + bracket midpoints from best guesses

table(w3_data$H3EC6, useNA="always") # exact household income: parent-home path
table(w3_data$H3EC7, useNA="always") # best guess bracket: parent-home path

table(w3_data$H3EC8, useNA="always") # exact household income: married/cohabiting path
table(w3_data$H3EC9, useNA="always") # best guess bracket: married/cohabiting path

# Recode bracketed best guesses into midpoints
w3_data <- w3_data %>%
  mutate(
    h3ec7_midpoint = case_when(
      H3EC7 == 1 ~ 5000,     # less than $10,000
      H3EC7 == 2 ~ 12500,    # $10,000-$14,999
      H3EC7 == 3 ~ 17500,    # $15,000-$19,999
      H3EC7 == 4 ~ 25000,    # $20,000-$29,999
      H3EC7 == 5 ~ 35000,    # $30,000-$39,999
      H3EC7 == 6 ~ 45000,    # $40,000-$49,999
      H3EC7 == 7 ~ 62500,    # $50,000-$74,999
      H3EC7 == 8 ~ 87500,    # $75,000 or more
      H3EC7 %in% c(96, 97, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    h3ec9_midpoint = case_when(
      H3EC9 == 1 ~ 5000,     # less than $10,000
      H3EC9 == 2 ~ 12500,    # $10,000-$14,999
      H3EC9 == 3 ~ 17500,    # $15,000-$19,999
      H3EC9 == 4 ~ 25000,    # $20,000-$29,999
      H3EC9 == 5 ~ 35000,    # $30,000-$39,999
      H3EC9 == 6 ~ 45000,    # $40,000-$49,999
      H3EC9 == 7 ~ 62500,    # $50,000-$74,999
      H3EC9 == 8 ~ 87500,    # $75,000 or more
      H3EC9 %in% c(96, 97, 98, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# create exact income value of H3EC6 and H3EC8
w3_data <- w3_data %>%
  mutate(
    h3ec6_exact = case_when(
      H3EC6 >= 0 & H3EC6 < 999996 ~ H3EC6,
      H3EC6 %in% c(999996, 999997, 999998, 999999) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    h3ec8_exact = case_when(
      H3EC8 >= 0 & H3EC8 < 999996 ~ H3EC8,
      H3EC8 %in% c(999996, 999997, 999998, 999999) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# create two household-income variables
w3_data <- w3_data %>%
  mutate(
    # exact reported household income only
    household_income_raw = case_when(
      !is.na(h3ec6_exact) ~ h3ec6_exact,
      !is.na(h3ec8_exact) ~ h3ec8_exact,
      TRUE ~ NA_real_
    ),

    # exact values first, then bracketed best-guess midpoints
    household_income = case_when(
      !is.na(h3ec6_exact) ~ h3ec6_exact,
      !is.na(h3ec8_exact) ~ h3ec8_exact,
      !is.na(h3ec7_midpoint) ~ h3ec7_midpoint,
      !is.na(h3ec9_midpoint) ~ h3ec9_midpoint,
      TRUE ~ NA_real_
    )
  )

# final checks
summary(w3_data$household_income_raw)
summary(w3_data$household_income)

# w4
val_labels(w4_data$H4EC1)
table(w4_data$H4EC1, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    household_income = case_when(
      H4EC1 == 1  ~ 2500,      # less than $5,000
      H4EC1 == 2  ~ 7500,      # $5,000-$9,999
      H4EC1 == 3  ~ 12500,     # $10,000-$14,999
      H4EC1 == 4  ~ 17500,     # $15,000-$19,999
      H4EC1 == 5  ~ 22500,     # $20,000-$24,999
      H4EC1 == 6  ~ 27500,     # $25,000-$29,999
      H4EC1 == 7  ~ 35000,     # $30,000-$39,999
      H4EC1 == 8  ~ 45000,     # $40,000-$49,999
      H4EC1 == 9  ~ 62500,     # $50,000-$74,999
      H4EC1 == 10 ~ 87500,     # $75,000-$99,999
      H4EC1 == 11 ~ 125000,    # $100,000-$149,999
      H4EC1 == 12 ~ 175000,    # $150,000 or more
      H4EC1 %in% c(96, 98) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w4_data$household_income, useNA = "always")
summary(w4_data$household_income)

# w5
table(w5_data$H5EC2, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    household_income = case_when(
      H5EC2 == 1  ~ 2500,      # less than $5,000
      H5EC2 == 2  ~ 7500,      # $5,000-$9,999
      H5EC2 == 3  ~ 12500,     # $10,000-$14,999
      H5EC2 == 4  ~ 17500,     # $15,000-$19,999
      H5EC2 == 5  ~ 22500,     # $20,000-$24,999
      H5EC2 == 6  ~ 27500,     # $25,000-$29,999
      H5EC2 == 7  ~ 35000,     # $30,000-$39,999
      H5EC2 == 8  ~ 45000,     # $40,000-$49,999
      H5EC2 == 9  ~ 62500,     # $50,000-$74,999
      H5EC2 == 10 ~ 87500,     # $75,000-$99,999
      H5EC2 == 11 ~ 125000,    # $100,000-$149,999
      H5EC2 == 12 ~ 175000,    # $150,000-$199,999
      H5EC2 == 13 ~ 225000,    # $200,000 or more

      # legitimate skips reflect survey routing, not zero household income
      # thus legitimate skip and don't know are coded as NA
      H5EC2 %in% c(997, 998) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w5_data$household_income, useNA = "always")
summary(w5_data$household_income)

# w6
table(w6_data$H6EC3, useNA = "always")

w6_data <- w6_data %>%
  mutate(
    household_income = case_when(
      H6EC3 == 1  ~ 2500,      # less than $5,000
      H6EC3 == 2  ~ 7500,      # $5,000-$9,999
      H6EC3 == 3  ~ 12500,     # $10,000-$14,999
      H6EC3 == 4  ~ 17500,     # $15,000-$19,999
      H6EC3 == 5  ~ 22500,     # $20,000-$24,999
      H6EC3 == 6  ~ 27500,     # $25,000-$29,999
      H6EC3 == 7  ~ 35000,     # $30,000-$39,999
      H6EC3 == 8  ~ 45000,     # $40,000-$49,999
      H6EC3 == 9  ~ 62500,     # $50,000-$74,999
      H6EC3 == 10 ~ 87500,     # $75,000-$99,999
      H6EC3 == 11 ~ 125000,    # $100,000-$149,999
      H6EC3 == 12 ~ 175000,    # $150,000-$199,999
      H6EC3 == 13 ~ 225000,    # $200,000 or more

      # legitimate skips reflect survey routing, not zero household income
      # thus legitimate skip and don't know are coded as NA
      H6EC3 %in% c(-9997, -9998) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(w6_data$household_income, useNA = "always")
summary(w6_data$household_income)
