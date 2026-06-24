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











# ----------------------------
# TIME USE
# ----------------------------

# UNSTRUCTURED SPARE TIME OUTSIDE

#w1
# hang_out_usto + absent_usto + skip_usto + night_away_usto + skip2_usto +
# expelled_usto + suspension_usto + run_away_usto
table(w1_data$H1DA7) #hang out past week

w1_data <- w1_data %>%
  mutate(
    hang_out_usto = case_when(
      H1DA7 == 0 ~ 0,   #not at all
      H1DA7 == 1 ~ 1.5, #1 or two times
      H1DA7 == 2 ~ 3.5, #3 or 4 times
      H1DA7 == 3 ~ 6,   #5 or more
      TRUE ~ NA_real_
    ),
    hang_out_usto = hang_out_usto / 7 #past week
  )

table(w1_data$H1ED1) #absent full day past year

w1_data <- w1_data %>%
  mutate(
    absent_usto = case_when(
      H1ED1 == 0 ~ 0,         #never
      H1ED1 == 1 ~ 1.5 * 6.5, #1 or two times
      H1ED1 == 2 ~ 6.5 * 6.5, #3 to 10 times
      H1ED1 == 3 ~ 11 * 6.5,  #more than 10 times
      TRUE ~ NA_real_
    ),
    absent_usto = absent_usto / 180 #school year
  )

table(w1_data$S59G) # skip school past 12 months

w1_data <- w1_data %>%
  mutate(
    skip_usto = case_when(
      S59G == 0 ~ 0,              #never
      S59G == 1 ~ 1.5 * 6.5,      #one or twice
      S59G == 2 ~ 7 * 6.5,        #once a month or less
      S59G == 3 ~ 2.5 * 12 * 6.5, #2 or 3 times a month
      S59G == 4 ~ 1.5 * 52 * 6.5, #once or twice a week
      S59G == 5 ~ 4 * 52 * 6.5,   #3 to 5 days a week
      S59G == 6 ~ 4.5 * 52 * 6.5, #nearly everyday
      TRUE ~ NA_real_
    ),
    skip_usto = skip_usto / 365 #past 12 months
  )

table(w1_data$H1GH53) #spent night away last 12 months

w1_data <- w1_data %>%
  mutate(
    night_away_usto = case_when(
      H1GH53 == 0 ~ 0, #no
      H1GH53 == 1 ~ 8, #yes
      TRUE ~ NA_real_
    ),
    night_away_usto = night_away_usto / 365 #last 12 months
  )

table(w1_data$H1ED2) # skip school school year

w1_data <- w1_data %>%
  mutate(
    skip2_usto = if_else(H1ED2 %in% c(996, 997, 998), NA_real_, H1ED2),
    skip2_usto = skip2_usto * 6.5,
    skip2_usto = skip2_usto / 180 # school year
  )

table(w1_data$H1ED9) #expelled school

w1_data <- w1_data %>%
  mutate(
    expelled_usto = case_when(
      H1ED9 == 0 ~ 0,       #no
      H1ED9 == 1 ~ 7 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    expelled_usto = expelled_usto / 365 #last 12 months
  )

table(w1_data$H1ED7) #out-of-school suspension

w1_data <- w1_data %>%
  mutate(
    suspension_usto = case_when(
      H1ED7 == 0 ~ 0,       #no
      H1ED7 == 1 ~ 1 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    suspension_usto = suspension_usto / 365 #last 12 months
  )

table(w1_data$H1DS7) #run away from home

w1_data <- w1_data %>%
  mutate(
    run_away_usto = case_when(
      H1DS7 == 0 ~ 0,         #never
      H1DS7 == 1 ~ 1.5 * 6.5, #1 or 2 times
      H1DS7 == 2 ~ 3.5 * 6.5, #3 or 4 times
      H1DS7 == 3 ~ 6 * 6.5,   #5 or more times
      TRUE ~ NA_real_
    ),
    run_away_usto = run_away_usto / 365 #last 12 months
  )

table(w1_data$run_away_usto)

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    usto = if_else(
      rowSums(!is.na(select(., hang_out_usto, absent_usto, skip_usto, night_away_usto,
                            skip2_usto, expelled_usto, suspension_usto,
                            run_away_usto))) == 0, NA_real_,
      rowSums(select(.,
               hang_out_usto, absent_usto, skip_usto, night_away_usto, skip2_usto,
               expelled_usto, suspension_usto, run_away_usto), na.rm = TRUE
      )))

#w2
# hang_out_usto + absent_usto + night_away_usto + run_away_usto + skip2_usto +
# expelled_usto + suspension_usto
table(w2_data$H2DA7) #hang out past week

w2_data <- w2_data %>%
  mutate(
    hang_out_usto = case_when(
      H2DA7 == 0 ~ 0,   #not at all
      H2DA7 == 1 ~ 1.5, #1 or two times
      H2DA7 == 2 ~ 3.5, #3 or 4 times
      H2DA7 == 3 ~ 6,   #5 or more
      TRUE ~ NA_real_
    ),
    hang_out_usto = hang_out_usto / 7 #past week
  )

table(w2_data$H2ED1) #absent full day past year

w2_data <- w2_data %>%
  mutate(
    absent_usto = case_when(
      H2ED1 == 0 ~ 0,         #never
      H2ED1 == 1 ~ 1.5 * 6.5, #1 or two times
      H2ED1 == 2 ~ 6.5 * 6.5, #3 to 10 times
      H2ED1 == 3 ~ 11 * 6.5,  #more than 10 times
      TRUE ~ NA_real_
    ),
    absent_usto = absent_usto / 180 #school year
  )

table(w2_data$H2GH46) #spent night away last 12 months

w2_data <- w2_data %>%
  mutate(
    night_away_usto = case_when(
      H2GH46 == 0 ~ 0, #no
      H2GH46 == 1 ~ 8, #yes
      TRUE ~ NA_real_
    ),
    night_away_usto = night_away_usto / 365 #last 12 months
  )

table(w2_data$H2DS5) #run away from home

w2_data <- w2_data %>%
  mutate(
    run_away_usto = case_when(
      H2DS5 == 0 ~ 0,         #never
      H2DS5 == 1 ~ 1.5 * 6.5, #1 or 2 times
      H2DS5 == 2 ~ 3.5 * 6.5, #3 or 4 times
      H2DS5 == 3 ~ 6 * 6.5,   #5 or more times
      TRUE ~ NA_real_
    ),
    run_away_usto = run_away_usto / 365 #last 12 months
  )

table(w2_data$H2ED2) #skip school school year

w2_data <- w2_data %>%
  mutate(
    skip2_usto = if_else(H2ED2 %in% c(996, 997, 998), NA_real_, H2ED2),
    skip2_usto = skip2_usto * 6.5,
    skip2_usto = skip2_usto / 180 # school year
  )

table(w2_data$H2ED5) #expelled school

w2_data <- w2_data %>%
  mutate(
    expelled_usto = case_when(
      H2ED5 == 0 ~ 0,       #no
      H2ED5 == 1 ~ 7 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    expelled_usto = expelled_usto / 365 #last 12 months
  )

table(w2_data$H2ED3) #out-of-school suspension

w2_data <- w2_data %>%
  mutate(
    suspension_usto = case_when(
      H2ED3 == 0 ~ 0,       #no
      H2ED3 == 1 ~ 1 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    suspension_usto = suspension_usto / 365 #last 12 months
  )

table(w2_data$suspension_usto)
class(w2_data$suspension_usto)

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    usto = if_else(
      rowSums(!is.na(select(., hang_out_usto, absent_usto, night_away_usto,
                              run_away_usto, skip2_usto, expelled_usto,
                              suspension_usto))) == 0, NA_real_,
      rowSums(select(.,
                     hang_out_usto, absent_usto, night_away_usto,
                       run_away_usto, skip2_usto, expelled_usto,
                       suspension_usto), na.rm = TRUE
      )))

#w3
# hang_out_usto + expelled_usto + run_away_usto
table(w3_data$H3DA15) #hang out past week

w3_data <- w3_data %>%
  mutate(
    hang_out_usto = case_when(
      H3DA15 == 0 ~ 0,   #not at all
      H3DA15 == 1 ~ 1,   #1 time
      H3DA15 == 2 ~ 2,   #2 times
      H3DA15 == 3 ~ 3,   #3 times
      H3DA15 == 4 ~ 4,   #4 times
      H3DA15 == 5 ~ 5,   #5 times
      H3DA15 == 6 ~ 6,   #6 times
      H3DA15 == 7 ~ 8,   #7 or more
      TRUE ~ NA_real_
    ),
    hang_out_usto = hang_out_usto / 7 #past week
  )

table(w3_data$H3ED33) #expelled school

w3_data <- w3_data %>%
  mutate(
    expelled_usto = case_when(
      H3ED33 == 0 ~ 0,       #no
      H3ED33 == 1 ~ 7 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    expelled_usto = expelled_usto / 365 #last 12 months
  )

table(w3_data$H3HR23) #run away from home

w3_data <- w3_data %>%
  mutate(
    run_away_usto = case_when(
      H3HR23 == 0 ~ 0,       #no
      H3HR23 == 1 ~ 2 * 6.5, #yes
      TRUE ~ NA_real_
    ),
    run_away_usto = run_away_usto / 365 #last 12 months
  )

table(w3_data$run_away_usto)

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    usto = if_else(
      rowSums(!is.na(select(., hang_out_usto, expelled_usto,
                            run_away_usto))) == 0, NA_real_,
      rowSums(select(.,
                     hang_out_usto, expelled_usto,
                     run_away_usto), na.rm = TRUE
      )))

#w4
# no items
w4_data <- w4_data %>%
  mutate(usto = NA_real_)

#w5
# friends_usto + neighbors_usto
table(w5_data$H5SS1) #friends past 12 months

w5_data <- w5_data %>%
  mutate(
    friends_usto = case_when(
      H5SS1 == 1 ~ 0,      #never
      H5SS1 == 2 ~ 0.9,    #less than once a year
      H5SS1 == 3 ~ 1.5,    #once or twice a year
      H5SS1 == 4 ~ 6,      #several times a year
      H5SS1 == 5 ~ 12,     #about once a month
      H5SS1 == 6 ~ 52,     #every week
      H5SS1 == 7 ~ 52 * 2, # several times a week
      TRUE ~ NA_real_
    ),
    friends_usto = friends_usto / 365 #past 12 months
  )

table(w5_data$H5SS2) #neighbors past 12 months

w5_data <- w5_data %>%
  mutate(
    neighbors_usto = case_when(
      H5SS2 == 1 ~ 0.9,    #hardly ever
      H5SS2 == 2 ~ 6,      #several times a year
      H5SS2 == 3 ~ 12 * 2, #several times a month
      H5SS2 == 4 ~ 52 * 2, # several times a week
      H5SS2 == 5 ~ 300,    # daily or almost daily
      TRUE ~ NA_real_
    ),
    neighbors_usto = neighbors_usto / 365 #past 12 months
  )

table(w5_data$neighbors_usto)

w5_data <- w5_data %>% # sum if at least 1 is available
  mutate(
    usto = if_else(
      rowSums(!is.na(select(., friends_usto, neighbors_usto))) == 0, NA_real_,
      rowSums(select(.,
                     friends_usto, neighbors_usto), na.rm = TRUE
      )))

#w6
# friends_usto + neighbors_usto
table(w6_data$H6CC1) #friends past 12 months

w6_data <- w6_data %>%
  mutate(
    friends_usto = case_when(
      H6CC1 == 1 ~ 0,      #never
      H6CC1 == 2 ~ 0.9,    #less than once a year
      H6CC1 == 3 ~ 1.5,    #once or twice a year
      H6CC1 == 4 ~ 6,      #several times a year
      H6CC1 == 5 ~ 12,     #about once a month
      H6CC1 == 6 ~ 52,     #every week
      H6CC1 == 7 ~ 52 * 2, #several times a week
      TRUE ~ NA_real_
    ),
    friends_usto = friends_usto / 365 #past 12 months
  )

table(w6_data$H6CC2) #neighbors past 12 months

w6_data <- w6_data %>%
  mutate(
    neighbors_usto = case_when(
      H6CC2 == 1 ~ 0.9,    #hardly ever
      H6CC2 == 2 ~ 6,      #several times a year
      H6CC2 == 3 ~ 12 * 2, #several times a month
      H6CC2 == 4 ~ 52 * 2, #several times a week
      H6CC2 == 5 ~ 300,    #daily or almost daily
      TRUE ~ NA_real_
    ),
    neighbors_usto = neighbors_usto / 365 #past 12 months
  )

table(w6_data$neighbors_usto)

w6_data <- w6_data %>% # sum if at least 1 is available
  mutate(
    usto = if_else(
      rowSums(!is.na(select(., friends_usto, neighbors_usto))) == 0, NA_real_,
      rowSums(select(.,
                     friends_usto, neighbors_usto), na.rm = TRUE
      )))

# UNSTRUCTURED SPARE TIME AT HOME

#w1
# tv_computer_usth + tv_cassettes_usth + radio_usth + computer_games_usth +
# tv_usth + videos_usth

table(w1_data$H1DA3) #tv, videogames, computer last week

w1_data <- w1_data %>%
  mutate(
    tv_computer_usth = case_when(
      H1DA3 == 0 ~ 0,   #not at all
      H1DA3 == 1 ~ 1.5, #1 or 2 times
      H1DA3 == 2 ~ 3.5, #3 or 4 times
      H1DA3 == 3 ~ 6,   #5 times or more
      TRUE ~ NA_real_
    ),
    tv_computer_usth = tv_computer_usth / 7 #last week
  )

table(w1_data$S47) #tv or cassettes average school day

w1_data <- w1_data %>%
  mutate(
    tv_cassettes_usth = case_when(
      S47 == 0 ~ 0,   #none
      S47 == 1 ~ 0.9, #less 1 hour
      S47 == 2 ~ 1.5, #1 to 2 hours
      S47 == 3 ~ 3.5, #3 to 4 hours
      S47 == 4 ~ 5,   #more than 4 hours
      TRUE ~ NA_real_
    )
  )

table(w1_data$H1DA11) #hours radio week

w1_data <- w1_data %>%
  mutate(
    radio_usth = if_else(H1DA11 %in% c(996, 998), NA_real_, H1DA11),
    radio_usth = radio_usth / 7 #weekly
  )

table(w1_data$H1DA10) #computer games week

w1_data <- w1_data %>%
  mutate(
    computer_games_usth = if_else(H1DA10 %in% c(996, 998), NA_real_, H1DA10),
    computer_games_usth = computer_games_usth / 7 #weekly
  )

table(w1_data$H1DA8) #tv week

w1_data <- w1_data %>%
  mutate(
    tv_usth = if_else(H1DA8 %in% c(996, 998, 999), NA_real_, H1DA8),
    tv_usth = tv_usth / 7 #weekly
  )

table(w1_data$H1DA9) #videos week

w1_data <- w1_data %>%
  mutate(
    videos_usth = if_else(H1DA9 %in% c(996, 998), NA_real_, H1DA9),
    videos_usth = videos_usth / 7 #weekly
  )

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    usth = if_else(
      rowSums(!is.na(select(., tv_computer_usth, tv_cassettes_usth, radio_usth,
                            computer_games_usth, tv_usth, videos_usth))) == 0, NA_real_,
      rowSums(select(., tv_computer_usth, tv_cassettes_usth, radio_usth,
                     computer_games_usth, tv_usth, videos_usth), na.rm = TRUE
      )))

#w2
# tv_computer_usth + radio_usth + computer_games_usth + tv_usth + videos_usth
table(w2_data$H2DA3) #tv, videogames, computer last week

w2_data <- w2_data %>%
  mutate(
    tv_computer_usth = case_when(
      H2DA3 == 0 ~ 0,   #not at all
      H2DA3 == 1 ~ 1.5, #1 or 2 times
      H2DA3 == 2 ~ 3.5, #3 or 4 times
      H2DA3 == 3 ~ 6,   #5 times or more
      TRUE ~ NA_real_
    ),
    tv_computer_usth = tv_computer_usth / 7 #last week
  )

table(w2_data$H2DA11) #hours radio week

w2_data <- w2_data %>%
  mutate(
    radio_usth = if_else(H2DA11 %in% c(998), NA_real_, H2DA11),
    radio_usth = radio_usth / 7 #weekly
  )

table(w2_data$H2DA10) #computer games week

w2_data <- w2_data %>%
  mutate(
    computer_games_usth = if_else(H2DA10 %in% c(98), NA_real_, H2DA10),
    computer_games_usth = computer_games_usth / 7 #weekly
  )

table(w2_data$H2DA8) #tv week

w2_data <- w2_data %>%
  mutate(
    tv_usth = if_else(H2DA8 %in% c(998), NA_real_, H2DA8),
    tv_usth = tv_usth / 7 #weekly
  )

table(w2_data$H2DA9) #videos week

w2_data <- w2_data %>%
  mutate(
    videos_usth = if_else(H2DA9 %in% c(98), NA_real_, H2DA9),
    videos_usth = videos_usth / 7 #weekly
  )

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    usth = if_else(
      rowSums(!is.na(select(., tv_computer_usth, radio_usth, computer_games_usth,
                            tv_usth, videos_usth))) == 0, NA_real_,
      rowSums(select(., tv_computer_usth, radio_usth, computer_games_usth,
                     tv_usth, videos_usth), na.rm = TRUE
      )))

#w3
# tv_computer_usth + computer_games_usth + tv_usth + videos_usth + tv2_usth
table(w3_data$H3DA3) #tv, videogames, computer last week

w3_data <- w3_data %>%
  mutate(
    tv_computer_usth = case_when(
      H3DA3 == 0 ~ 0, #not at all
      H3DA3 == 1 ~ 1, #1 time
      H3DA3 == 2 ~ 2, #2 times
      H3DA3 == 3 ~ 3, #3 times
      H3DA3 == 4 ~ 4, #4 times
      H3DA3 == 5 ~ 5, #5 times
      H3DA3 == 6 ~ 6, #6 times
      H3DA3 == 7 ~ 8, #7 times or more
      TRUE ~ NA_real_
    ),
    tv_computer_usth = tv_computer_usth / 7 #last week
  )

table(w3_data$H3DA5) #computer games week

w3_data <- w3_data %>%
  mutate(
    computer_games_usth = if_else(H3DA5 %in% c(996, 998, 999), NA_real_, H3DA5),
    computer_games_usth = computer_games_usth / 7 #weekly
  )

table(w3_data$H3DA7) #tv week

w3_data <- w3_data %>%
  mutate(
    tv_usth = if_else(H3DA7 %in% c(996, 998, 999), NA_real_, H3DA7),
    tv_usth = tv_usth / 7 #weekly
  )

table(w3_data$H3DA4) #videos week

w3_data <- w3_data %>%
  mutate(
    videos_usth = if_else(H3DA4 %in% c(996, 998, 999), NA_real_, H3DA4),
    videos_usth = videos_usth / 7 #weekly
  )

table(w3_data$H3DA6) #tv last 7 days

w3_data <- w3_data %>%
  mutate(
    tv2_usth = case_when(
      H3DA6 == 0 ~ 0, #not at all
      H3DA6 == 1 ~ 1, #1 time
      H3DA6 == 1 ~ 1, #1 time
      H3DA6 == 2 ~ 2, #2 times
      H3DA6 == 3 ~ 3, #3 times
      H3DA6 == 4 ~ 4, #4 times
      H3DA6 == 5 ~ 5, #5 times
      H3DA6 == 6 ~ 6, #6 times
      H3DA6 == 7 ~ 8, #7 times or more
      TRUE ~ NA_real_
    ),
    tv2_usth = tv2_usth / 7 #past 7 days
  )

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    usth = if_else(
      rowSums(!is.na(select(., tv_computer_usth, computer_games_usth, tv_usth,
                            videos_usth, tv2_usth))) == 0, NA_real_,
      rowSums(select(., tv_computer_usth, computer_games_usth, tv_usth,
                     videos_usth, tv2_usth), na.rm = TRUE
      )))

#w4
# computer_games_usth + internet_usth + tv_usth
table(w4_data$H4DA23) #computer games week

w4_data <- w4_data %>%
  mutate(
    computer_games_usth = if_else(H4DA23 %in% c(996, 997, 998), NA_real_, H4DA23),
    computer_games_usth = computer_games_usth / 7 #weekly
  )

table(w4_data$H4DA22) #internet 7 days

w4_data <- w4_data %>%
  mutate(
    internet_usth = if_else(H4DA22 %in% c(996, 997, 998), NA_real_, H4DA22),
    internet_usth = internet_usth / 7 #past 7 days
  )

table(w4_data$H4DA1) #tv 7 days

w4_data <- w4_data %>%
  mutate(
    tv_usth = if_else(H4DA1 %in% c(996,  998), NA_real_, H4DA1),
    tv_usth = tv_usth / 7 #past 7 days
  )

w4_data <- w4_data %>% # sum if at least 1 is available
  mutate(
    usth = if_else(
      rowSums(!is.na(select(., computer_games_usth, internet_usth, tv_usth))) == 0, NA_real_,
      rowSums(select(., computer_games_usth, internet_usth, tv_usth), na.rm = TRUE
      )))

#w5
# tv_usth
table(w5_data$H5ID23) #tv 7 days

w5_data <- w5_data %>%
  mutate(
    tv_usth = if_else(H5ID23 %in% c(460), NA_real_, H5ID23),
    tv_usth = tv_usth / 7 #past 7 days
  )

w5_data <- w5_data %>% # sum if at least 1 is available
  mutate(
    usth = if_else(
      rowSums(!is.na(select(., tv_usth))) == 0, NA_real_,
      rowSums(select(., tv_usth), na.rm = TRUE
      )))

#w6
w6_data <- w6_data %>%
  mutate(usth = NA_real_)

# STRUCTURED SPARE TIME OUTSIDE

#w1
# clubs_ssto + religious_ssto + religious2_ssto

table(w1_data$S44A1) #French club binary
# ...
table(w1_data$S44A33) #yearbook binary

w1_data <- w1_data %>%
  mutate(
    clubs_ssto = if_else(
      rowSums(!is.na(select(., S44A1:S44A17, S44A30:S44A33))) == 0,
      NA_real_,
      rowSums(
        select(., S44A1:S44A17, S44A30:S44A33) == 1,
        na.rm = TRUE
      )),
    clubs_ssto = clubs_ssto / 7 #at least one activity a week
  )

table(w1_data$H1RE7) #religious youth activities past 12 months
table(w1_data$H1RE1)

w1_data <- w1_data %>%
  mutate(
    H1RE7_recoded = if_else(H1RE1 == 0, 4, H1RE7),
    religious_ssto = case_when(
      H1RE7_recoded == 1 ~ 53, #once a week or more
      H1RE7_recoded == 2 ~ 32, #more than once a month, less than once a week
      H1RE7_recoded == 3 ~ 11, #less than once a month
      H1RE7_recoded == 4 ~ 0,  #never
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # past 12 months
  )

table(w1_data$H1RE3) # religious activities past 12 months
table(w1_data$H1RE1, w1_data$H1RE3)

w1_data <- w1_data %>%
  mutate(
    H1RE3_recoded = if_else(H1RE1 == 0, 4, H1RE3),
    religious2_ssto = case_when(
      H1RE3_recoded == 1 ~ 53, #once a week or more
      H1RE3_recoded == 2 ~ 32, #more than once a month, less than once a week
      H1RE3_recoded == 3 ~ 11, #less than once a month
      H1RE3_recoded == 4 ~ 0,  #never
      TRUE ~ NA_real_
    ),
    religious2_ssto = religious2_ssto / 365 # past 12 months
  )

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., clubs_ssto, religious_ssto, religious2_ssto))) == 0, NA_real_,
      rowSums(select(., clubs_ssto, religious_ssto, religious2_ssto), na.rm = TRUE
      )))

#w2
# religious_ssto + religious2_ssto
table(w2_data$H2RE7) #religious youth activities past 12 months
table(w2_data$H2RE1, w2_data$H2RE7)

w2_data <- w2_data %>%
  mutate(
    H2RE7_recoded = if_else(H2RE1 %in% c(0, 29), 4, H2RE7),
    religious_ssto = case_when(
      H2RE7_recoded == 1 ~ 53, #once a week or more
      H2RE7_recoded == 2 ~ 32, #more than once a month, less than once a week
      H2RE7_recoded == 3 ~ 11, #less than once a month
      H2RE7_recoded == 4 ~ 0,  #never
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # past 12 months
  )

table(w2_data$H2RE3) # religious activities past 12 months
table(w2_data$H2RE1, w2_data$H2RE3)

w2_data <- w2_data %>%
  mutate(
    H2RE3_recoded = if_else(H2RE1 %in% c(0, 29), 4, H2RE3),
    religious2_ssto = case_when(
      H2RE3_recoded == 1 ~ 53, #once a week or more
      H2RE3_recoded == 2 ~ 32, #more than once a month, less than once a week
      H2RE3_recoded == 3 ~ 11, #less than once a month
      H2RE3_recoded == 4 ~ 0,  #never
      TRUE ~ NA_real_
    ),
    religious2_ssto = religious2_ssto / 365 # past 12 months
  )

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., religious_ssto, religious2_ssto))) == 0, NA_real_,
      rowSums(select(., religious_ssto, religious2_ssto), na.rm = TRUE
      )))

#w3
# religious_ssto + religious2_ssto + volunteer_ssto
table(w3_data$H3RE25) #religious youth activities past 12 months
table(w3_data$H3RE1, w3_data$H3RE25)

w3_data <- w3_data %>%
  mutate(
    religious_ssto = case_when(
      H3RE25 == 0 ~ 0,        #never
      H3RE25 == 1 ~ 3,        #a few times
      H3RE25 == 2 ~ 6,        #several times
      H3RE25 == 3 ~ 12,       #once a month
      H3RE25 == 4 ~ 12 * 2.5, #2 or 3 times a month
      H3RE25 == 5 ~ 52,       #once a week
      H3RE25 == 6 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # past 12 months
  )

table(w3_data$H3RE24) # religious activities past 12 months
table(w3_data$H3RE1, w3_data$H3RE24)

w3_data <- w3_data %>%
  mutate(
    religious2_ssto = case_when(
      H3RE24 == 0 ~ 0,        #never
      H3RE24 == 1 ~ 3,        #a few times
      H3RE24 == 2 ~ 6,        #several times
      H3RE24 == 3 ~ 12,       #once a month
      H3RE24 == 4 ~ 12 * 2.5, #2 or 3 times a month
      H3RE24 == 5 ~ 52,       #once a week
      H3RE24 == 6 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious2_ssto = religious2_ssto / 365 # past 12 months
  )

table(w3_data$H3CC3) #volunteer community service 12 months

w3_data <- w3_data %>%
  mutate(
    volunteer_ssto = case_when(
      H3CC3 == 0 ~ 0, #no
      H3CC3 == 1 ~ 1, #yes (assume 1 hour weekly)
      TRUE ~ NA_real_
    ),
    volunteer_ssto = volunteer_ssto / 7 #weekly
  )

table(w3_data$H3CC9A)

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., religious_ssto, religious2_ssto, volunteer_ssto))) == 0, NA_real_,
      rowSums(select(., religious_ssto, religious2_ssto, volunteer_ssto), na.rm = TRUE
      )))

#w4
# religious_ssto + religious2_ssto + volunteer_ssto
table(w4_data$H4RE8) #religious youth activities past 12 months
table(w4_data$H4RE1, w4_data$H4RE8)

w4_data <- w4_data %>%
  mutate(
    religious_ssto = case_when(
      H4RE8 == 0 ~ 0,        #never
      H4RE8 == 1 ~ 3,        #a few times
      H4RE8 == 2 ~ 12,       #once a month
      H4RE8 == 3 ~ 12 * 2.5, #2 or 3 times a month
      H4RE8 == 4 ~ 52,       #once a week
      H4RE8 == 5 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # past 12 months
  )

table(w4_data$H4RE7) # religious activities past 12 months
table(w4_data$H4RE1, w4_data$H4RE7)

w4_data <- w4_data %>%
  mutate(
    religious2_ssto = case_when(
      H4RE7 == 0 ~ 0,        #never
      H4RE7 == 1 ~ 3,        #a few times
      H4RE7 == 2 ~ 12,       #once a month
      H4RE7 == 3 ~ 12 * 2.5, #2 or 3 times a month
      H4RE7 == 4 ~ 52,       #once a week
      H4RE7 == 5 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious2_ssto = religious2_ssto / 365 # past 12 months
  )

table(w4_data$H4DA26) # volunteer community work past 12 months

w4_data <- w4_data %>%
  mutate(
    volunteer_ssto = case_when(
      H4DA26 == 1 ~ 0,     #0 hours
      H4DA26 == 2 ~ 10,    #1 to 19 hours
      H4DA26 == 3 ~ 29.5,  #20 to 39 hours
      H4DA26 == 4 ~ 59.5,  #40 to 79 hours
      H4DA26 == 5 ~ 119.5, #80 to 159 hours
      H4DA26 == 6 ~ 161,   #160 or more hours
      TRUE ~ NA_real_
    ),
    volunteer_ssto = volunteer_ssto / 365 # past 12 months
  )

w4_data <- w4_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., religious_ssto, religious2_ssto, volunteer_ssto))) == 0, NA_real_,
      rowSums(select(., religious_ssto, religious2_ssto, volunteer_ssto), na.rm = TRUE
      )))

#w5
# religious_ssto + volunteer_ssto
table(w5_data$H5RE2) # religious activities past 12 months
table(w5_data$H5RE1, w5_data$H5RE2)

w5_data <- w5_data %>%
  mutate(
    religious_ssto = case_when(
      H5RE2 == 1 ~ 0,        #never
      H5RE2 == 2 ~ 3,        #a few times
      H5RE2 == 3 ~ 12,       #once a month
      H5RE2 == 4 ~ 12 * 2.5, #2 or 3 times a month
      H5RE2 == 5 ~ 52,       #once a week
      H5RE2 == 6 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # daily rate over past 12 months
  )

table(w5_data$H5SS7) # volunteer community work past 12 months

w5_data <- w5_data %>%
  mutate(
    volunteer_ssto = case_when(
      H5SS7 == 1 ~ 0,     #0 hours
      H5SS7 == 2 ~ 10,    #1 to 19 hours
      H5SS7 == 3 ~ 29.5,  #20 to 39 hours
      H5SS7 == 4 ~ 59.5,  #40 to 79 hours
      H5SS7 == 5 ~ 119.5, #80 to 159 hours
      H5SS7 == 6 ~ 161,   #160 or more hours
      TRUE ~ NA_real_
    ),
    volunteer_ssto = volunteer_ssto / 365 # past 12 months
  )

w5_data <- w5_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., religious_ssto, volunteer_ssto))) == 0, NA_real_,
      rowSums(select(., religious_ssto, volunteer_ssto), na.rm = TRUE
      )))

#w6
# religious_ssto + volunteer_ssto
table(w6_data$H6RE2) # religious activities past 12 months
table(w6_data$H6RE1, w6_data$H6RE2)

w6_data <- w6_data %>%
  mutate(
    religious_ssto = case_when(
      H6RE2 == 1 ~ 0,        #never
      H6RE2 == 2 ~ 3,        #a few times
      H6RE2 == 3 ~ 12,       #once a month
      H6RE2 == 4 ~ 12 * 2.5, #2 or 3 times a month
      H6RE2 == 5 ~ 52,       #once a week
      H6RE2 == 6 ~ 52 * 2,   #more than once a week
      TRUE ~ NA_real_
    ),
    religious_ssto = religious_ssto / 365 # daily rate over past 12 months
  )

table(w6_data$H6SS1) # volunteer community work past 12 months

w6_data <- w6_data %>%
  mutate(
    volunteer_ssto = case_when(
      H6SS1 == 1 ~ 0,     #0 hours
      H6SS1 == 2 ~ 10,    #1 to 19 hours
      H6SS1 == 3 ~ 29.5,  #20 to 39 hours
      H6SS1 == 4 ~ 59.5,  #40 to 79 hours
      H6SS1 == 5 ~ 119.5, #80 to 159 hours
      H6SS1 == 6 ~ 161,   #160 or more hours
      TRUE ~ NA_real_
    ),
    volunteer_ssto = volunteer_ssto / 365 # past 12 months
  )

w6_data <- w6_data %>% # sum if at least 1 is available
  mutate(
    ssto = if_else(
      rowSums(!is.na(select(., religious_ssto, volunteer_ssto))) == 0, NA_real_,
      rowSums(select(., religious_ssto, volunteer_ssto), na.rm = TRUE
      )))

# STRUCTURED SPARE TIME AT HOME

#w1
# chores_ssth
table(w1_data$H1DA1) #work around house last week

w1_data <- w1_data %>%
  mutate(
    chores_ssth = case_when(
      H1DA1 == 0 ~ 0,   #not at all
      H1DA1 == 1 ~ 1.5, #1 or 2 times
      H1DA1 == 2 ~ 3.5, #3 or 4 times
      H1DA1 == 3 ~ 6,   #5 times or more
      TRUE ~ NA_real_
    ),
    chores_ssth = chores_ssth / 7 #last week
  )

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    ssth = if_else(
      rowSums(!is.na(select(., chores_ssth))) == 0, NA_real_,
      rowSums(select(., chores_ssth), na.rm = TRUE
      )))

#w2
# chores_ssth +
table(w2_data$H2DA1) #work around house last week

w2_data <- w2_data %>%
  mutate(
    chores_ssth = case_when(
      H2DA1 == 0 ~ 0,   #not at all
      H2DA1 == 1 ~ 1.5, #1 or 2 times
      H2DA1 == 2 ~ 3.5, #3 or 4 times
      H2DA1 == 3 ~ 6,   #5 times or more
      TRUE ~ NA_real_
    ),
    chores_ssth = chores_ssth / 7 #last week
  )

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    ssth = if_else(
      rowSums(!is.na(select(., chores_ssth))) == 0, NA_real_,
      rowSums(select(., chores_ssth), na.rm = TRUE
      )))

#w3
# chores_ssth + religious_home_ssth + care_children_ssth
table(w3_data$H3DA1) #work around house last week

w3_data <- w3_data %>%
  mutate(
    chores_ssth = case_when(
      H3DA1 == 0 ~ 0,  #not at all
      H3DA1 == 1 ~ 1,  #1 time
      H3DA1 == 2 ~ 2,  #2 times
      H3DA1 == 3 ~ 3,  #3 times
      H3DA1 == 4 ~ 4,  #4 times
      H3DA1 == 5 ~ 5,  #5 times
      H3DA1 == 6 ~ 6,  #6 times
      H3DA1 == 7 ~ 8,  #7 times or more
      TRUE ~ NA_real_
    ),
    chores_ssth = chores_ssth / 7 #last week
  )

table(w3_data$H3RE31) #religious activities home average week

w3_data <- w3_data %>%
  mutate(
    religious_home_ssth = if_else(H3RE31 %in% c(96, 98, 99), NA_real_, H3RE31),
    religious_home_ssth = religious_home_ssth / 7 #weekly
  )

table(w3_data$H3DA20) #taking care children per week

w3_data <- w3_data %>%
  mutate(
    H3DA20_recode = if_else(H3DA20 == 997, 0, H3DA20), #no children in household
    care_children_ssth = if_else(H3DA20_recode %in% c(996, 998), NA_real_, H3DA20_recode),
    care_children_ssth = care_children_ssth / 7 #weekly
  )

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    ssth = if_else(
      rowSums(!is.na(select(., chores_ssth, religious_home_ssth, care_children_ssth))) == 0, NA_real_,
      rowSums(select(., chores_ssth, religious_home_ssth, care_children_ssth), na.rm = TRUE
      )))

#w4
w4_data <- w4_data %>%
  mutate(ssth = NA_real_)

#w5
w5_data <- w5_data %>%
  mutate(ssth = NA_real_)

#w6
w6_data <- w6_data %>%
  mutate(ssth = NA_real_)

# COMMITTED TIME

#w1
# sleep_ct + work_ct
table(w1_data$H1GH51) #hours sleep

w1_data <- w1_data %>%
  mutate(
    sleep_ct = if_else(H1GH51 %in% c(96, 98, 99), NA_real_, H1GH51)
  )

table(w1_data$H1EE4) # work non-summer week
table(w1_data$H1EE6) # work summer week

w1_data <- w1_data %>%
  mutate(
    work_nonsummer_ct = if_else(H1EE4 %in% c(996, 998, 999), NA_real_, H1EE4),
    work_nonsummer_ct = work_nonsummer_ct * 40, #non summer weeks
    work_summer_ct = if_else(H1EE6 %in% c(996, 998, 999), NA_real_, H1EE6),
    work_summer_ct = work_summer_ct * 12, #summer weeks
    work_ct = (work_nonsummer_ct + work_summer_ct) / 57,
    work_ct = work_ct / 7 #weekly
  )

table(w1_data$work_ct, useNA = "always")

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., sleep_ct, work_ct))) == 0, NA_real_,
      rowSums(select(., sleep_ct, work_ct), na.rm = TRUE
      )))

#w2
# sleep_ct + work_ct
table(w2_data$H2GH44) #hours sleep

w2_data <- w2_data %>%
  mutate(
    sleep_ct = if_else(H2GH44 %in% c(96, 98), NA_real_, H2GH44)
  )

table(w2_data$H2EE4) # work non-summer week
table(w2_data$H2EE6) # work summer week

w2_data <- w2_data %>%
  mutate(
    work_nonsummer_ct = if_else(H2EE4 %in% c(996, 998, 999), NA_real_, H2EE4),
    work_nonsummer_ct = work_nonsummer_ct * 40, #non summer weeks
    work_summer_ct = if_else(H2EE6 %in% c(996, 998, 999), NA_real_, H2EE6),
    work_summer_ct = work_summer_ct * 12, #summer weeks
    work_ct = (work_nonsummer_ct + work_summer_ct) / 57,
    work_ct = work_ct / 7 #weekly
  )

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., sleep_ct, work_ct))) == 0, NA_real_,
      rowSums(select(., sleep_ct, work_ct), na.rm = TRUE
      )))

#w3
# school_ct + nap_ct + work_ct
table(w3_data$H3DA37) #hours school per week

w3_data <- w3_data %>%
  mutate(
    H3DA37_recode = if_else(H3DA37 == 997, 0, H3DA37), #not enrolled
    school_ct = if_else(H3DA37_recode %in% c(996, 998, 999), NA_real_, H3DA37_recode),
    school_ct = school_ct / 7 #weekly
    )

table(w3_data$H3GH16) # nap number days a week

w3_data <- w3_data %>%
  mutate(
    nap_ct = case_when(
      H3GH16 == 0 ~ 0,  #never
      H3GH16 == 1 ~ 3,  #a few times
      H3GH16 == 2 ~ 5,  #almost every day
      H3GH16 == 3 ~ 7,  #every day
      TRUE ~ NA_real_
    ),
    nap_ct = nap_ct / 7 #number times a week
  )

table(w3_data$H3DA31) #work per week

w3_data <- w3_data %>%
  mutate(H3DA31_recode = if_else(H3DA31 == 997, 0, H3DA31), #no job
         work_ct = if_else(H3DA31_recode %in% c(996, 998, 999), NA_real_, H3DA31_recode),
         work_ct = work_ct / 7 #weekly
         )

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., school_ct, nap_ct, work_ct))) == 0, NA_real_,
      rowSums(select(., school_ct, nap_ct, work_ct), na.rm = TRUE
      )))

#w4
# work_ct
table(w4_data$H4LM19) #work per week

w4_data <- w4_data %>%
  mutate(H4LM19_recode = if_else(H4LM19 == 997, 0, H4LM19), #no job
         work_ct = if_else(H4LM19_recode %in% c(995, 996, 998), NA_real_, H4LM19_recode),
         work_ct = work_ct / 7 #weekly
  )

table(w4_data$work_ct, useNA = "always")

w4_data <- w4_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., work_ct))) == 0, NA_real_,
      rowSums(select(., work_ct), na.rm = TRUE
      )))

#w5
# sleep_ct + work_ct
table(w5_data$H5ID15) #hours sleep

w5_data <- w5_data %>%
  mutate(
    sleep_ct = if_else(H5ID15 %in% c(23), NA_real_, H5ID15)
  )

table(w5_data$H5LM7) #work per week

w5_data <- w5_data %>%
  mutate(
    work_ct = case_when(
      H5LM7 == 5 ~ 7,   #5-9 h
      H5LM7 == 10 ~ 12, #10-14 h
      H5LM7 == 15 ~ 17, #15-19 h
      H5LM7 == 20 ~ 18, #16-20 h (odd overlap)
      H5LM7 == 25 ~ 23, #21-25 h
      H5LM7 == 30 ~ 28, #26-30 h
      H5LM7 == 35 ~ 33, #31-35 h
      H5LM7 == 40 ~ 38, #36-40 h
      H5LM7 == 45 ~ 43, #41-45 h
      H5LM7 == 50 ~ 48, #46-50 h
      H5LM7 == 55 ~ 53, #51-55 h
      H5LM7 == 60 ~ 58, #56-60 h
      H5LM7 == 65 ~ 63, #61-65 h
      H5LM7 == 70 ~ 68, #66-70 h
      H5LM7 == 75 ~ 73, #71-75 h
      H5LM7 == 80 ~ 78, #76-80 h
      H5LM7 == 85 ~ 83, #81-85 h
      H5LM7 == 90 ~ 86, #86 h and beyong
      H5LM7 == 9997 ~ 0,#legitimate skip (no work)
      TRUE ~ NA_real_
    ),
    work_ct = work_ct / 7 #weekly
    )

w5_data <- w5_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., sleep_ct, work_ct))) == 0, NA_real_,
      rowSums(select(., sleep_ct, work_ct), na.rm = TRUE
      )))

#w6
# sleep_ct + work_ct
table(w6_data$H6SP1) #hours sleep

w6_data <- w6_data %>%
  mutate(sleep_ct = H6SP1)

table(w6_data$H6LM4AE, useNA = "always") # total hours jobs week

w6_data <- w6_data %>%
  mutate(
    work_ct = if_else(H6LM4AE %in% c(-9997), 0, H6LM4AE),
    work_ct = work_ct / 7 #weekly
  )

w6_data <- w6_data %>% # sum if at least 1 is available
  mutate(
    ct = if_else(
      rowSums(!is.na(select(., sleep_ct, work_ct))) == 0, NA_real_,
      rowSums(select(., sleep_ct, work_ct), na.rm = TRUE
      )))

# SPORTS TIME

#w1
# clubs_sport + rollerblade_sport + active_sport_sport + exercised_sport +
# weights_sport + exercised2_sport
table(w1_data$S44A18) #baseball
# ...
table(w1_data$S44A33) #other sport

w1_data <- w1_data %>%
  mutate(
    clubs_sport = if_else(
      rowSums(!is.na(select(., S44A18:S44A29))) == 0,
      NA_real_,
      rowSums(
        select(., S44A18:S44A29) == 1,
        na.rm = TRUE
      )),
    clubs_sport = clubs_ssto / 7 #at least one activity a week
  )

table(w1_data$H1DA4) #rollerblade past week

w1_data <- w1_data %>%
  mutate(
    rollerblade_sport = case_when(
      H1DA4 == 0 ~ 0,   #not at all
      H1DA4 == 1 ~ 1.5, #1 or 2 times
      H1DA4 == 2 ~ 3.5, #3 or 4 times
      H1DA4 == 3 ~ 6,   #5 or more times
      TRUE ~ NA_real_
    ),
    rollerblade_sport = rollerblade_sport / 7 #weekly
  )

table(w1_data$H1DA5) #active sport past week

w1_data <- w1_data %>%
  mutate(
    active_sport_sport = case_when(
      H1DA5 == 0 ~ 0,   #not at all
      H1DA5 == 1 ~ 1.5, #1 or 2 times
      H1DA5 == 2 ~ 3.5, #3 or 4 times
      H1DA5 == 3 ~ 6,   #5 or more times
      TRUE ~ NA_real_
    ),
    active_sport_sport = active_sport_sport / 7 #weekly
  )

table(w1_data$active_sport_sport)

table(w1_data$H1GH31B) #exercised past 7 days

w1_data <- w1_data %>%
  mutate(
    exercised_sport = case_when(
      H1GH31B == 7 ~ 0,  #legitimate skip
      H1GH31B == 0 ~ 0,  #not marked
      H1GH31B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised_sport = exercised_sport / 7 #weekly
  )

table(w1_data$H1GH31C) #weights past 7 days

w1_data <- w1_data %>%
  mutate(
    weights_sport = case_when(
      H1GH31C == 7 ~ 0,  #legitimate skip
      H1GH31C == 0 ~ 0,  #not marked
      H1GH31C == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    weights_sport = weights_sport / 7 #weekly
  )

table(w1_data$H1GH30B) #exercised to lose weight past 7 days

w1_data <- w1_data %>%
  mutate(
    exercised2_sport = case_when(
      H1GH30B == 7 ~ 0,  #legitimate skip
      H1GH30B == 0 ~ 0,  #not marked
      H1GH30B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised2_sport = exercised2_sport / 7 #weekly
  )

table(w1_data$exercised2_sport)

w1_data <- w1_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., clubs_sport, rollerblade_sport, active_sport_sport,
                            exercised_sport, weights_sport, exercised2_sport))) == 0,
      NA_real_,
      rowSums(select(., clubs_sport, rollerblade_sport, active_sport_sport,
                     exercised_sport, weights_sport, exercised2_sport), na.rm = TRUE
      )))

#w2
# rollerblade_sport + active_sport_sport + exercised_sport + weights_sport +
# exercised2_sport
table(w2_data$H2DA4) #rollerblade past week

w2_data <- w2_data %>%
  mutate(
    rollerblade_sport = case_when(
      H2DA4 == 0 ~ 0,   #not at all
      H2DA4 == 1 ~ 1.5, #1 or 2 times
      H2DA4 == 2 ~ 3.5, #3 or 4 times
      H2DA4 == 3 ~ 6,   #5 or more times
      TRUE ~ NA_real_
    ),
    rollerblade_sport = rollerblade_sport / 7 #weekly
  )

table(w2_data$H2DA5) #active sport past week

w2_data <- w2_data %>%
  mutate(
    active_sport_sport = case_when(
      H2DA5 == 0 ~ 0,   #not at all
      H2DA5 == 1 ~ 1.5, #1 or 2 times
      H2DA5 == 2 ~ 3.5, #3 or 4 times
      H2DA5 == 3 ~ 6,   #5 or more times
      TRUE ~ NA_real_
    ),
    active_sport_sport = active_sport_sport / 7 #weekly
  )

table(w2_data$H2GH33B) #exercised past 7 days

w2_data <- w2_data %>%
  mutate(
    exercised_sport = case_when(
      H2GH33B == 7 ~ 0,  #legitimate skip
      H2GH33B == 0 ~ 0,  #not marked
      H2GH33B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised_sport = exercised_sport / 7 #weekly
  )

table(w2_data$H2GH33C) #weights past 7 days

w2_data <- w2_data %>%
  mutate(
    weights_sport = case_when(
      H2GH33C == 7 ~ 0,  #legitimate skip
      H2GH33C == 0 ~ 0,  #not marked
      H2GH33C == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    weights_sport = weights_sport / 7 #weekly
  )

table(w2_data$weights_sport)

table(w2_data$H2GH32B) #exercised to lose weight past 7 days

w2_data <- w2_data %>%
  mutate(
    exercised2_sport = case_when(
      H2GH32B == 7 ~ 0,  #legitimate skip
      H2GH32B == 0 ~ 0,  #not marked
      H2GH32B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised2_sport = exercised2_sport / 7 #weekly
  )

table(w2_data$exercised2_sport)

w2_data <- w2_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., rollerblade_sport, active_sport_sport, exercised_sport,
                            weights_sport, exercised2_sport))) == 0,
      NA_real_,
      rowSums(select(., rollerblade_sport, active_sport_sport, exercised_sport,
                     weights_sport, exercised2_sport), na.rm = TRUE
      )))

#w3
# exercised_sport + weights_sport + exercised2_sport + fitness_sport +
# gymnastics_sport + individual_sport_sport + team_sport_sport + other_sport_sport

table(w3_data$H3GH9B) #exercised past 7 days

w3_data <- w3_data %>%
  mutate(
    exercised_sport = case_when(
      H3GH9B == 7 ~ 0,  #legitimate skip
      H3GH9B == 0 ~ 0,  #not marked
      H3GH9B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised_sport = exercised_sport / 7 #weekly
  )

table(w3_data$exercised_sport)

table(w3_data$H3GH9C) #weights past 7 days

w3_data <- w3_data %>%
  mutate(
    weights_sport = case_when(
      H3GH9C == 7 ~ 0,  #legitimate skip
      H3GH9C == 0 ~ 0,  #not marked
      H3GH9C == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    weights_sport = weights_sport / 7 #weekly
  )

table(w3_data$H3GH4B) #exercised to lose weight past 7 days

w3_data <- w3_data %>%
  mutate(
    exercised2_sport = case_when(
      H3GH4B == 7 ~ 0,  #legitimate skip
      H3GH4B == 0 ~ 0,  #not marked
      H3GH4B == 1 ~ 1,  #marked
      TRUE ~ NA_real_
    ),
    exercised2_sport = exercised2_sport / 7 #weekly
  )

table(w3_data$H3GH5) #fitness centre past 7 days (1 to 20 minutes)

w3_data <- w3_data %>%
  mutate(
    fitness_sport = if_else(H3GH5 %in% c(96, 98, 99), NA_real_, H3GH5),
    fitness_sport = fitness_sport * 0.17, #1 to 20 minutes
    fitness_sport = fitness_sport / 7 #weekly
  )

table(w3_data$fitness_sport)

table(w3_data$H3DA12) #gymnastics past 7 days

w3_data <- w3_data %>%
  mutate(
    gymnastics_sport = case_when(
      H3DA12 == 0 ~ 0,  #not at all
      H3DA12 == 1 ~ 1,  #1 time
      H3DA12 == 2 ~ 2,  #2 times
      H3DA12 == 3 ~ 3,  #3 times
      H3DA12 == 4 ~ 4,  #4 times
      H3DA12 == 5 ~ 5,  #5 times
      H3DA12 == 6 ~ 6,  #6 times
      H3DA12 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    gymnastics_sport = gymnastics_sport / 7 #weekly
  )

table(w3_data$H3DA11) #individual sports past 7 days

w3_data <- w3_data %>%
  mutate(
    individual_sport_sport = case_when(
      H3DA11 == 0 ~ 0,  #not at all
      H3DA11 == 1 ~ 1,  #1 time
      H3DA11 == 2 ~ 2,  #2 times
      H3DA11 == 3 ~ 3,  #3 times
      H3DA11 == 4 ~ 4,  #4 times
      H3DA11 == 5 ~ 5,  #5 times
      H3DA11 == 6 ~ 6,  #6 times
      H3DA11 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    individual_sport_sport = individual_sport_sport / 7 #weekly
  )

table(w3_data$H3DA10) #team sports past 7 days

w3_data <- w3_data %>%
  mutate(
    team_sport_sport = case_when(
      H3DA10 == 0 ~ 0,  #not at all
      H3DA10 == 1 ~ 1,  #1 time
      H3DA10 == 2 ~ 2,  #2 times
      H3DA10 == 3 ~ 3,  #3 times
      H3DA10 == 4 ~ 4,  #4 times
      H3DA10 == 5 ~ 5,  #5 times
      H3DA10 == 6 ~ 6,  #6 times
      H3DA10 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    team_sport_sport = team_sport_sport / 7 #weekly
  )

table(w3_data$H3DA13) #other sports past 7 days

w3_data <- w3_data %>%
  mutate(
    other_sport_sport = case_when(
      H3DA13 == 0 ~ 0,  #not at all
      H3DA13 == 1 ~ 1,  #1 time
      H3DA13 == 2 ~ 2,  #2 times
      H3DA13 == 3 ~ 3,  #3 times
      H3DA13 == 4 ~ 4,  #4 times
      H3DA13 == 5 ~ 5,  #5 times
      H3DA13 == 6 ~ 6,  #6 times
      H3DA13 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    other_sport_sport = other_sport_sport / 7 #weekly
  )

w3_data <- w3_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., exercised_sport, weights_sport, exercised2_sport,
                            fitness_sport, gymnastics_sport, individual_sport_sport,
                            team_sport_sport, other_sport_sport))) == 0,
      NA_real_,
      rowSums(select(., exercised_sport, weights_sport, exercised2_sport,
                     fitness_sport, gymnastics_sport, individual_sport_sport,
                     team_sport_sport, other_sport_sport), na.rm = TRUE
      )))

#w4
# gymnastics_sport + individual_sport_sport + team_sport_sport + other_sport_sport +
# fitness_sport
table(w4_data$H4DA6) #gymnastics past 7 days

w4_data <- w4_data %>%
  mutate(
    gymnastics_sport = case_when(
      H4DA6 == 0 ~ 0,  #not at all
      H4DA6 == 1 ~ 1,  #1 time
      H4DA6 == 2 ~ 2,  #2 times
      H4DA6 == 3 ~ 3,  #3 times
      H4DA6 == 4 ~ 4,  #4 times
      H4DA6 == 5 ~ 5,  #5 times
      H4DA6 == 6 ~ 6,  #6 times
      H4DA6 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    gymnastics_sport = gymnastics_sport / 7 #weekly
  )

table(w4_data$gymnastics_sport)

table(w4_data$H4DA5) #individual sports past 7 days

w4_data <- w4_data %>%
  mutate(
    individual_sport_sport = case_when(
      H4DA5 == 0 ~ 0,  #not at all
      H4DA5 == 1 ~ 1,  #1 time
      H4DA5 == 2 ~ 2,  #2 times
      H4DA5 == 3 ~ 3,  #3 times
      H4DA5 == 4 ~ 4,  #4 times
      H4DA5 == 5 ~ 5,  #5 times
      H4DA5 == 6 ~ 6,  #6 times
      H4DA5 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    individual_sport_sport = individual_sport_sport / 7 #weekly
  )

table(w4_data$individual_sport_sport)

table(w4_data$H4DA4) #team sports past 7 days

w4_data <- w4_data %>%
  mutate(
    team_sport_sport = case_when(
      H4DA4 == 0 ~ 0,  #not at all
      H4DA4 == 1 ~ 1,  #1 time
      H4DA4 == 2 ~ 2,  #2 times
      H4DA4 == 3 ~ 3,  #3 times
      H4DA4 == 4 ~ 4,  #4 times
      H4DA4 == 5 ~ 5,  #5 times
      H4DA4 == 6 ~ 6,  #6 times
      H4DA4 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    team_sport_sport = team_sport_sport / 7 #weekly
  )

table(w4_data$H4DA7) #other sports past 7 days

w4_data <- w4_data %>%
  mutate(
    other_sport_sport = case_when(
      H4DA7 == 0 ~ 0,  #not at all
      H4DA7 == 1 ~ 1,  #1 time
      H4DA7 == 2 ~ 2,  #2 times
      H4DA7 == 3 ~ 3,  #3 times
      H4DA7 == 4 ~ 4,  #4 times
      H4DA7 == 5 ~ 5,  #5 times
      H4DA7 == 6 ~ 6,  #6 times
      H4DA7 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    other_sport_sport = other_sport_sport / 7 #weekly
  )

table(w4_data$H4DA13) #fitness center past 7 days

w4_data <- w4_data %>%
  mutate(
    fitness_sport = case_when(
      H4DA13 == 0 ~ 0,  #not at all
      H4DA13 == 1 ~ 1,  #1 time
      H4DA13 == 2 ~ 2,  #2 times
      H4DA13 == 3 ~ 3,  #3 times
      H4DA13 == 4 ~ 4,  #4 times
      H4DA13 == 5 ~ 5,  #5 times
      H4DA13 == 6 ~ 6,  #6 times
      H4DA13 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    fitness_sport = fitness_sport / 7 #weekly
  )

table(w4_data$fitness_sport)

w4_data <- w4_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., gymnastics_sport, individual_sport_sport,
                            team_sport_sport, other_sport_sport,
                            fitness_sport))) == 0,
      NA_real_,
      rowSums(select(., gymnastics_sport, individual_sport_sport,
                     team_sport_sport, other_sport_sport,
                     fitness_sport), na.rm = TRUE
      )))

#w5
# gymnastics_sport + individual_sport_sport + other_sport_sport
table(w5_data$H5ID26) #gymnastics past 7 days

w5_data <- w5_data %>%
  mutate(
    gymnastics_sport = case_when(
      H5ID26 == 0 ~ 0,  #not at all
      H5ID26 == 1 ~ 1,  #1 time
      H5ID26 == 2 ~ 2,  #2 times
      H5ID26 == 3 ~ 3,  #3 times
      H5ID26 == 4 ~ 4,  #4 times
      H5ID26 == 5 ~ 5,  #5 times
      H5ID26 == 6 ~ 6,  #6 times
      H5ID26 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    gymnastics_sport = gymnastics_sport / 7 #weekly
  )

table(w5_data$gymnastics_sport)

table(w5_data$H5ID27) #individual sports past 7 days

w5_data <- w5_data %>%
  mutate(
    individual_sport_sport = case_when(
      H5ID27 == 0 ~ 0,  #not at all
      H5ID27 == 1 ~ 1,  #1 time
      H5ID27 == 2 ~ 2,  #2 times
      H5ID27 == 3 ~ 3,  #3 times
      H5ID27 == 4 ~ 4,  #4 times
      H5ID27 == 5 ~ 5,  #5 times
      H5ID27 == 6 ~ 6,  #6 times
      H5ID27 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    individual_sport_sport = individual_sport_sport / 7 #weekly
  )

table(w5_data$H5ID28) #other sports past 7 days

w5_data <- w5_data %>%
  mutate(
    other_sport_sport = case_when(
      H5ID28 == 0 ~ 0,  #not at all
      H5ID28 == 1 ~ 1,  #1 time
      H5ID28 == 2 ~ 2,  #2 times
      H5ID28 == 3 ~ 3,  #3 times
      H5ID28 == 4 ~ 4,  #4 times
      H5ID28 == 5 ~ 5,  #5 times
      H5ID28 == 6 ~ 6,  #6 times
      H5ID28 == 7 ~ 8,  #7 or more times
      TRUE ~ NA_real_
    ),
    other_sport_sport = other_sport_sport / 7 #weekly
  )

table(w5_data$other_sport_sport)

w5_data <- w5_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., gymnastics_sport, individual_sport_sport, other_sport_sport))) == 0,
      NA_real_,
      rowSums(select(., gymnastics_sport, individual_sport_sport, other_sport_sport), na.rm = TRUE
      )))

#w6
# vigorous_sport + moderate_sport
table(w6_data$H6DA10) #vigorous activity last 7 days

w6_data <- w6_data %>%
  mutate(
    vigorous_sport = H6DA10 / 7 #weekly
  )

table(w6_data$H6DA12) #moderate activity last 7 days

w6_data <- w6_data %>%
  mutate(
    moderate_sport = H6DA12 / 7 #weekly
  )

w6_data <- w6_data %>% # sum if at least 1 is available
  mutate(
    sport = if_else(
      rowSums(!is.na(select(., vigorous_sport, moderate_sport))) == 0,
      NA_real_,
      rowSums(select(., vigorous_sport, moderate_sport), na.rm = TRUE
      )))

hist(w6_data$sport)

#Check density plots
w1_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

w2_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

w3_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

w4_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

w5_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

w6_data %>%
  pivot_longer(c(usto, usth, ssto, ct, sport),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)

#standardize time to max 24 hours
w1_data <- w1_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w1_data$total)
summary(w1_data$total_24)

w2_data <- w2_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w2_data$total)
summary(w2_data$total_24)

w3_data <- w3_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w3_data$total)
summary(w3_data$total_24)

w4_data <- w4_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w4_data$total)
summary(w3_data$total_24)

w5_data <- w5_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w5_data$total)
summary(w5_data$total_24)

w6_data <- w6_data %>%
  mutate(
    total = rowSums(across(c(usto, usth, ssto, ssth, ct, sport)), na.rm = TRUE),
    sst = rowSums(across(c(ssto, ssth)), na.rm = TRUE),
    ust = rowSums(across(c(usto, usth)), na.rm = TRUE),
    n_valid = rowSums(!is.na(across(c(usto, usth, ssto, ssth, ct, sport)))),
    total = if_else(n_valid == 0 | total == 0, NA_real_, total),

    usto_24  = usto / total * 24,
    usth_24  = usth / total * 24,
    ust_24   = ust / total * 24,
    ssto_24  = ssto / total * 24,
    ssth_24  = ssth / total * 24,
    sst_24   = sst / total * 24,
    ct_24    = ct / total * 24,
    sport_24 = sport / total * 24,

    total_24 = rowSums(across(c(usto_24, usth_24, ssto_24, ssth_24, ct_24, sport_24)), na.rm = TRUE),
    total_24 = if_else(is.na(total), NA_real_, total_24)
  )
summary(w6_data$total)
summary(w6_data$total_24)

#Check density plots again
w1_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w1_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

w2_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w2_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

w3_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w3_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

w4_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w4_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

w5_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w5_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

w6_data %>%
  pivot_longer(c(ust_24, sst_24, ct_24, sport_24),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value, colour = variable)) +
  geom_density(linewidth = 1)
summary(w6_data[c("ust_24", "sst_24", "ct_24", "sport_24")])

