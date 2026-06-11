# ----------------------------
# NON-IPV VIOLENCE/OFFENDING
# violence_bin = any non-IPV violence/offending in past year
# violence_frequency = summed 0-3 frequency score

# Frequency coding:
# 0 = never
# 1 = low frequency: once or 1-2 times
# 2 = Medium frequency: more than once / 3-10 times / 3-4 times
# 3 = high frequency: 5+ times / >10 times
# ----------------------------

# w1
val_labels(w1_data$H1FV7) # Pulled a knife or gun on someone
table(w1_data$H1FV7, useNA = "always")

val_labels(w1_data$H1FV8) # Shot or stabbed someone
table(w1_data$H1FV8, useNA = "always")

val_labels(w1_data$H1FV13) # Physical fight where respondent was injured
table(w1_data$H1FV13, useNA = "always")

val_labels(w1_data$H1DS5) # Physical fight in past year
table(w1_data$H1DS5, useNA = "always")

val_labels(w1_data$H1DS6) # Hurt someone badly enough to need bandages/doctor
table(w1_data$H1DS6, useNA = "always")

val_labels(w1_data$H1DS14) # Group fight
table(w1_data$H1DS14, useNA = "always")

w1_data <- w1_data %>%
  mutate(
    knife_gun_freq = case_when(
      H1FV7 == 0 ~ 0,
      H1FV7 == 1 ~ 1,
      H1FV7 == 2 ~ 2,                  # more than once; cannot distinguish further
      H1FV7 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    shot_stab_freq = case_when(
      H1FV8 == 0 ~ 0,
      H1FV8 == 1 ~ 1,
      H1FV8 == 2 ~ 2,                  # more than once; conservative coding
      H1FV8 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    fight_injured_freq = case_when(
      H1FV13 == 0 | H1FV13 == 997 ~ 0,  # legitimate skip
      H1FV13 %in% c(1, 2) ~ 1,
      H1FV13 >= 3 & H1FV13 <= 10 ~ 2,
      H1FV13 > 10 & H1FV13 < 996 ~ 3,
      H1FV13 %in% c(996, 998, 999) | is.na(H1FV13) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    physical_fight_freq = case_when(
      H1DS5 == 0 ~ 0,
      H1DS5 == 1 ~ 1,                  # 1 or 2 times
      H1DS5 == 2 ~ 2,                  # 3 or 4 times
      H1DS5 == 3 ~ 3,                  # 5 or more times
      H1DS5 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    hurt_someone_freq = case_when(
      H1DS6 == 0 ~ 0,
      H1DS6 == 1 ~ 1,                  # 1 or 2 times
      H1DS6 == 2 ~ 2,                  # 3 or 4 times
      H1DS6 == 3 ~ 3,                  # 5 or more times
      H1DS6 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    group_fight_freq = case_when(
      H1DS14 == 0 ~ 0,
      H1DS14 == 1 ~ 1,                 # 1 or 2 times
      H1DS14 == 2 ~ 2,                 # 3 or 4 times
      H1DS14 == 3 ~ 3,                 # 5 or more times
      H1DS14 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    violence_frequency = case_when(
      if_all(
        c(knife_gun_freq, shot_stab_freq, fight_injured_freq,
          physical_fight_freq, hurt_someone_freq, group_fight_freq),
        ~ is.na(.x)
      ) ~ NA_real_,

      TRUE ~ rowSums(
        across(c(knife_gun_freq, shot_stab_freq, fight_injured_freq,
                 physical_fight_freq, hurt_someone_freq, group_fight_freq)),
        na.rm = TRUE
      )
    ),

    violence_bin = case_when(
      violence_frequency > 0 ~ 1,
      violence_frequency == 0 ~ 0,
      is.na(violence_frequency) ~ NA_real_
    )
  )

table(w1_data$violence_bin, useNA = "always")
table(w1_data$violence_frequency, useNA = "always")

# w2

val_labels(w2_data$H2FV6)   # Pulled a knife or gun on someone
table(w2_data$H2FV6, useNA = "always")

val_labels(w2_data$H2FV7)   # Shot or stabbed someone
table(w2_data$H2FV7, useNA = "always")

val_labels(w2_data$H2FV22)  # Hurt someone badly enough to need care
table(w2_data$H2FV22, useNA = "always")

val_labels(w2_data$H2FV17)  # Used a weapon in a fight
table(w2_data$H2FV17, useNA = "always")

val_labels(w2_data$H2FV20)  # Fight where respondent was injured/treated
table(w2_data$H2FV20, useNA = "always")

val_labels(w2_data$H2DS13)  # Group fight
table(w2_data$H2DS13, useNA = "always")

val_labels(w2_data$H2FV16)  # Physical fight
table(w2_data$H2FV16, useNA = "always")


w2_data <- w2_data %>%
  mutate(
    knife_gun_freq = case_when(
      H2FV6 == 0 ~ 0,
      H2FV6 == 1 ~ 1,
      H2FV6 == 2 ~ 2,                  # more than once; cannot distinguish further
      H2FV6 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    shot_stab_freq = case_when(
      H2FV7 == 0 ~ 0,
      H2FV7 == 1 ~ 1,
      H2FV7 == 2 ~ 2,                  # more than once; conservative coding
      H2FV7 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    hurt_someone_freq = case_when(
      H2FV22 == 0 | H2FV22 == 7 ~ 0,   # legitimate skip
      H2FV22 == 1 ~ 1,                 # 1 or 2 times
      H2FV22 == 2 ~ 2,                 # 3 or 4 times
      H2FV22 == 3 ~ 3,                 # 5 or more times
      H2FV22 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    weapon_fight_freq = case_when(
      H2FV17 == 0 | H2FV17 == 7 ~ 0,   # legitimate skip
      H2FV17 == 1 ~ 1,                 # 1 or 2 times
      H2FV17 == 2 ~ 2,                 # 3 or 4 times
      H2FV17 == 3 ~ 3,                 # 5 or more times
      H2FV17 %in% c(6, 8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    fight_injured_freq = case_when(
      H2FV20 == 0 | H2FV20 == 997 ~ 0, # legitimate skip
      H2FV20 %in% c(1, 2) ~ 1,
      H2FV20 >= 3 & H2FV20 <= 10 ~ 2,
      H2FV20 > 10 & H2FV20 < 996 ~ 3,
      H2FV20 %in% c(996, 998)  ~ NA_real_,
      TRUE ~ NA_real_
    ),

    group_fight_freq = case_when(
      H2DS13 == 0 ~ 0,
      H2DS13 == 1 ~ 1,                 # 1 or 2 times
      H2DS13 == 2 ~ 2,                 # 3 or 4 times
      H2DS13 == 3 ~ 3,                 # 5 or more times
      H2DS13 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    physical_fight_freq = case_when(
      H2FV16 == 0 ~ 0,
      H2FV16 == 1 ~ 1,                 # 1 or 2 times
      H2FV16 == 2 ~ 2,                 # 3 or 4 times
      H2FV16 == 3 ~ 3,                 # 5 or more times
      H2FV16 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    violence_frequency = case_when(
      if_all(
        c(knife_gun_freq, shot_stab_freq, hurt_someone_freq,
          weapon_fight_freq, fight_injured_freq, group_fight_freq,
          physical_fight_freq),
        ~ is.na(.x)
      ) ~ NA_real_,

      TRUE ~ rowSums(
        across(c(knife_gun_freq, shot_stab_freq, hurt_someone_freq,
                 weapon_fight_freq, fight_injured_freq, group_fight_freq,
                 physical_fight_freq)),
        na.rm = TRUE
      )
    ),

    violence_bin = case_when(
      violence_frequency > 0 ~ 1,
      violence_frequency == 0 ~ 0,
      is.na(violence_frequency) ~ NA_real_
    )
  )

table(w2_data$violence_bin, useNA = "always")
table(w2_data$violence_frequency, useNA = "always")

# w3
# H3DS18H and H3DS18I are binary items,so included in binary only

val_labels(w3_data$H3DS18H)  # Pulled a knife or gun on someone
table(w3_data$H3DS18H, useNA = "always")

val_labels(w3_data$H3DS18I)  # Shot or stabbed someone
table(w3_data$H3DS18I, useNA = "always")

val_labels(w3_data$H3TO127)  # Physical fight because using drugs
table(w3_data$H3TO127, useNA = "always")

val_labels(w3_data$H3DS17)   # Hurt someone badly enough to need care
table(w3_data$H3DS17, useNA = "always")

val_labels(w3_data$H3DS11)   # Used a weapon in a fight
table(w3_data$H3DS11, useNA = "always")

val_labels(w3_data$H3DS16)   # Fight where respondent was injured/treated
table(w3_data$H3DS16, useNA = "always")

val_labels(w3_data$H3DS7)    # Group fight
table(w3_data$H3DS7, useNA = "always")

w3_data <- w3_data %>%
  mutate(
    knife_gun_bin = case_when(
      H3DS18H == 1 ~ 1,
      H3DS18H == 0 ~ 0,
      H3DS18H %in% c(6, 8, 9) | is.na(H3DS18H) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    shot_stab_bin = case_when(
      H3DS18I == 1 ~ 1,
      H3DS18I == 0 ~ 0,
      H3DS18I %in% c(6, 8, 9) | is.na(H3DS18I) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    drug_fight_freq = case_when(
      H3TO127 == 0 | H3TO127 == 7 ~ 0,
      H3TO127 %in% c(1, 2) ~ 1,      # once/twice
      H3TO127 == 3 ~ 2,              # 3 or 4 times
      H3TO127 == 4 ~ 3,              # 5 or more times
      H3TO127 %in% c(6, 8, 9) | is.na(H3TO127) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    hurt_someone_freq = case_when(
      H3DS17 == 0 ~ 0,
      H3DS17 %in% c(1, 2) ~ 1,
      H3DS17 >= 3 & H3DS17 <= 10 ~ 2,
      H3DS17 > 10 & H3DS17 < 996 ~ 3,
      H3DS17 %in% c(996, 998, 999) | is.na(H3DS17) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    weapon_fight_freq = case_when(
      H3DS11 == 0 ~ 0,
      H3DS11 == 1 ~ 1,
      H3DS11 == 2 ~ 2,
      H3DS11 == 3 ~ 3,
      H3DS11 %in% c(6, 8, 9) | is.na(H3DS11) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    fight_injured_freq = case_when(
      H3DS16 == 0 ~ 0,
      H3DS16 %in% c(1, 2) ~ 1,
      H3DS16 >= 3 & H3DS16 <= 10 ~ 2,
      H3DS16 > 10 & H3DS16 < 96 ~ 3,
      H3DS16 %in% c(96, 98, 99) | is.na(H3DS16) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    group_fight_freq = case_when(
      H3DS7 == 0 ~ 0,
      H3DS7 == 1 ~ 1,
      H3DS7 == 2 ~ 2,
      H3DS7 == 3 ~ 3,
      H3DS7 %in% c(6, 8, 9) | is.na(H3DS7) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    violence_frequency = case_when(
      if_all(
        c(drug_fight_freq, hurt_someone_freq, weapon_fight_freq,
          fight_injured_freq, group_fight_freq),
        ~ is.na(.x)
      ) ~ NA_real_,

      TRUE ~ rowSums(
        across(c(drug_fight_freq, hurt_someone_freq, weapon_fight_freq,
                 fight_injured_freq, group_fight_freq)),
        na.rm = TRUE
      )
    ),

    violence_bin = case_when(
      if_any(
        c(knife_gun_bin, shot_stab_bin, drug_fight_freq, hurt_someone_freq,
          weapon_fight_freq, fight_injured_freq, group_fight_freq),
        ~ .x > 0
      ) ~ 1,

      violence_frequency == 0 & knife_gun_bin == 0 & shot_stab_bin == 0 ~ 0,

      TRUE ~ NA_real_
    )
  )

table(w3_data$violence_bin, useNA = "always")
table(w3_data$violence_frequency, useNA = "always")

# w4
# H4DS19 and H4DS20 are binary, so included in binary only

val_labels(w4_data$H4DS19)  # Pulled a knife or gun on someone
table(w4_data$H4DS19, useNA = "always")

val_labels(w4_data$H4DS20)  # Shot or stabbed someone
table(w4_data$H4DS20, useNA = "always")

val_labels(w4_data$H4DS12)  # Hurt someone badly enough to need care
table(w4_data$H4DS12, useNA = "always")

val_labels(w4_data$H4DS7)   # Group fight
table(w4_data$H4DS7, useNA = "always")

val_labels(w4_data$H4DS11)  # Physical fight
table(w4_data$H4DS11, useNA = "always")

w4_data <- w4_data %>%
  mutate(
    knife_gun_bin = case_when(
      H4DS19 == 1 ~ 1,
      H4DS19 == 0 ~ 0,
      H4DS19 %in% c(6, 8) | is.na(H4DS19) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    shot_stab_bin = case_when(
      H4DS20 == 1 ~ 1,
      H4DS20 == 0 ~ 0,
      H4DS20 %in% c(6, 8) | is.na(H4DS20) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    hurt_someone_freq = case_when(
      H4DS12 == 0 | H4DS12 == 7 ~ 0,
      H4DS12 == 1 ~ 1,
      H4DS12 == 2 ~ 2,
      H4DS12 == 3 ~ 3,
      TRUE ~ NA_real_
    ),

    group_fight_freq = case_when(
      H4DS7 == 0 ~ 0,
      H4DS7 == 1 ~ 1,
      H4DS7 == 2 ~ 2,
      H4DS7 == 3 ~ 3,
      H4DS7 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    physical_fight_freq = case_when(
      H4DS11 == 0 ~ 0,
      H4DS11 == 1 ~ 1,
      H4DS11 == 2 ~ 2,
      H4DS11 == 3 ~ 3,
      H4DS11 %in% c(6, 8) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    violence_frequency = case_when(
      if_all(c(hurt_someone_freq, group_fight_freq, physical_fight_freq), ~ is.na(.x)) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(hurt_someone_freq, group_fight_freq, physical_fight_freq)),
        na.rm = TRUE
      )
    ),

    violence_bin = case_when(
      if_any(c(knife_gun_bin, shot_stab_bin,
               hurt_someone_freq, group_fight_freq, physical_fight_freq),
             ~ .x > 0) ~ 1,
      violence_frequency == 0 & knife_gun_bin == 0 & shot_stab_bin == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(w4_data$violence_bin, useNA = "always")
table(w4_data$violence_frequency, useNA = "always")

# w5
# both items are binary, so no comparable frequency measure
table(w5_data$H5CJ1D, useNA = "always")
table(w5_data$H5CJ1E, useNA = "always")

w5_data <- w5_data %>%
  mutate(
    knife_gun_bin = case_when(
      H5CJ1E == 1 ~ 1,
      H5CJ1E == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    serious_fight_bin = case_when(
      H5CJ1D == 1 ~ 1,
      H5CJ1D == 0 ~ 0,
      TRUE ~ NA_real_
    ),

    violence_bin = case_when(
      if_any(c(knife_gun_bin, serious_fight_bin), ~ .x == 1) ~ 1,
      if_all(c(knife_gun_bin, serious_fight_bin), ~ .x == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    violence_frequency = NA_real_
  )

table(w5_data$violence_bin, useNA = "always")
table(w5_data$violence_frequency, useNA = "always")

# w6: no comparable variables
w6_data <- w6_data %>%
  mutate(
    violence_bin = NA_real_,
    violence_frequency = NA_real_
  )

# ----------------------------
# IPV VIOLENCE
# ----------------------------
# w1, w2: no IPV variables
w1_data <- w1_data %>%
  mutate(
    ipv_bin = NA_real_,
    ipv_frequency = NA_real_
  )

w2_data <- w2_data %>%
  mutate(
    ipv_bin = NA_real_,
    ipv_frequency = NA_real_
  )

# w3
# IPV items are recorded at the relationship level
# One respondent can have several relationship rows, identified by AID + RRELNO.
# To make W3 comparable with W4-W6 respondent-level measures,
# first code IPV within each relationship, then collapse to one row per AID

val_labels(w3_IPV$H3RD109)  # threatened/pushed/shoved/thrown something
table(w3_IPV$H3RD109, useNA = "always")

val_labels(w3_IPV$H3RD111)  # slapped/hit/kicked partner
table(w3_IPV$H3RD111, useNA = "always")

val_labels(w3_IPV$H3RD113)  # insisted on/made partner have unwanted sex
table(w3_IPV$H3RD113, useNA = "always")

val_labels(w3_IPV$H3RD116)  # partner injured because of fight with respondent
table(w3_IPV$H3RD116, useNA = "always")

# take category midpoints/lower bounds
recode_ipv_w3 <- function(x) {
  case_when(
    x == 0 ~ 0,
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 4,
    x == 4 ~ 8,
    x == 5 ~ 15,
    x == 6 ~ 20,
    x == 7 ~ 0,
    x %in% c(95, 96, 98, 99) ~ NA_real_,
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# take the maximum reported frequency across relationships
max_value <- function(x) {
  if (all(is.na(x))) NA_real_ else base::max(x, na.rm = TRUE)
}

w3_IPV <- w3_IPV %>%
  mutate(

    # Relationship-level IPV frequency scores
    push_rel   = recode_ipv_w3(H3RD109),
    slap_rel   = recode_ipv_w3(H3RD111),
    sex_rel    = recode_ipv_w3(H3RD113),
    injury_rel = recode_ipv_w3(H3RD116),

    # Relationship-level total IPV frequency
    ipv_freq_rel = case_when(
      if_all(c(push_rel, slap_rel, sex_rel, injury_rel), is.na) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(push_rel, slap_rel, sex_rel, injury_rel)),
        na.rm = TRUE
      )
    ),

    # Relationship-level IPV binary
    ipv_bin_rel = case_when(
      ipv_freq_rel > 0 ~ 1,
      ipv_freq_rel == 0 ~ 0,
      is.na(ipv_freq_rel) ~ NA_real_
    )
  )

# collapse relationship-level records to respondent level
# for each IPV, use the maximum value across relationships
# avoid inflating frequency because a respondent has multiple relationships recorded
w3_IPV_resp <- w3_IPV %>%
  group_by(AID) %>%
  summarise(
    n_rel_rows = n(),
    n_rel = n_distinct(RRELNO[!is.na(RRELNO)]),

    push_freq   = max_value(push_rel),
    slap_freq   = max_value(slap_rel),
    sex_freq    = max_value(sex_rel),
    injury_freq = max_value(injury_rel),

    .groups = "drop"
  ) %>%
  mutate(

    # respondent-level IPV frequency
    # sum the four IPV maximum scores
    ipv_frequency = case_when(
      if_all(c(push_freq, slap_freq, sex_freq, injury_freq), is.na) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(push_freq, slap_freq, sex_freq, injury_freq)),
        na.rm = TRUE
      )
    ),

    # respondent-level IPV binary
    ipv_bin = case_when(
      ipv_frequency > 0 ~ 1,
      ipv_frequency == 0 ~ 0,
      is.na(ipv_frequency) ~ NA_real_
    )
  )

# merge respondent-level IPV variables back into W3 main data
w3_data <- w3_data %>%
  left_join(
    w3_IPV_resp %>%
      select(
        AID, n_rel_rows, n_rel,
        push_freq, slap_freq, sex_freq, injury_freq,
        ipv_frequency, ipv_bin
      ),
    by = "AID"
  )

table(w3_IPV$ipv_bin_rel, useNA = "always")
table(w3_IPV$ipv_freq_rel, useNA = "always")

table(w3_data$ipv_bin, useNA = "always")
table(w3_data$ipv_frequency, useNA = "always")
summary(w3_data$ipv_frequency)

# w4
# w4 IPV items are respondent-level
val_labels(w4_data$H4RD22)  # Threatened/pushed/shoved/thrown something at partner
table(w4_data$H4RD22, useNA = "always")

val_labels(w4_data$H4RD23)  # Slapped/hit/kicked partner
table(w4_data$H4RD23, useNA = "always")

val_labels(w4_data$H4RD25)  # Insisted on/made partner have unwanted sexual relations
table(w4_data$H4RD25, useNA = "always")

val_labels(w4_data$H4RD24)  # Partner injured because of fight with respondent
table(w4_data$H4RD24, useNA = "always")

recode_ipv_w4 <- function(x, skip_zero = FALSE) {
  case_when(
    x %in% c(0, 1) ~ 0,
    x == 2 ~ 1,
    x == 3 ~ 2,
    x == 4 ~ 4,
    x == 5 ~ 8,
    x == 6 ~ 15,
    x == 7 ~ 20,
    skip_zero & x == 97 ~ 0,
    x %in% c(96, 97, 98) ~ NA_real_,
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

w4_data <- w4_data %>%
  mutate(
    push_freq   = recode_ipv_w4(H4RD22),
    slap_freq   = recode_ipv_w4(H4RD23),
    sex_freq    = recode_ipv_w4(H4RD25),
    injury_freq = recode_ipv_w4(H4RD24, skip_zero = TRUE),

    ipv_frequency = case_when(
      if_all(c(push_freq, slap_freq, sex_freq, injury_freq), is.na) ~ NA_real_,
      TRUE ~ rowSums(
        across(c(push_freq, slap_freq, sex_freq, injury_freq)),
        na.rm = TRUE
      )
    ),

    ipv_bin = case_when(
      ipv_frequency > 0 ~ 1,
      ipv_frequency == 0 ~ 0,
      is.na(ipv_frequency) ~ NA_real_
    )
  )

table(w4_data$ipv_bin, useNA = "always")
table(w4_data$ipv_frequency, useNA = "always")
summary(w4_data$ipv_frequency)

# w5
table(w5_data$H5TR25, useNA = "always") # Pushed/shoved/threw something at partner
table(w5_data$H5TR26, useNA = "always") # Slapped/hit/kicked partner
table(w5_data$H5TR28, useNA = "always") # Insisted on/made partner have unwanted sexual relations
table(w5_data$H5TR27, useNA = "always") # Partner injured because of fight with respondent

recode_ipv_w5 <- function(x, injury = FALSE) {
  case_when(
    x %in% c(1, 2) ~ 0,      # never / happened before but not in last year
    x == 3 ~ 1,              # once
    x == 4 ~ 2,              # twice
    x == 5 ~ 4,              # 3-5 times
    !injury & x == 6 ~ 6,    # 6 or more times
    x == 97 | is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

w5_data <- w5_data %>%
  mutate(
    push_freq   = recode_ipv_w5(H5TR25),
    slap_freq   = recode_ipv_w5(H5TR26),
    sex_freq    = recode_ipv_w5(H5TR28),
    injury_freq = recode_ipv_w5(H5TR27, injury = TRUE),

    ipv_frequency = case_when(
      if_all(c(push_freq, slap_freq, sex_freq, injury_freq), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(c(push_freq, slap_freq, sex_freq, injury_freq)), na.rm = TRUE)
    ),

    ipv_bin = case_when(
      ipv_frequency > 0 ~ 1,
      ipv_frequency == 0 ~ 0,
      is.na(ipv_frequency) ~ NA_real_
    )
  )

table(w5_data$ipv_bin, useNA = "always")
table(w5_data$ipv_frequency, useNA = "always")


# w6
table(w6_data$H6TR22, useNA = "always") # Pushed/shoved/threw something at partner
table(w6_data$H6TR23, useNA = "always") # Slapped/hit/kicked partner
table(w6_data$H6TR25, useNA = "always")  # Insisted on/made partner have unwanted sexual relations
table(w6_data$H6TR24, useNA = "always") # Partner injured because of fight with respondent

recode_ipv_w6 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 0,       # never / happened before but not in last year
    x == 3 ~ 1,               # once
    x == 4 ~ 2,               # twice
    x == 5 ~ 4,               # 3+ or 3-5 times, depending on item
    x == 6 ~ 6,               # 6+ times, only available for one item
    x == -9997 ~ NA_real_,    # legitimate skip, not asked for those who did not meet relationship eligibility criteria
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

w6_data <- w6_data %>%
  mutate(
    push_freq   = recode_ipv_w6(H6TR22),
    slap_freq   = recode_ipv_w6(H6TR23),
    injury_freq = recode_ipv_w6(H6TR24),
    sex_freq    = recode_ipv_w6(H6TR25),

    ipv_frequency = case_when(
      if_all(c(push_freq, slap_freq, injury_freq, sex_freq), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(c(push_freq, slap_freq, injury_freq, sex_freq)), na.rm = TRUE)
    ),

    ipv_bin = case_when(
      ipv_frequency > 0 ~ 1,
      ipv_frequency == 0 ~ 0,
      is.na(ipv_frequency) ~ NA_real_
    )
  )

table(w6_data$ipv_bin, useNA = "always")
table(w6_data$ipv_frequency, useNA = "always")

