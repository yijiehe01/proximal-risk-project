#install.packages("haven")
#install.packages("tidyverse")
#install.packages("here")
#install.packages("labelled")
library(haven)
library(tidyverse)
library(here)
library(labelled)

# ---------------- Import Wave 1 ----------------
w1_main <- read_sav(
  here("data", "wave1", "w1inhome_dvn.sav")
)

w1_weight <- read_sav(
  here("data", "wave1", "w1weight.sav")
)

# Filter selected variables
w1_selected <- w1_main %>%
  select(
    # ID
    AID,

    # Parent variables
    PA12, PB8, PA55, PA57A, PA21, PA22, PA38, PA10, matches("^PA6_[1-5]$"), # PA6_1:PA6_5

    # SCH variables
    S59G,S47,matches("^S44A([1-9]|[1-2][0-9]|3[0-3])$"), # S44A1:S44A33
    S64,S20,S14,S1,S2,matches("^S6[A-E]$"),S27,

    # Wave 1 variables
    IYEAR,H1FP21_4,H1DA7,H1DA4,H1ED1,H1GH53,H1ED2,H1ED9,H1ED7,H1DS7,H1DA3,H1DA11,H1DA10,
    H1DA8,H1DA9,H1RE3,H1RE7,H1DA5,matches("^H1GH31[A-G]$"),matches("^H1GH30[A-G]$"),H1DA1,
    H1GH51,H1EE4,H1EE6,H1TO42,H1TO36,H1TO46,H1FV7,H1FV8,H1JO26,H1JO21,H1DS11,
    H1DS6,H1FV13,H1DS14,H1DS5,H1CO10,H1RF4,H1RM4,H1EE8,H1EE5,H1EE7,
    H1GI1M,H1GI1Y,BIO_SEX,matches("^H1GI6[A-E]$"),H1RE1,H1RR1,H1GI15,
    matches("^H1HR2[A-T]$"),H1FP24A1,H1FP24A2,H1TO29,H1TO9,H1TO33,H1TO32,H1TO44,IYEAR,
  )

# Merge weight, cluster variables with selected Wave 1 variables
w1_data <- left_join(
  w1_selected,
  w1_weight,
  by = "AID"
)

w1_data <- w1_data %>%
  mutate(wave = 1, .after = AID)


# ---------------- Import Wave 2 ----------------
w2_main <- read_sav(
  here("data", "wave2", "w2inhome_dvn.sav")
)

w2_weight <- read_sav(
  here("data", "wave2", "w2weight.sav")
)

w2_selected <- w2_main %>%
  select(
    aid,

    iyear2,h2da7,h2da4,h2ed1,h2gh46,h2ds5,h2ed2,h2ed5,h2ed3,h2da3,h2da11,h2da10,h2da8,
    h2da9,h2re3,h2re7,h2da5,matches("^h2gh33[a-g]$"),matches("^h2gh32[a-g]$"),
    h2da1,h2gh44,h2ee4,h2ee6,h2to60,h2to62,h2to52,h2to64,h2to61,h2to58,h2to50,
    h2to59,h2fv6,h2fv7,h2fv10,h2jo13,h2ds9,h2fv22,h2fv17,h2fv20,h2ds13,
    h2fv16,h2co12,h2rf4,h2rm4,h2ee8,h2ee5,h2ee7,h2gi1m,h2gi1y,bio_sex2,
    h2re1,h2rr2a,h2gi5,matches("^h2hr2[a-q]$"),h2hr1,h2fp28a1,h2fp28a2,h2fp28a3,
    h2ds14,h2to41,h2to10,h2to48,iyear2
  ) %>%
  rename(AID = aid)

w2_data <- left_join(
  w2_selected,
  w2_weight,
  by = "AID") %>%

  mutate(wave = 2, .after = AID)

names(w2_data) <- toupper(names(w2_data))

# ---------------- Import Wave 3 ----------------
w3_main <- read_sav(
  here("data", "wave3", "w3inhome_dvn.sav")
)

w3_weight <- read_sav(
  here("data", "wave3", "w3weight.sav")
)

# include marriage/relationship records
w3_partner <- read_sav(
  here("data", "wave3", "w3partner_dvn.sav")
)

w3_partnerdetail <- read_sav(
  here("data", "wave3", "w3prtnrDtail_dvn.sav")
)

w3_birth  <- read_sav(
  here("data", "wave3", "w3birth_dvn.sav")
)

# main respondent-level variables
w3_selected <- w3_main %>%
  select(
    aid,

    iyear3,h3da15,h3da40,h3ed33,h3hr23,h3da3,h3da5,h3da7,h3da4,h3da6,h3cc3,h3cc9a,
    h3re24,h3re25,h3da27,matches("^h3gh9[a-e]$"),matches("^h3gh4[a-i]$"),
    h3gh5,h3da12,h3da11,h3da10,h3da13,h3da1,h3re31,h3da20,h3da37,h3gh16,
    h3da31,h3lm16,h3to119,h3to113,h3to116,h3to122,h3to111,h3to114,
    h3to121,h3to118,h3to120,h3to117,h3ds18h,h3ds18i,h3to127,h3ds4,h3ds17,
    h3ds11,h3ds16,h3ds7,h3ec4,h3ec2,h3ec5,h3ec3,h3ec7,h3ec6,h3ec8,h3ec9,h3ec40,h3ec62,
    h3od1m,h3od1y,bio_sex3,matches("^h3od4[a-d]$"),h3cj160,h3cj152,prison3,h3re1,
    h3hr6,h3hr5,h3hr2,h3ds13,h3to104,h3to103,h3to108,h3to109,h3to110,h3to112,h3to115,iyear3
  ) %>%
  rename(AID = aid)

w3_data <- left_join(
  w3_selected,
  w3_weight,
  by = "AID") %>%

  mutate(wave = 3, .after = AID)

names(w3_data) <- toupper(names(w3_data))

# four IPV-related DV
w3_IPV <- w3_partnerdetail %>%
  select( AID,RRELNO,H3RD116,H3RD113,H3RD111,H3RD109)

# ---------------- Import Wave 4 ----------------
w4_main <- read_sav(
  here("data", "wave4", "w4inhome_dvn.sav")
)

w4_weight <- read_sav(
  here("data", "wave4", "w4weight.sav")
)

# include marriage/relationship records
w4_partner <- read_sav(
  here("data", "wave4", "w4partner_dvn.sav")
)

w4_birth  <- read_sav(
  here("data", "wave4", "w4birth_dvn.sav")
)

# main respondent-level variables
w4_selected <- w4_main %>%
  select(
    AID,

    IYEAR4,H4DA23,H4DA22,H4DA1,H4DA26,H4RE7,H4RE8,H4DA6,H4DA5,H4DA4,H4DA7,H4DA13,
    H4LM19,H4LM13,H4TO66,H4TO67,H4TO65A,H4TO65B,H4TO65C,H4TO65D,H4TO65E,H4DS19,
    H4DS20,H4DS4,H4DS12,H4DS7,H4DS11,H4RD24,H4RD25,H4RD23,H4RD22,H4EC2,
    H4EC3,H4EC1,H4EC18,H4EC16,H4OD1M,H4OD1Y,BIO_SEX4,H4WP3,H4WP30,H4WP16,
    H4WP9,H4CJ24M,H4CJ17,PRISON4,H4RE1,H4TR4,H4HR4,H4HR3,H4HR1,H4TR11,IYEAR4
  )

w4_data <- left_join(
  w4_selected,
  w4_weight,
  by = "AID") %>%

  mutate(wave = 4, .after = AID)

# ---------------- Import Wave 5 ----------------
w5_main <- read_xpt(here("data", "wave5", "pwave5.xpt"))
w5_weight <- read_xpt(here("data", "wave5", "p5weight.xpt"))

w5_selected <- w5_main %>%
  select(
    AID,

    IYEAR5,H5SS1,H5SS2,H5ID23,H5SS7,H5RE2,H5ID26,H5ID27,H5ID28,H5ID15,H5LM7,H5TO27A,
    H5TO27B,H5TO27C,H5TO27D,H5CJ1E,H5CJ1D,H5TR27,H5TR28,H5TR26,H5TR25,
    H5EC1,H5EC2,H5OD1M,H5OD1Y,H5OD2A,matches("^H5OD4[A-G]$"),H5WP1,
    H5WP22,H5WP8,H5WP15,H5CJ5,H5CJ8YM,H5RE1,H5OD11,H5TR6,H5TR7,
    H5TR5,H5TR4,H5TR3,H5HR3,H5HR2,H5SE23,matches("^H5PG26[1-6]$"),matches("^H5HR4[A-H]$"),
    IYEAR5
  )

w5_data <- left_join(
  w5_selected,
  w5_weight,
  by = "AID") %>%

  mutate(wave = 5, .after = AID)

# ---------------- Import Wave 6 ----------------
w6_main <- read_sas(here("data", "wave6", "pwave6.sas7bdat"))
w6_weight <- read_sas(here("data", "wave6", "p6weight.sas7bdat"))

w6_selected <- w6_main %>%
  select(
    AID,

    IYEAR6,H6CC1,H6CC2,H6SS1,H6RE2,H6DA10,H6DA11,H6SP1,matches("^H6LM4[A-E]$"),
    H6LM4AE,H6TO44A,H6TO44B,H6TO44C,H6TO44D,H6TO44E,H6TR24,H6TR25,
    H6TR23,H6TR22,H6EC1,H6EC3,matches("^H6EC5[A-P]$"),H6OD1Y,
    H6OD2,matches("^H6OD3[A-H]$"),H6CJ30,H6CJ35MY,H6RE1,H6OD9,H6MH1,
    H6TR6,H6TR3,H6HR1,H6HR2,H6HR3,matches("^H6PG22[A-F]$"),
    matches("^H6PG26[A-F]$"),matches("^H6HR4[A-H]$"),IYEAR6
  )

w6_data <- left_join(
  w6_selected,
  w6_weight,
  by = "AID") %>%

  mutate(wave = 6,.after = AID)

