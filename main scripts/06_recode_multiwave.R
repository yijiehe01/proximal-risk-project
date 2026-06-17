# add purchase power parities (PPP) to standardize household income
# source: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?locations=US

ppp <- read.csv(here("data", "API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_396610.csv"),
                skip = 4)

ppp <- ppp %>%
  filter(Country.Name == "United States") %>%
  select(X1990:X2024) %>%
  pivot_longer(
    cols = X1990:X2024,
    names_to = "year",
    values_to = "ppp"
  ) %>%
  mutate(
    year = as.numeric(sub("^X", "", year))
  ) %>%
  add_row(
    year = 2025,
    ppp = 89991 # source: https://www.worldometers.info/gdp/gdp-by-country/?metric=ppp&region=worldwide&source=imf&year=2025
  )

addhealth_long <- addhealth_long %>%
  left_join(ppp, by = "year")






# fill NAs race, mother_criminal, father_criminal, imprisonment, education, household_size, household_income