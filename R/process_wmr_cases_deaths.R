# Process WRM cases and deaths outputs

library(countrycode)
library(dplyr)

wmr_cases_deaths_raw <- read.csv("raw_data/wmr/wmr_annexes/2021/WMR2021_Annex5F.csv")

wmr_cases_deaths <- wmr_cases_deaths_raw |>
  mutate(iso3c = countrycode(country, "country.name", "iso3c"),
         incidence_l = wmr_cases_l / wmr_par,
         incidence = wmr_cases / wmr_par,
         incidence_u = wmr_cases_u / wmr_par,
         mortality_l = wmr_deaths_l / wmr_par,
         mortality = wmr_deaths / wmr_par,
         mortality_u = wmr_deaths_u / wmr_par)

write.csv(wmr_cases_deaths, "data/wmr_cases_deaths.csv", row.names = FALSE)
