# HH size
library(countrycode)
library(dplyr)

hh_raw <- read.csv("raw_data/population/un_hh_size.csv")
dplyr::glimpse(hh_raw)

hh <- hh_raw |>
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) |>
  filter(!is.na(iso3c)) |>
  group_by(iso3c) |>
  slice(which.max(as.Date(date, '%d/%m/%Y'))) |>
  ungroup() |>
  mutate(hh_size = ifelse(is.na(average_hh_size),
                          median(average_hh_size, na.rm = TRUE),
                          average_hh_size)) |>
  select(iso3c, hh_size)
glimpse(hh)
hist(hh$hh_size)

write.csv(hh, "data/hh_size.csv", row.names = FALSE)
