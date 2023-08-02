# Population

library(dplyr)
library(tidyr)
library(ggplot2)

# UN World popualtion prospects
wpp <- read.csv("raw_data/population/WPP2022_Total_Population.csv")

# Urban rural WUP
urban_raw <- read.csv("raw_data/population/WUP2018-F19-Urban_Population_Annual.csv")
rural_raw <- read.csv("raw_data/population/WUP2018-F20-Rural_Population_Annual.csv")

process_ur <- function(x, name){
  x |> 
    dplyr::filter(country != "Channel Islands") |>
    dplyr::mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c")) |>
    dplyr::select(-country) |>
    tidyr::pivot_longer(cols = -iso3c, names_to = "year", values_to = name, names_prefix = "X") |>
    dplyr::mutate(year = as.integer(year))
}

urban <- process_ur(urban_raw, "urban")
rural <- process_ur(rural_raw, "rural")

# Combine total population with urban/rural and rescale to match
pop <- wpp |>
  dplyr::filter(year >= 1990, year <= 2050,
                iso3c %in% urban$iso3c) |>
  dplyr::left_join(urban, by = c("iso3c", "year")) |>
  dplyr::left_join(rural, by = c("iso3c", "year")) |>
  dplyr::mutate(pop = pop * 1000,
                urban = urban * 1000,
                rural = rural * 1000,
                scaler = (rural + urban) / pop,
                rural = rural * scaler,
                urban = urban * scaler) |>
  dplyr::select(-scaler)

pd <- pop %>%
  dplyr::group_by(year) |>
  dplyr::summarise(urban = sum(urban),
                   rural = sum(rural)) |>
  tidyr::pivot_longer(cols = -year, values_to = "pop", names_to = "type")

ggplot(pd, aes(x = year, y = pop / 1e6, col = type)) +
  geom_line() +
  theme_bw()

write.csv(pop, "data/un_pop.csv", row.names = FALSE)
