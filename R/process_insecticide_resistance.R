# Insecticide resistance
library(dplyr)
library(countrycode)
library(ggplot2)

pyrethroid_resistance_raw <- readRDS("raw_data/insecticide_resistance/predictions_model12b_posgrowth_2050.rds")

pyrethroid_resistance <- pyrethroid_resistance_raw |>
  mutate(iso3c = countrycode(country_name, "country.name", "iso3c")) |>
  filter(prob == 0.5, 
         year >= 2000) |>
  select(iso3c, admin1_name, year, resistance_qs) |>
  rename(name_1 = admin1_name,
         pyrethroid_resistance = resistance_qs)

ggplot(pyrethroid_resistance, aes(x = year, y = pyrethroid_resistance, group = name_1)) +
  geom_line() +
  facet_wrap(~ iso3c)

write.csv(pyrethroid_resistance, "data/pyrethroid_resistance.csv", row.names = FALSE)

# 
# t1 <- get_gadm("GHA")
# 
# n <- filter(ri_raw, country_name == "Ghana")
# mean(unique(n$admin1_name) %in% t1$name_1)
# setdiff(n$admin1_name, t1$name_1)


# Net efficacy
# rnm ref from https://www.sciencedirect.com/science/article/pii/S2542519621002965?via%3Dihub
process_efficacy <-  function(x, net_type){
  x |>
    dplyr::select(resistance, bioassay_mortality, dn0_med, rn0_med, gamman_med) |>
    dplyr::rename(
      pyrethroid_resistance = resistance,
      dn0 = dn0_med,
      rn0 = rn0_med,
      gamman = gamman_med) |>
    mutate(
      rnm = 0.24,
      net_type = net_type)
}

pyrethroid_only <- read.csv("raw_data/insecticide_resistance/pyrethroid_only_nets.csv") |>
  process_efficacy("pyrethroid_only")
pyrethroid_pbo <- read.csv("raw_data/insecticide_resistance/pyrethroid_pbo_nets.csv") |>
  process_efficacy("pyrethroid_pbo")
pyrethroid_pyrrole <- read.csv("raw_data/insecticide_resistance/pyrethroid_pyrrole_nets.csv") |>
  process_efficacy("pyrethroid_pyrrole")

net_efficacy <- dplyr::bind_rows(pyrethroid_only, pyrethroid_pbo, pyrethroid_pyrrole)
write.csv(net_efficacy, "data/net_efficacy.csv", row.names = FALSE)
