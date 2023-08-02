  # SMC

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
source("R/interpolation.R")

target_years <- 2000:2021
assummed_max_cov <- 0.8

# Areas with historical SMC
smc_areas <- read.csv("raw_data/smc/smc_areas.csv")
# Year of SMC initiation
smc_start <- read.csv("raw_data/smc/smc_first_implementation.csv")

# Areas with SMC in access-SMC
smc_early <- smc_areas |>
  filter(year == 2017) |>
  select(-year) |>
  left_join(smc_start, by = "iso3c") |>
  mutate(start = 1) |>
  rename(year = first_smc_year)
# New areas with SMC in SMC alliance
smc_late <- smc_areas |>
  filter(year == 2020) |>
  mutate(start = 1,
         year = 2018)
smc_initial <- bind_rows(smc_early, smc_late) %>%
  mutate(year = year - 1)

smc_final <- smc_areas |>
  mutate(final = 1)

smc <- smc_areas |>
  complete(nesting(iso3c, name_1), year = target_years) |>
  left_join(smc_initial, by = c("iso3c", "name_1", "year")) |>
  left_join(smc_final,  by = c("iso3c", "name_1", "year")) |>
  replace_na(list(start = 0, final = 0)) |>
  mutate(smc_cov = ifelse(start == 1, 0, NA),
         smc_cov = ifelse(final == 1, assummed_max_cov, smc_cov),
         smc_cov = ifelse(year == 2000, 0, smc_cov)) |>
  group_by(iso3c, name_1) |>
  mutate(smc_interpolated = interpolate_int(smc_cov, year))

ggplot() +
  geom_line(data = smc, aes(x = year, y = smc_interpolated, group = paste(iso3c, name_1), col = iso3c)) +
  geom_point(data = smc, aes(x = year, y = smc_cov)) +
  theme_bw()

smc <- smc |>
  select(-c(start, final, smc_cov)) |>
  rename(smc_cov = smc_interpolated)

write.csv(smc, "data/smc_coverage.csv", row.names = FALSE)
