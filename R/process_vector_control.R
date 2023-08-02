# IRS
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
source("R/interpolation.R")

target_years <- 2000:2021

# Interpolate any missing data from WMR IRS people protected
wmr_irs <- read.csv("raw_data/wmr/commodities/commodities.csv") |>
  select(iso3c, year, irs_people_protected) |>
  complete(year = target_years, iso3c) |>
  arrange(iso3c, year) |>
  mutate(irs_people_protected = ifelse(year == 2000, 0, irs_people_protected)) |>
  group_by(iso3c) |>
  mutate(irs_interpolated = interpolate_int(irs_people_protected, year)) |>
  ungroup()

irs_interp_plot <- ggplot(wmr_irs, aes(x = year, y = irs_interpolated, col = iso3c)) +
  geom_line() +
  scale_color_discrete(guide = "none") +
  theme_bw()

write.csv(wmr_irs, "data/irs_people_protected.csv", row.names = FALSE)

# Interpolate any missing data from WMR ITN numbers delivered
# Note - these values not used for SSA where we have the full bednet model and spital cube
wmr_itn <- read.csv("raw_data/wmr/commodities/commodities.csv") |>
  select(iso3c, year, llins_sold_or_delivered) |>
  complete(year = target_years, iso3c) |>
  arrange(iso3c, year) |>
  mutate(llins_sold_or_delivered = ifelse(year == 2000, 0, llins_sold_or_delivered)) |>
  group_by(iso3c) |>
  mutate(itn_interpolated = interpolate_int(llins_sold_or_delivered, year)) |>
  ungroup()

itn_interp_plot <- ggplot(wmr_itn, aes(x = year, y = itn_interpolated, col = iso3c)) +
  geom_line() +
  scale_color_discrete(guide = "none") +
  theme_bw()

write.csv(wmr_itn, "data/itn_delivered.csv", row.names = FALSE)

# Visualise
irs_interp_plot | itn_interp_plot
