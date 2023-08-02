# ACT use 

act_raw <- read.csv("raw_data/dhs/act_use.csv")

ggplot2::ggplot(act_raw, ggplot2::aes(x = year, y = took_any_act)) + 
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ country)

act <- act_raw |>
  dplyr::rename(prop_act = took_any_act) |>
  dplyr::mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c")) |>
  # Filter out strange (very low) results from 2020 surveys
  filter(year < 2020) |>
  dplyr::select(iso3c, year, prop_act) |>
  dplyr::group_by(iso3c) |>
  tidyr::complete(year = 2000:2020) |>
  # Add in 0 ACt if missing on or before year ACT first reccommended
  dplyr::mutate(prop_act = ifelse(year <= 2006 & is.na(prop_act), 0, prop_act)) |>
  # Linearly interpolate between points, assummed constant after last point
  dplyr::mutate(prop_act = interpolate_int(prop_act, year)) |>
  ungroup()

ggplot2::ggplot(act, ggplot2::aes(x = year, y = prop_act)) + 
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ iso3c)

write.csv(act, "data/proportion_act.csv")

