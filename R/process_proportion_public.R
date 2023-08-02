# Public_sector

ps_raw <- read.csv("raw_data/dhs/public_sector.csv")

ggplot2::ggplot(ps_raw, ggplot2::aes(x = year, y = public_sector)) + 
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ country)

ps <- ps_raw |>
  dplyr::filter(year >= 2010) |>
  dplyr::mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c")) |>
  dplyr::group_by(iso3c) |>
  dplyr::summarise(prop_public = mean(public_sector)) |>
  ungroup()
  
write.csv(ps, "data/proportion_public.csv")
  
