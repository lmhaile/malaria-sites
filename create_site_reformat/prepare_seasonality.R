prepare_seasonality<- function(iso3c, gadm){
  
  # Season
  rainfall_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "rainfall", full.names = TRUE))
  rainfall <- get_rainfall(rainfall_rast, gadm)
  seasonality <- rainfall |>
    replace_na(list(rainfall = 0)) |>
    get_season_coefs() |>
    add_units(gadm_df)
  rainfall <- rainfall |>
    add_units(gadm_df)
  
  return(seasonality)
}


mean2 <- function(x, na.rm = TRUE){
  return(mean(x[x>=0], na.rm = na.rm))
}

get_rainfall <- function(rainfall_rast, gadm){
  raw_rain <- terra::extract(rainfall_rast, gadm, fun = mean2) 
  rain <- raw_rain |>
    pivot_longer(cols = -ID, names_to = "date", values_to = "rainfall", names_prefix = "rainfall_") |>
    mutate(date = stringr::str_replace_all(date, "_", "-")) |>
    mutate(day = lubridate::yday(date)) |>
    filter(day < 366)
}

get_season_coefs <- function(rainfall){
  season_coefs <- rainfall |>
    group_by(ID) |>
    summarise(coef = as.data.frame(t(umbrella::fit_fourier(rainfall = rainfall, t = day, floor = 0.001)$coefficients))) |>
    unpack(coef)
  return(season_coefs)
}

