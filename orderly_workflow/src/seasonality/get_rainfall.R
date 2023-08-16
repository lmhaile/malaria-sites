get_rainfall <- function(rainfall_rast, gadm){
  raw_rain <- terra::extract(rainfall_rast, gadm, fun = mean2) 
  rain <- raw_rain |>
    pivot_longer(cols = -ID, names_to = "date", values_to = "rainfall", names_prefix = "rainfall_") |>
    mutate(date = stringr::str_replace_all(date, "_", "-")) |>
    mutate(day = lubridate::yday(date)) |>
    filter(day < 366)
}