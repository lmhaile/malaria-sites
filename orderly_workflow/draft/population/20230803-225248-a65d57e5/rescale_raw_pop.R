
#' Rescale raw population. This should be very close to WorldPop estimates which
#' have already been scaled
#'
#' @param pop_raw Population data
#' @param iso iso3c coutnry code
#' @param un_pop_address address of UN population data
rescale_raw_pop <- function(pop_raw, iso, un_pop_address = "data/un_pop.csv"){
  un <- read.csv(un_pop_address) |>
    filter(iso3c == iso)
  pop_scaler <- pop_raw |>
    group_by(year) |>
    summarise(p = sum(pop)) |>
    ungroup() |>
    left_join(un, by = "year") |>
    mutate(scaler = pop / p) |>
    select(year, scaler)
  
  pop_raw <- pop_raw |>
    left_join(pop_scaler, by = "year") |>
    mutate(pop = round(pop * scaler)) |>
    select(-scaler)
  return(pop_raw)
}