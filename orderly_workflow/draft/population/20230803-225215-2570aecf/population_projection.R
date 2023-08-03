#' Population projections
#'
#' @param population Population data
#' @param iso iso3c code
#' @param reference_year reference year to rescale against 
#' @param un_pop_address address of UN population data
population_projection <- function(population,
                                  iso,
                                  reference_year,
                                  un_pop_address = "data/un_pop.csv"){
  # UN data
  un <- read.csv(un_pop_address) |>
    filter(iso3c == iso) |>
    rename(un_pop = pop)
  # UN urban rural rates of increase compared to reference year
  ur_rate <- un |>
    mutate(rural = rural / rural[year == reference_year],
           urban = urban / urban[year == reference_year]) |>
    filter(year > reference_year) |>
    select(-un_pop, -iso3c) |>
    pivot_longer(cols = -c(year), names_to = "urban_rural", values_to = "rate")
  
  # Scale up future urban and rural populations
  pd <- population |>
    select(ID, urban_rural, year, pop, par, par_pf, par_pv) |>
    group_by(ID, urban_rural) |>
    # Add in future years
    complete(year = min(year):2050) |>
    # Fill future years with reference year pop
    mutate(pop = ifelse(is.na(pop), pop[year == reference_year], pop),
           par = ifelse(is.na(par), par[year == reference_year], par),
           par_pf = ifelse(is.na(par_pf), par_pf[year == reference_year], par_pf),
           par_pv = ifelse(is.na(par_pv), par_pv[year == reference_year], par_pv)) |>
    ungroup() |>
    # Multiple future years by relative growth rate
    left_join(ur_rate, by = c("urban_rural", "year")) |>
    mutate(pop = ifelse(is.na(rate), pop, pop * rate),
           par = ifelse(is.na(rate), par, par * rate),
           par_pf = ifelse(is.na(rate), par_pf, par_pf * rate),
           par_pv = ifelse(is.na(rate), par_pv, par_pv * rate))
  
  # Scale total by UN
  scaler <- pd |>
    group_by(year) |>
    summarise(pop = sum(pop)) |>
    ungroup() |>
    left_join(select(un, year, un_pop), by = "year") |>
    mutate(scaler = un_pop / pop) |>
    select(year, scaler)
  
  pop_out <- pd |>
    left_join(scaler, by = "year") |>
    mutate(pop = round(pop * scaler),
           par = round(par * scaler),
           par_pf = round(par_pf * scaler),
           par_pv = round(par_pv * scaler)) |>
    select(-rate, -scaler)
  
  return(pop_out)
}
