#' Prepares population portion of site file
#' 
#' Reformats population rasters to data frame, 
#' splits population by urbanicity, then rescales to UN Worldpop populations (which have already been scaled)
#'
#' @param   pop_raster country code for site file of interest
#' @param   urban_density_threshold Threshold density that is classified as urban, for urban/ rural split
#' @returns population component of site file
prepare_population<- function(iso3c,
                              urban_density_threshold){

  message('Population')
  pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  pattern = paste0("population_", iso3c), full.names = TRUE))
  pop_raw <- long_pixel(pop_raster, pop_raster, gadm, "pop") |>
    create_urban_rural() |>
    missing_pop() |>
    rescale_raw_pop(iso = iso3c)
  
  message("PfPr")
  pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pfpr", full.names = TRUE))
  pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
  rm(pfpr_rast)
  
  
  message("PvPr")
  pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pvpr", full.names = TRUE))
  pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")
  rm(pvpr_rast)
  
  message("Population at risk")
  par_raw <- get_par_raw(pfpr_raw, pvpr_raw, pop_raw)
  
  message("Aggregating population")
  population <- aggregate_pop(pop_raw, par_raw)
  rm(pop_raw)
  
  message("Population projection")
  population <- population_projection(population, iso = iso3c, reference_year = wmr_year - 1) |>
    add_units(gadm_df)
  
  return(population)
}




#' Extracts raster data to long form df
#'
#' @param stack Raster or raster stack
#' @param country_pop country population data. Extend and resolution of this will 
#' be matched
#' @param gadm admin boundaries spatial file 
#' @param name name of variable to extract
#' @param start_year Staring year of stack
long_pixel <- function(stack,
                       country_pop,
                       gadm,
                       name,
                       start_year = 2000){
  sitesv <- methods::as(gadm, "SpatVector")
  
  country_stack <- terra::crop(stack, gadm) |>
    resample(country_pop)
  
  raw_values <- terra::extract(x = country_stack, y = sitesv)
  ### Assumes raster stack is ordered temporally
  colnames(raw_values) <- c("ID", paste(start_year:(start_year + ncol(raw_values) - 2)))
  
  raw_values_long <- raw_values |>
    group_by(ID) |>
    mutate(pixel = 1:n()) |>
    pivot_longer(cols = -c(ID, pixel), names_to = "year", values_to = name, names_transform = list(year = as.integer)) |>
    ungroup()
  
  raw_values_long
}


#' create urban/rural split
#'
#' @param pop_raw popualtion data
#' @param urban_density_threshold Threshold density that is classified as urban 
create_urban_rural <- function(pop_raw,
                               urban_density_threshold = 1500){
  pop_raw <- pop_raw |>
    # Areas with no pop are considered rural
    mutate(urban_rural = ifelse(pop < urban_density_threshold | is.na(pop), "rural", "urban"))
  return(pop_raw)
}


#' Unsure complete set of spatial units and years
#'
#' @param pop_raw Population data
missing_pop <- function(pop_raw){
  pop_raw <- pop_raw |>
    complete(nesting(ID, urban_rural, pixel), year) |>
    replace_na(list(pop = 0))
  
  return(pop_raw)
}



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

#' Create Population at risk based on limits of transmission in 2000
#'
#' @param pfpr_raw PfPr
#' @param pvpr_raw PvPr
#' @param pop_raw population data
get_par_raw <- function(pfpr_raw,
                        pvpr_raw,
                        pop_raw){
  combined <- pfpr_raw |>
    left_join(pvpr_raw, by = c("ID", "pixel", "year")) |>
    left_join(pop_raw, by = c("ID", "pixel", "year")) |>
    mutate(pvpr = ifelse(pvpr == -1, NA, pvpr))
  
  spatial_limits <- combined |>
    filter(year == 2000) |>
    mutate(
      pf_limits = ifelse(is.na(pfpr) | pfpr == 0, 0, 1),
      pv_limits = ifelse(is.na(pvpr) | pvpr == 0, 0, 1),
      limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
    ) |>
    select(ID, pixel, pf_limits, pv_limits, limits)
  
  ap <- pop_raw |>
    left_join(spatial_limits, by = c("ID", "pixel")) |>
    mutate(par_pf = pf_limits * pop,
           par_pv = pv_limits * pop,
           par = limits * pop) |>
    select(ID, pixel, year, urban_rural, par, par_pf, par_pv)
  return(ap)
}


aggregate_pop <- function(pop_raw, par_raw){
  pop <- pop_raw |>
    left_join(par_raw, by = c("ID", "pixel", "year", "urban_rural")) |>
    group_by(ID, urban_rural, year) |>
    summarise(pop = round(sum(pop)),
              par = round(sum(par)),
              par_pf = round(sum(par_pf)),
              par_pv = round(sum(par_pv))) |>
    ungroup()
  return(pop)
}


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

