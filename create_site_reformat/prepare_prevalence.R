


prepare_prevalence<- function(iso3c, gadm){
 
  # Prepare pop
  pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  pattern = paste0("population_", iso3c), full.names = TRUE))
  
  # Pf
  pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pfpr", full.names = TRUE))
  pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
  rm(pfpr_rast)
  
  
  message("PvPr")
  # Pv
  pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pvpr", full.names = TRUE))
  pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")
  rm(pvpr_rast)
  
  
  message("Population at risk")
  # Population at risk raw
  par_raw <- get_par_raw(pfpr_raw, pvpr_raw, pop_raw)
  
  # Prevalence and population at risk
  prevalence <- aggregate_prevalence(pfpr_raw, pvpr_raw, par_raw)
  rm(pvpr_raw)
  rm(pfpr_raw)
  
  prevalence <- prevalence |>
    add_units(gadm_df)
  
  return(prevalence)
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

aggregate_prevalence <- function(pfpr_raw, pvpr_raw, par_raw){
  ap <- pfpr_raw |>
    left_join(pvpr_raw,  c("ID", "pixel", "year")) |>
    left_join(par_raw,  c("ID", "pixel", "year")) |>
    mutate(pvpr = ifelse(pvpr == -1, NA, pvpr)) |>
    group_by(ID, urban_rural, year) |>
    summarise(
      pfpr = ifelse(sum(par_pf) == 0,
                    mean(pfpr, na.rm = TRUE),
                    weighted.mean(pfpr, par_pf, na.rm = TRUE)),
      par_pf = sum(par_pf),
      pvpr = ifelse(sum(par_pv) == 0,
                    mean(pvpr, na.rm = TRUE),
                    weighted.mean(pvpr, par_pv, na.rm = TRUE)),
      par_pv = sum(par_pv),
      par = sum(par)) |>
    ungroup() |>
    replace_na(list(pvpr = 0, pfpr = 0)) |>
    select(ID, urban_rural, year, pfpr, pvpr)
  return(ap)
}


#' Link spatial unit ID to full names
#'
#' @param x dataset with ID index column
#' @param gadm_df spatial units data
add_units <- function(x,
                      gadm_df){
  x <- x |>
    left_join(gadm_df, by = "ID") |>
    select(-ID) |>
    select(country, iso3c, name_1, any_of(c("name_2", "urban_rural", "year")), everything())
  return(x)
}
