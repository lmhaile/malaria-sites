#' Prepare prevalence component of site file
#' 
#' Reformats PfPR and PVPR rasters
#' Creates population at risk based on limits of transmission in 2000
#' 
#' @param   pop_raster population rasters for country of interest
#' @param   pop_raw    population component of site file (without population at risk)
#' @returns population component of site file


prepare_prevalence<- function(pop_raster){
  
  message('PfPr')
  pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pfpr", full.names = TRUE))
  pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
  
  
  message("PvPr")
  pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pvpr", full.names = TRUE))
  pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")

  message('Population at risk')
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




