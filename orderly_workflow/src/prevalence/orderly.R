

orderly2::orderly_shared_resource(long_pixel() = 'long_pixel.R')
orderly2::orderly_artefact('prevalence output', 'prevalence.rds')

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



saveRDS(prevalence, 'prevalence.rds')
