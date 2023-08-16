# prevalence  ------------------------------------------------------------------
#setwd('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/src/prevalence/')

orderly2::orderly_parameters(iso = 'NGA')
orderly2::orderly_description('Generate prevalence component of site file')
orderly2::orderly_artefact('prevalence output', 'prevalence.rds')

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm.rds = "gadm.rds"))
orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm_df.rds = "gadm_df.rds"))

gadm<- readRDS('gadm.rds')
gadm_df<- readRDS('gadm_df.rds')

source('add_units.R')
source('aggregate_prevalence.R')
source('get_par_raw.R')
source('long_pixel.R')
# Prepare pop
pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  pattern = paste0("population_", iso), full.names = TRUE))

pop_raw <- long_pixel(pop_raster, pop_raster, gadm, "pop") |>
  create_urban_rural() |>
  missing_pop() |>
  rescale_raw_pop(iso = iso)


message('PfPr')
pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pfpr", full.names = TRUE))
pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
rm(pfpr_rast)


message("PvPr")
pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pvpr", full.names = TRUE))
pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")
rm(pvpr_rast)


message("Population at risk")
par_raw <- get_par_raw(pfpr_raw, pvpr_raw, pop_raw)

message('Prevalence')
prevalence <- aggregate_prevalence(pfpr_raw, pvpr_raw, par_raw)
rm(pvpr_raw)
rm(pfpr_raw)

prevalence <- prevalence |>
  add_units(gadm_df)

saveRDS(prevalence, 'prevalence.rds')
