# population  ------------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate population component of site file')
orderly2::orderly_artefact('population output', 'population.rds')
orderly2::orderly_shared_resource(long_pixel= 'long_pixel.R')
orderly2::orderly_dependency("initialize_site",
                             "latest()",
                             c(gadm.rds = "gadm.rds"))


source('prepare_population.R')
source('aggregate_pop.R')
source('create_urban_rural.R')
source('get_par_raw.R')
source('missing_pop.R')
source('population_projection.R')
source('prepare_population.R')
source('rescale_raw_pop.R')



gadm<- read_rds('gadm.rds')

message('Population')
pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  
                              pattern = paste0("population_", iso3c), 
                              full.names = TRUE))
pop_raw <- long_pixel(pop_raster, pop_raster, gadm, "pop") |>
  create_urban_rural() |>
  missing_pop() |>
  rescale_raw_pop(iso = iso)

message("PfPr")
pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                             pattern = "pfpr", 
                             full.names = TRUE))
pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
rm(pfpr_rast)


message("PvPr")
pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                             pattern = "pvpr", 
                             full.names = TRUE))
pvpr_raw <- long_pixel(pvpr_rast, 
                       pop_raster, 
                       gadm, "pvpr")
rm(pvpr_rast)

message("Population at risk")
par_raw <- get_par_raw(pfpr_raw,
                       pvpr_raw, 
                       pop_raw)

message("Aggregating population")
population <- aggregate_pop(pop_raw, 
                            par_raw)
rm(pop_raw)


message("Population projection")
population <- population_projection(population, iso = iso, reference_year = wmr_year - 1) |>
  add_units(gadm_df)


saveRDS(population, 'population.rds')