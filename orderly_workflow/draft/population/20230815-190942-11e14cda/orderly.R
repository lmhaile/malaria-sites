# population  ------------------------------------------------------------------
#setwd('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/src/population/')
orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate population component of site file')
orderly2::orderly_artefact('population output', 'population.rds')
orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm.rds = "gadm.rds"))
orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm_df.rds = "gadm_df.rds"))

source('long_pixel.R')
source('prepare_population.R')
source('aggregate_pop.R')
source('create_urban_rural.R')
source('get_par_raw.R')
source('missing_pop.R')
source('population_projection.R')
source('prepare_population.R')
source('rescale_raw_pop.R')

#library(terra)
#library(dplyr)
#library(sjmisc)
gadm<- readRDS('gadm.rds')
gadm_df<- readRDS('gadm_df.rds')

packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

message('Population')
pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  
                              pattern = paste0("population_", iso), 
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
population <- population_projection(population, 
                                    iso = iso, 
                                    reference_year = wmr_year - 1) |>
              add_units(gadm_df)


saveRDS(population, 'population.rds')