#*# * Download raster data * #*#
setwd('M:/Lydia/malaria_sites/')

library(malariaAtlas)
library(terra)
library(cart)
#remotes::install_github("mrc-ide/cart")
### Inputs #####################################################################
countries <- read.csv("raw_data/wmr/wmr_countries.csv")
years <- 2000:2021
address <- "data/raster/"
################################################################################

### Define which rasters to obtain and what they will be called ################
map_rasters <- suppressMessages(malariaAtlas::listRaster())

prevalence_rasters = list(
  pfpr = "Plasmodium falciparum PR2 - 10 version 2020",
  pvpr = "Plasmodium vivax PR1-99 version 2020"
)
intervention_rasters = list(
  itn_use = "Insecticide treated bednet (ITN) use version 2020",
  irs_cov = "Indoor Residual Spraying (IRS) coverage version 2020",
  tx = "Effective treatment with an Antimalarial drug version 2020"
)

spatial_limits_rasters = list(
  pf_limits = "Plasmodium falciparum Spatial Limits",
  pv_limits = "Plasmodium vivax Spatial Limits"
)
vector_relative_abundance_rasters = list(
  relative_funestus = "Anopheles funestus",
  relative_arabiensis = "Anopheles arabiensis Patton, 1905",
  relative_gambiae = "Anopheles gambiae Giles, 1902"
)
vector_occurence_rasters  <- as.list(map_rasters[grepl("2010_Anopheles", map_rasters$raster_code),1])
names(vector_occurence_rasters) <- sapply(
  stringr::str_split(unlist(vector_occurence_rasters), pattern = "\\ "),
  function(x){
    paste0("occurence_", x[2])
  })
################################################################################

### Download temporal rasters ##################################################
prevalence_rasters_available <- lapply(prevalence_rasters,
                                       cart:::raster_available,
                                       years = years,
                                       raster_list = map_rasters)
intervention_rasters_available <- lapply(intervention_rasters,
                                         cart:::raster_available,
                                         years = years,
                                         raster_list = map_rasters)

for(y in seq_along(years)){
  year <- years[y]
  for(p in seq_along(prevalence_rasters)){
    if(prevalence_rasters_available[[p]][y]){
      raster_name <- paste0(address, names(prevalence_rasters)[p], "_", year, ".tif")
      r1 <- getRaster(surface = prevalence_rasters[[p]], year = year)
      r2 <- terra::rast(r1)
      terra::writeRaster(r2, raster_name)
    }
  }
  
  for(i in seq_along(intervention_rasters)){
    if(intervention_rasters_available[[i]][y]){
      raster_name <- paste0(address, names(intervention_rasters)[i], "_", year, ".tif")
      r1 <- getRaster(surface = intervention_rasters[[i]], year = year)
      r2 <- terra::rast(r1)
      terra::writeRaster(r2, raster_name)
    }
  }
}
################################################################################

### Download non-temporal rasters ##############################################
for(i in seq_along(spatial_limits_rasters)){
  raster_name <- paste0(address, names(spatial_limits_rasters)[i], ".tif")
  r1 <- getRaster(surface = spatial_limits_rasters[[i]])
  r2 <- terra::rast(r1)
  terra::writeRaster(r2, raster_name)
}

for(i in seq_along(vector_relative_abundance_rasters)){
  raster_name <- paste0(address, names(vector_relative_abundance_rasters)[i], ".tif")
  r1 <- getRaster(surface = vector_relative_abundance_rasters[[i]], vector_year = 2017)
  r2 <- terra::rast(r1)
  terra::writeRaster(r2, raster_name)
}

for(i in seq_along(vector_occurence_rasters)){
  raster_name <- paste0(address, names(vector_occurence_rasters)[i], ".tif")
  r1 <- getRaster(surface = vector_occurence_rasters[[i]], vector_year = 2010)
  r2 <- terra::rast(r1)
  terra::writeRaster(r2, raster_name)
}
################################################################################

### Download population rasters ################################################
for(y in seq_along(years)){
  year <- years[y]
  for(z in seq_along(countries$iso3c)){
    iso3c <- countries$iso3c[z]
    r1 <- cart::get_pop(iso3c, year, TRUE)
    raster_name <- paste0(address, "population_", iso3c, "_", year, ".tif")
    terra::writeRaster(r1, raster_name)
  }
}
################################################################################

