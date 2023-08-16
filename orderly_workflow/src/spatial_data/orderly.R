# initialize site --------------------------------------------------------------
#setwd('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/src/pull_spatial_data/')
orderly2::orderly_parameters(iso = NULL, admin_level= 1)
orderly2::orderly_description('Initialize site')
orderly2::orderly_artefact('GADM file', 'gadm.rds')
orderly2::orderly_artefact('GADM units', 'gadm_df.rds')

source('get_gadm.R')
library(sf)
library(dplyr)

message('gadm')

gadm <- get_gadm(iso3c = iso, 
                 admin_level = admin_level)


if(iso == "VNM"){
  gadm <- get_gadm(iso3c = iso, 
                   admin_level = admin_level, 
                   name1_overwrite = "VARNAME_1")
}

message('processing')
gadm <- gadm |>
  mutate(ID = 1:n())
gadm_df <- sf::st_drop_geometry(gadm)


saveRDS(gadm, 'gadm.rds')
saveRDS(gadm_df, 'gadm_df.rds')

