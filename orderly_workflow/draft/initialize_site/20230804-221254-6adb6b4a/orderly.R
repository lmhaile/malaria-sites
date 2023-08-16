

orderly2::orderly_parameters(iso3c = NULL, admin_level= 1)
orderly2::orderly_artefact('GADM file', 'gadm.rds')

source('get_gadm.R')

gadm <- get_gadm(iso3c = iso3c, 
                 admin_level = admin_level)


if(iso3c == "VNM"){
  gadm <- get_gadm(iso3c = iso3c, 
                   admin_level = admin_level, 
                   name1_overwrite = "VARNAME_1")
}

gadm <- gadm |>
  mutate(ID = 1:n())
gadm_df <- sf::st_drop_geometry(gadm)




saveRDS(gadm_df, 'gadm.rds')

