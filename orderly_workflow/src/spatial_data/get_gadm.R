# Get the simple feature file for a country
get_gadm <- function(iso3c,
                     admin_level = 1,
                     name1_overwrite = NULL){
  
  gadm <- readRDS(paste0("iso3c/", iso3c, "/", iso3c, "_", admin_level, ".RDS")) 
  
  if(!is.null(name1_overwrite)){
    gadm[,"NAME_1"] <- gadm[[name1_overwrite]]
  }
  
  gadm <- gadm |>
    dplyr::rename(iso3c = ID_0,
                  country = COUNTRY,
                  name_1 = NAME_1,
                  geometry = geom)
  
  if(admin_level == 2){
    gadm <-  gadm |>
      dplyr::rename(name_2 = NAME_2)
  }
  
  gadm <- gadm |>
    dplyr::select(dplyr::any_of(c("iso3c", "country", "name_1", "name_2", "geom")))
  
  return(gadm)
}
