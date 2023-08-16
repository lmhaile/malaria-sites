
initialize_site<- function(iso3c, admin_level){
  
  gadm <- get_gadm(iso3c = iso, 
                   admin_level = admin_level)
  
  
  if(iso3c == "VNM"){
    gadm <- get_gadm(iso3c = iso, 
                     admin_level = admin_level, 
                     name1_overwrite = "VARNAME_1")
  }
  
  gadm <- gadm |>
    mutate(ID = 1:n())
  gadm_df <- sf::st_drop_geometry(gadm)
  
  
  return(gadm)
  
}



