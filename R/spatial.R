# Get the simple feature file for a country
get_gadm <- function(iso3c,
                     admin_level = 1,
                     gadm_dir = "C:/Users/lhaile/OneDrive - Imperial College London/GADM/version_4.0.4/iso3c",
                     name1_overwrite = NULL){
  
  gadm <- readRDS(paste0(gadm_dir, "/", iso3c, "/", iso3c, "_", admin_level, ".RDS")) 
  
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

#' Link spatial unit ID to full names
#'
#' @param x dataset with ID index column
#' @param gadm_df spatial units data
add_units <- function(x,
                      gadm_df){
  x <- x |>
    left_join(gadm_df, by = "ID") |>
    select(-ID) |>
    select(country, iso3c, name_1, any_of(c("name_2", "urban_rural", "year")), everything())
  return(x)
}

#' Extracts raster data to long form df
#'
#' @param stack Raster or raster stack
#' @param country_pop country population data. Extend and resolution of this will 
#' be matched
#' @param gadm admin boundaries spatial file 
#' @param name name of variable to extract
#' @param start_year Staring year of stack
long_pixel <- function(stack,
                       country_pop,
                       gadm,
                       name,
                       start_year = 2000){
  sitesv <- methods::as(gadm, "SpatVector")
  
  country_stack <- terra::crop(stack, gadm) |>
    resample(country_pop)
  
  raw_values <- terra::extract(x = country_stack, y = sitesv)
  ### Assumes raster stack is ordered temporally
  colnames(raw_values) <- c("ID", paste(start_year:(start_year + ncol(raw_values) - 2)))
  
  raw_values_long <- raw_values |>
    group_by(ID) |>
    mutate(pixel = 1:n()) |>
    pivot_longer(cols = -c(ID, pixel), names_to = "year", values_to = name, names_transform = list(year = as.integer)) |>
    ungroup()
  
  raw_values_long
}