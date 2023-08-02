prepare_vectors<- function(iso3c, gadm){

  pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  pattern = paste0("population_", iso3c), full.names = TRUE))
  
  # Vectors
  if(continent == "Africa"){
    vectors <- get_vectors_africa(pop_raster, gadm)
  } else {
    vectors <- get_vectors_not_africa(pop_raster, gadm)
  }
  vectors <- vectors |>
    group_by(ID) |>
    mutate(prop = prop / sum(prop)) |>
    ungroup() |>
    add_units(gadm_df)
  
  missing_vectors <- dplyr::anti_join(gadm_df, vectors) 
  if(nrow(missing_vectors) > 0){
    vectors_append <- missing_vectors |>
      select(-ID) |>
      mutate(species = "unknown",
             prop_na = 0,
             prop = 1,
             blood_meal_rates = median(vectors$blood_meal_rates),
             foraging_time = median(vectors$foraging_time),
             Q0 = median(vectors$Q0),
             phi_bednets = median(vectors$phi_bednets),
             phi_indoors = median(vectors$phi_indoors),
             mum = median(vectors$mum))
    vectors <- bind_rows(vectors, vectors_append)
  }
  
 return(vectors) 
}





#' Africa vectors - informed by MAP relative abundance estimates
#'
#' @param pop_raster Population data
#' @param gadm admin boundaries
#' @param binomics_address address of vector bionomics data
get_vectors_africa <- function(pop_raster,
                               gadm,
                               binomics_address = "data/vector_bionomics.csv"){
  vb <- read.csv(binomics_address)
  ra <- list.files("M:/Pete/malaria_sites/data/raster/", pattern = "relative", full.names = TRUE)
  ra_names <- list.files("M:/Pete/malaria_sites/data/raster/", pattern = "relative") |>
    stringr::str_replace("relative_", "") |>
    stringr::str_replace(".tif", "")
  vector_raw <- list()
  for(i in seq_along(ra)){
    vector_rast <- terra::rast(ra[i])
    vector_raw[[i]] <- long_pixel(vector_rast, pop_raster, gadm, ra_names[i])
  }
  vectors <- purrr::reduce(vector_raw, left_join, by = c("ID", "pixel", "year")) |>
    select(-year) |>
    pivot_longer(cols = -c(ID, pixel), names_to = "species", values_to = "prop") |>
    filter(!is.na(prop)) |>
    group_by(ID, pixel) |>
    mutate(prop = prop / sum(prop)) |>
    group_by(ID, species) |>
    summarise(prop = mean(prop, na.rm = TRUE)) |>
    ungroup() |>
    left_join(vb, by = "species")
  return(vectors)
}




#' Non-Africa vectors - informed by MAP relative occurence estimates
#'
#' @param pop_raster Population data
#' @param gadm admin boundaries
#' @param binomics_address address of vector bionomics data
get_vectors_not_africa <- function(pop_raster,
                                   gadm,
                                   binomics_address = "data/vector_bionomics.csv"){
  vb <- read.csv(binomics_address)
  ra <- list.files("M:/Pete/malaria_sites/data/raster/", pattern = "occurence_", full.names = TRUE)
  ra_names <- list.files("M:/Pete/malaria_sites/data/raster/", pattern = "occurence_") |>
    stringr::str_replace("occurence_", "") |>
    stringr::str_replace(".tif", "")
  vector_raw <- list()
  index <- 1
  for(i in seq_along(ra)){
    vector_rast <- terra::rast(ra[i])
    # Check if country and vector map overlap at all
    ext_vector <- ext(vector_rast)
    ext_pop <- ext(pop_raster)
    overlap <- (ext_vector[1] < ext_pop[2] & ext_pop[1] < ext_vector[2] &
                  ext_vector[3] < ext_pop[4] & ext_pop[3] < ext_vector[4])
    if(overlap){
      vector_raw[[index]] <- long_pixel(vector_rast, pop_raster, gadm, ra_names[i])
      index <- index + 1
    }
  }
  vectors <- purrr::reduce(vector_raw, left_join, by = c("ID", "pixel", "year")) |>
    select(-year) |>
    pivot_longer(cols = -c(ID, pixel), names_to = "species", values_to = "prop") |>
    group_by(ID, species) |>
    summarise(prop_na = mean(is.na(prop)),
              prop = mean(prop, na.rm = TRUE)) |>
    ungroup() |>
    filter(!is.na(prop)) |>
    group_by(ID) |>
    # Keep the top 3 by averaged probability of occurence
    slice_max(order_by = prop, n = 3, with_ties = FALSE) |>
    # Give species equal relative abundance as we do not have any other data
    mutate(prop = 1 / n()) |>
    ungroup()  |>
    left_join(vb, by = "species") |>
    # Currently, for vectors with unknown bionomics we take the median
    replace_na(list(blood_meal_rates = median(vb$blood_meal_rates),
                    foraging_time = median(vb$foraging_time),
                    Q0 = median(vb$Q0),
                    phi_bednets = median(vb$phi_bednets),
                    phi_indoors = median(vb$phi_indoors),
                    mum = median(vb$mum)))
  
  return(vectors)
}
