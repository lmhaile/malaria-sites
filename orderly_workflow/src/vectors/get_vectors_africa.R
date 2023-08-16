
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
