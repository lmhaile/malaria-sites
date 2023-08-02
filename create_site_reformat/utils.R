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