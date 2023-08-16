
#' create urban/rural split
#'
#' @param pop_raw popualtion data
#' @param urban_density_threshold Threshold density that is classified as urban 
create_urban_rural <- function(pop_raw,
                               urban_density_threshold = 1500){
  pop_raw <- pop_raw |>
    # Areas with no pop are considered rural
    mutate(urban_rural = ifelse(pop < urban_density_threshold | is.na(pop), "rural", "urban"))
  return(pop_raw)
}
