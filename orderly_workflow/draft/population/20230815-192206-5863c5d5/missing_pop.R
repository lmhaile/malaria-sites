
#' Unsure complete set of spatial units and years
#'
#' @param pop_raw Population data
missing_pop <- function(pop_raw){
  pop_raw <- pop_raw |>
    complete(nesting(ID, urban_rural, pixel), year) |>
    replace_na(list(pop = 0))
  
  return(pop_raw)
}

