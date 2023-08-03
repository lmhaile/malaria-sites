
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
