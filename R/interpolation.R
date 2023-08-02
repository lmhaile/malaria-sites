#' Interpolation for intervention missing data
#'
#' @param y intervention measure
#' @param years years
#' @param target_years output years 
#'
#' @return Interpolated y
interpolate_int <- function(y, years){
  missing <- is.na(y)
  y <- y[!missing]
  input_years <- years[!missing]
  
  if(sum(!missing) > 1){
    # Interpolate linearly between values
    y_interp <- approx(input_years, y, years)$y
  } else {
    y_interp <- y
  }
  # Trailing NAs are assummed to have constant coverage from last known value
  y_interp[is.na(y_interp)] <- y_interp[max(which(!is.na(y_interp)))]
  return(y_interp)
}

complete_years <- function(x, years){
  x <- x |>
  complete(nesting(ID, urban_rural), year = years) |>
    group_by(ID, urban_rural) |>
    fill(everything(), .direction = "down") |>
    ungroup()
  return(x)
}