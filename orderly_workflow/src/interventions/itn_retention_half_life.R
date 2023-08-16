#' ITN retention halflife
#' 
#' For countries outside of SSA the median of countries within SSA is returned
#'
#' @param iso iso3c country code
itn_retention_half_life <- function(iso){
  # Usage rates
  hld <- netz::get_halflife_data()
  
  if(iso %in% hld$iso3){
    hl <- hld[hld$iso3 == iso, "half_life"]
  } else {
    hl <- median(hld$half_life)
  }
  return(hl)
}
