#' Create Population at risk based on limits of transmission in 2000
#'
#' @param pfpr_raw PfPr
#' @param pvpr_raw PvPr
#' @param pop_raw population data
get_par_raw <- function(pfpr_raw,
                        pvpr_raw,
                        pop_raw){
  combined <- pfpr_raw |>
    left_join(pvpr_raw, by = c("ID", "pixel", "year")) |>
    left_join(pop_raw, by = c("ID", "pixel", "year")) |>
    mutate(pvpr = ifelse(pvpr == -1, NA, pvpr))
  
  spatial_limits <- combined |>
    filter(year == 2000) |>
    mutate(
      pf_limits = ifelse(is.na(pfpr) | pfpr == 0, 0, 1),
      pv_limits = ifelse(is.na(pvpr) | pvpr == 0, 0, 1),
      limits = ifelse(pf_limits == 1 | pv_limits == 1, 1, 0)
    ) |>
    select(ID, pixel, pf_limits, pv_limits, limits)
  
  ap <- pop_raw |>
    left_join(spatial_limits, by = c("ID", "pixel")) |>
    mutate(par_pf = pf_limits * pop,
           par_pv = pv_limits * pop,
           par = limits * pop) |>
    select(ID, pixel, year, urban_rural, par, par_pf, par_pv)
  return(ap)
}
