
aggregate_prevalence <- function(pfpr_raw, pvpr_raw, par_raw){
  ap <- pfpr_raw |>
    left_join(pvpr_raw,  c("ID", "pixel", "year")) |>
    left_join(par_raw,  c("ID", "pixel", "year")) |>
    mutate(pvpr = ifelse(pvpr == -1, NA, pvpr)) |>
    group_by(ID, urban_rural, year) |>
    summarise(
      pfpr = ifelse(sum(par_pf) == 0,
                    mean(pfpr, na.rm = TRUE),
                    weighted.mean(pfpr, par_pf, na.rm = TRUE)),
      par_pf = sum(par_pf),
      pvpr = ifelse(sum(par_pv) == 0,
                    mean(pvpr, na.rm = TRUE),
                    weighted.mean(pvpr, par_pv, na.rm = TRUE)),
      par_pv = sum(par_pv),
      par = sum(par)) |>
    ungroup() |>
    replace_na(list(pvpr = 0, pfpr = 0)) |>
    select(ID, urban_rural, year, pfpr, pvpr)
  return(ap)
}