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

aggregate_itn_use <- function(itns_raw, par_raw){
  ap <- itns_raw |>
    left_join(par_raw, by = c("ID", "pixel", "year")) |>
    group_by(ID, urban_rural, year) |>
    summarise(itn_use = ifelse(sum(par) == 0,
                               0,
                               weighted.mean(itn_use, par, na.rm = TRUE))) |>
    ungroup()
  return(ap)
}

aggregate_irs <- function(irs_raw, par_raw){
  ap <- irs_raw |>
    left_join(par_raw, by = c("ID", "pixel", "year")) |>
    group_by(ID, urban_rural, year) |>
    summarise(irs_cov = ifelse(sum(par) == 0,
                               0,
                               weighted.mean(irs_cov, par, na.rm = TRUE))) |>
    ungroup() |>
    replace_na(list(irs_cov = 0))
  return(ap)
}

aggregate_tx <- function(tx_raw, par_raw){
  ap <- tx_raw |>
    left_join(par_raw, by = c("ID", "pixel", "year")) |>
    group_by(ID, urban_rural, year) |>
    summarise(tx_cov = ifelse(sum(par) == 0,
                              0,
                              weighted.mean(tx_cov, par, na.rm = TRUE))) |>
    ungroup() |>
    group_by(year) |>
    # Areas with missing value get the country-wide meadian for that year
    mutate(tx_cov = ifelse(is.na(tx_cov), median(tx_cov, na.rm = TRUE), tx_cov)) |>
    ungroup()
  return(ap)
}

aggregate_pop <- function(pop_raw, par_raw){
  pop <- pop_raw |>
    left_join(par_raw, by = c("ID", "pixel", "year", "urban_rural")) |>
    group_by(ID, urban_rural, year) |>
    summarise(pop = round(sum(pop)),
              par = round(sum(par)),
              par_pf = round(sum(par_pf)),
              par_pv = round(sum(par_pv))) |>
    ungroup()
  return(pop)
}