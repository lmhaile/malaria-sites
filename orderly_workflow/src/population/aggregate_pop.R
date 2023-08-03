
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
