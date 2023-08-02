tx_act <- function(tx, iso, act_address = "data/proportion_act.csv"){
  proportion_act <- read.csv(act_address)
  
  if(iso %in% proportion_act$iso3c){
    pa <- proportion_act |>
      dplyr::filter(iso3c == iso) |>
      select(year, prop_act)
  } else {
    pa <- proportion_act |>
      dplyr::group_by(year) |>
      dplyr::summarise(prop_act = mean(prop_act)) |>
      dplyr::ungroup()
  }
  
  tx <-  tx |>
    dplyr::left_join(pa, by = "year")
  
  return(tx)
}

tx_public <- function(tx, iso, public_address = "data/proportion_public.csv"){
  proportion_public <- read.csv(public_address)
  if(iso %in% proportion_public$iso3c){
    pp <- proportion_public[proportion_public$iso3c == iso ,"prop_public"]
  } else {
    pp <- median(proportion_public$prop_public)
  }
  
  tx <- tx |>
    dplyr::mutate(prop_public = pp)
  
  return(tx)
}