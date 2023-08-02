
prepare_demography<- function(iso3c, gadm){
  
  message("Demography")
  # Demography  
  demography <- load_deathrates(iso3c = iso3c) |>
    filter(year >= 2000)
  if(iso3c == "BWA"){
    demography <- load_deathrates(iso3c = "ZWE") |>
      filter(year >= 2000) |>
      mutate(iso3c = "BWA",
             country = "Botswana")
  }
  if(iso3c == "BRA"){
    demography <- load_deathrates(iso3c = "PER") |>
      filter(year >= 2000) |>
      mutate(iso3c = "BRA",
             country = "Brazil")
  }
  if(iso3c == "DOM"){
    demography <- load_deathrates(iso3c = "CUB") |>
      filter(year >= 2000) |>
      mutate(iso3c = "DOM",
             country = "Dominican Republic")
  }
  if(iso3c == "PAN"){
    demography <- load_deathrates(iso3c = "NIC") |>
      filter(year >= 2000) |>
      mutate(iso3c = "PAN",
             country = "Panama")
  }
  
  return(demography)
  
}
