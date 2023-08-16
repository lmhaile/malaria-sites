# Demography  ------------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate demography component of site file')
orderly2::orderly_artefact('demography output', 'demography.rds')

packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)


demography <- load_deathrates(iso3c = iso) |>
  filter(year >= 2000)
if(iso == "BWA"){
  demography <- load_deathrates(iso3c = "ZWE") |>
    filter(year >= 2000) |>
    mutate(iso3c = "BWA",
           country = "Botswana")
}
if(iso == "BRA"){
  demography <- load_deathrates(iso3c = "PER") |>
    filter(year >= 2000) |>
    mutate(iso3c = "BRA",
           country = "Brazil")
}
if(iso == "DOM"){
  demography <- load_deathrates(iso3c = "CUB") |>
    filter(year >= 2000) |>
    mutate(iso3c = "DOM",
           country = "Dominican Republic")
}
if(iso == "PAN"){
  demography <- load_deathrates(iso3c = "NIC") |>
    filter(year >= 2000) |>
    mutate(iso3c = "PAN",
           country = "Panama")
}


saveRDS(demography, 'demography.rds')
