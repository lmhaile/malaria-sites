#*# * Appending calibrated EIRs * #*#

library(readr)

### Input ######################################################################
isos <- read.csv("raw_data/wmr/wmr_countries.csv") |>
  dplyr::mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) |>
  dplyr::filter(continent != "Africa") |>
  dplyr::pull(iso3c)
version <- "2022/v1.0/"
################################################################################

### Load individual calibrations and append to country site file ###############
for(iso in isos){
  site_file <- readRDS(paste0(version, iso, "/", iso, ".RDS"))
  
  if(iso == "IND"){
    type_string <-  "ccccccd"
  } else {
    type_string <-  "cccccd"
  }
  
  fitted_eir <- lapply(
    list.files(paste0(version, iso, "/eir_fits/"), full.names = TRUE), read_csv, 
    col_types = type_string,
    locale = locale(encoding = "latin1")) |>
    dplyr::bind_rows()
  
  # Check none of the fitting has failed
  n_fits <- length(list.files(paste0(version, iso, "/parameters/")))
  if(n_fits != nrow(fitted_eir)){
    message("EIR fits missing for ", iso)
  }
  
  # Append full selection
  site_file$eir <- site_file$sites |>
    dplyr::left_join(data.frame(spp = c("pf", "pv")), by = character()) |>
    dplyr::left_join(fitted_eir) |>
    tidyr::replace_na(list(eir = 0))
  
  saveRDS(site_file, paste0(version, iso, "/", iso, ".RDS"))
}
################################################################################