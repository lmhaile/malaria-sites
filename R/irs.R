#' Set IRS target areas based on baseline (year 2000) prevalence
#'
#' @param prevalence prevalence data
#' @param irs_threshold IRS target prevalence threshold
#' @param nominal_irs_cov Place holder irs coverage that should be rescaled
prevalence_target_irs <- function(prevalence,
                                  irs_threshold = 0.01,
                                  nominal_irs_cov = 0.2){
  irs <- prevalence |>
    group_by(ID, urban_rural) |>
    mutate(
      irs_pf_target = if(is.na(pfpr[year == 2000]) | pfpr[year == 2000] < irs_threshold) 0 else 1,
      irs_pv_target = if(is.na(pvpr[year == 2000]) | pvpr[year == 2000] < irs_threshold) 0 else 1,
      irs_target = ifelse(irs_pf_target == 1 | irs_pv_target == 1, 1, 0)) |>
    ungroup() |>
    mutate(irs_cov = irs_target * nominal_irs_cov) |>
    dplyr::select(-c(irs_pf_target, irs_pv_target, irs_target, pfpr, pvpr))
  return(irs)
}

#' Rescale IRS coverage to match WMR people protected estimates
#'
#' @param irs IRS data
#' @param population population
#' @param iso iso3c code
#' @param wmr_irs_address address for WMR IRS data 
rescale_irs <- function(irs,
                        population,
                        iso,
                        wmr_irs_address = "data/irs_people_protected.csv"){
  irs_pp <- read.csv(wmr_irs_address) |>
    filter(iso3c == iso) |>
    select(year, irs_interpolated)
  
  irs_pp_scaler <- irs |>
    left_join(population, by = c("ID", "urban_rural", "year")) |>
    tidyr::fill(par, .direction = "down") |>
    group_by(year) |>
    summarise(irs_pp = sum(irs_cov * par, na.rm = TRUE)) |>
    ungroup() |>
    left_join(irs_pp, by = "year") |>
    mutate(irs_scaler = irs_interpolated / irs_pp,
           irs_scaler = ifelse(is.infinite(irs_scaler), 100, irs_scaler),
           irs_scaler = ifelse(irs_interpolated == 0, 0, irs_scaler)) |>
    select(year, irs_scaler)
  
  irs <- irs |>
    left_join(irs_pp_scaler, by = "year") %>%
    mutate(irs_cov = pmin(irs_cov * irs_scaler, 0.8)) |>
    select(-irs_scaler)
  
  if(mean(irs$irs_cov == 0.8, na.rm = TRUE) > 0.25){
    warning("Rescaling IRS coverage leads to >75% of estimates of coverage
            at maximum of 80%. Consider expanding spatial extent of IRS
            targeting by lowering the irs_threshold in add_vc()")
  }
  
  return(irs)
}

#' Add IRS insecticide
#'
#' @param irs IRS data
#' @param actellic_switch_year Assumed year of switch to Actellic
#' @param irs_insecticide_address Address of irs insecticide parameters
irs_add_insecticide <- function(irs, 
                                actellic_switch_year = 2017,
                                irs_insecticide_address = "data/irs_insecticide_parameters.csv"){
  insecticide <- read.csv(irs_insecticide_address)
  
  irs <- irs |>
    mutate(
      # Switch to Actellic-like insecticide
      irs_insecticide = ifelse(year < actellic_switch_year, "ddt", "actellic"),
      # Assume 1 round per year
      irs_spray_rounds = 1) |>
    left_join(insecticide, by = "irs_insecticide")
  
  return(irs)
}

#' Add average household size (number of occupants) for IRS HH sprayed outputs
#'
#' @param irs IRS data
#' @param iso iso3c code
#' @param irs_hh_size_address Address of irs household size estimates
irs_add_hh_size <- function(irs,
                            iso,
                            irs_hh_size_address = "data/hh_size.csv"){
  hh_size_data <- read.csv(irs_hh_size_address)
  if(iso %in% hh_size_data$iso3c){
    hh_size <- hh_size_data[hh_size_data$iso3c == iso, "hh_size"]
  } else{
    hh_size <- median(hh_size_data$hh_size)
  }
  
  irs$hh_size <- hh_size
  
  return(irs)
}