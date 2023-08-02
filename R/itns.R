#' Set ITN target areas based on baseline (year 2000) prevalence
#'
#' @param prevalence prevalence data
#' @param itn_threshold ITN target prevalence threshold
#' @param nominal_itn_use Place holder itn usage that should be rescaled
prevalence_target_itns <- function(prevalence,
                                   itn_threshold = 0.01,
                                   nominal_itn_use = 0.2){
  itns <- prevalence |>
    group_by(ID, urban_rural) |>
    mutate(
      itn_pf_target = if(is.na(pfpr[year == 2000]) | pfpr[year == 2000] < itn_threshold) 0 else 1,
      itn_pv_target = if(is.na(pvpr[year == 2000]) | pvpr[year == 2000] < itn_threshold) 0 else 1,
      itn_target = ifelse(itn_pf_target == 1 | itn_pv_target == 1, 1, 0)) |>
    ungroup() |>
    mutate(itn_use = itn_target * nominal_itn_use) |>
    dplyr::select(-c(itn_pf_target, itn_pv_target, itn_target, pfpr, pvpr))
  return(itns)
}

#' Rescale ITNs to match the number of ITNs distributed in WMR
#'
#' @param itns ITN data
#' @param population Population data
#' @param iso iso3c code
#' @param retention_half_life Net retention half life (days)
#' @param usage_rate Usage rate
#' @param wmr_itn_address address of WMR data distribution numbers
#' @param distribution_freq Frequency of ITN distribution (days)
rescale_itns <- function(itns,
                         population,
                         iso,
                         retention_half_life,
                         usage_rate,
                         wmr_itn_address = "data/itn_delivered.csv",
                         distribution_freq = 3 * 365){
  
  # Link to population
  itns <- itns |>
    left_join(population, by = c("ID", "urban_rural", "year"))
  
  # Par of those using ITNs
  itn_par <- itns |>
    filter(itn_use > 0) |>
    group_by(year) |>
    summarise(par = sum(par, na.rm = TRUE))
  
  # Estimate usage from WMR distribution numbers
  itns_wmr <- read.csv(wmr_itn_address) |>
    filter(iso3c == iso) |>
    select(year, itn_interpolated) |>
    left_join(itn_par, by = "year") |>
    mutate(
      crop = netz::distribution_to_crop(itn_interpolated / par,
                                        distribution_freq = distribution_freq,
                                        half_life = retention_half_life),
      access = netz::crop_to_access(crop, type = "loess_extrapolate"),
      access = ifelse(is.na(access), netz::crop_to_access(crop, type = "linear"), access),
      access = ifelse(is.na(access), 1, access),
      use_interpolated = netz::access_to_usage(access, usage_rate))
  
  # Estimate scaling of itn usage needed to match wmr use
  itn_use_scaler <- itns |>
    group_by(year) |>
    summarise(itn_use = weighted.mean(itn_use, par, na.rm = TRUE)) |>
    ungroup() |>
    left_join(itns_wmr, by = "year") |>
    mutate(itn_scaler = use_interpolated / itn_use,
           itn_scaler = ifelse(is.infinite(itn_scaler), 100, itn_scaler)) |>
    select(year, itn_scaler)
  
  # Rescale ITN use (up to a fixed maximum usage)
  itns <- itns |>
    left_join(itn_use_scaler, by = "year") %>%
    mutate(itn_use = pmin(itn_use * itn_scaler, 0.8)) |>
    select(ID, urban_rural, year, itn_use)
  
  if(mean(itns$itn_use == 0.8, na.rm = TRUE) > 0.25){
    warning("Rescaling ITN usage leads to >75% of estimates of coverage
            at maximum of 80%. Consider expanding spatial extent of ITNs
            targeting by lowering the itn_threshold in add_vc()")
  }
  
  return(itns)
}

#' ITN usage rate
#'
#' For countries outside of SSA the median of countries within SSA is returned
#'
#' @param iso iso3c country code
itn_usage_rate <- function(iso){
  # Usage rates
  urd <- netz::get_usage_rate_data()
  
  if(iso %in% urd$iso3){
    ur <- urd[urd$iso3 == iso, "usage_rate"]
  } else {
    ur <- median(urd$usage_rate)
  }
  return(ur)
}

#' ITN rentention halflife
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

#' Estimate model input distribution needed to achieve target usage
#'
#' @param itns ITNs data
#' @param stopval Stopping criterion
#' @param maxevel Maximum evaluations to try when fitting
itn_input_dist <- function(itns,
                           stopval = 0.01,
                           maxevel = 5000){
  itns <- itns |>
    group_by(ID, urban_rural) |>
    mutate(# Inform the upper bound of distribution from observed use-changes
      # This is to avoid over-distributing by replacing existing nets too much
      use_change = diff(c(0, itn_use)),
      dist_upper = ifelse(use_change > 0, 1, 0.2),
      itn_input_dist = netz::fit_usage(
        target_usage = itn_use,
        # Assume mating mid-year usage measure
        target_usage_timesteps = 1 + (year - 2000 + 0.5) * 365,
        # Assume distribution at start of year
        distribution_timesteps = 1 + (year - 2000) * 365,
        distribution_upper = dist_upper, 
        distribution_init  = pmin(dist_upper, itn_use / 2),
        # Stop if sum of squared dif < 0.01
        control = list(stopval = stopval, maxeval = maxevel))$par) |>
    select(-c(use_change, dist_upper)) |>
    ungroup()
  return(itns)
}

#' Add a net type column
#'
#' @param itns ITNs data
#' @param pyrethroid_pbo_switch_year Year when it is assumed a switch is made to pyrethroid-pbo nets
net_type <- function(itns, pyrethroid_pbo_switch_year = 2025){
  itns <- itns |>
    mutate(net_type = ifelse(year < pyrethroid_pbo_switch_year, "pyrethroid_only", "pyrethroid_pbo"))
}

#' Link to pyrethroid resistance
#'
#' @param itns ITN data
#' @param pyrethroid_resistance_address address of pyrethroid resistance estimates
#' @param net_efficacy_addresss address of net efficacy estimate data
itns_add_insecticide <- function(itns,
                                  pyrethroid_resistance_address = "data/pyrethroid_resistance.csv",
                                  net_efficacy_addresss = "data/net_efficacy.csv"){
  pyrethroid_resistance <- read.csv(pyrethroid_resistance_address)
  net_efficacy <- read.csv(net_efficacy_addresss)
  
  resistance <- itns |>
    left_join(pyrethroid_resistance, by = c("iso3c", "name_1", "year")) |>
    group_by(year) |>
    mutate(pyrethroid_resistance = ifelse(is.na(pyrethroid_resistance), median(pyrethroid_resistance, na.rm = TRUE), pyrethroid_resistance )) |>
    ungroup() |>
    replace_na(list(pyrethroid_resistance = 0)) |>
    mutate(pyrethroid_resistance = round(pyrethroid_resistance, 2)) |>
    left_join(net_efficacy, by = c("pyrethroid_resistance", "net_type"))
  
  return(resistance)
}

get_pyrethroid_resistance <- function(sites, pyrethroid_resistance_address = "data/pyrethroid_resistance.csv"){
  pr <- read.csv(pyrethroid_resistance_address)
  
  pyrethroid_resistance <- sites |>
    left_join(data.frame(year = 2000:2050), by = character()) |>
    left_join(pr, by = c("iso3c", "name_1", "year")) |>
    group_by(year) |>
    mutate(pyrethroid_resistance = ifelse(is.na(pyrethroid_resistance), median(pyrethroid_resistance, na.rm = TRUE), pyrethroid_resistance )) |>
    ungroup() |>
    replace_na(list(pyrethroid_resistance = 0))

  return(pyrethroid_resistance)
}