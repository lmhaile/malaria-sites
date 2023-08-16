# interventions ----------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate intervention component of site file')
orderly2::orderly_artefact('interventions output', 'interventions.rds')

message("ITNs")
packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm.rds = "gadm.rds"))

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm_df.rds = "gadm_df.rds"))

gadm<- readRDS('gadm.rds')
gadm_df<- readRDS('gadm_df.rds')

continent <- countrycode(iso, "iso3c", "continent")

pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  
                                pattern = paste0("population_", iso), 
                                full.names = TRUE))
  
message('PfPr')
pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                               pattern = "pfpr", 
                               full.names = TRUE))
pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
rm(pfpr_rast)
  
  
message("PvPr")
pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                             pattern = "pvpr", #
                             full.names = TRUE))
pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")
rm(pvpr_rast)
  
par_raw <- get_par_raw(pfpr_raw, 
                       pvpr_raw, 
                       pop_raw)
  
  
# ITNs
retention_half_life <- itn_retention_half_life(iso)
usage_rate <- itn_usage_rate(iso)
  if(continent == "Africa"){
    itn_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "itn", full.names = TRUE))
    itn_raw <- long_pixel(itn_rast, pop_raster, gadm, "itn_use")
    itns <- aggregate_itn_use(itn_raw, par_raw)|>
      complete_years(2000:(wmr_year-1))
    rm(itn_rast, itn_raw)
  } else {
    itns <- prevalence_target_itns(prevalence) |>
      complete_years(2000:(wmr_year-1)) |>
      rescale_itns(population = population,
                   iso = iso,
                   retention_half_life = retention_half_life,
                   usage_rate = usage_rate)
  }
  itns <- itns |>
    complete_years(2000:(wmr_year + 1)) |>
    group_by(ID, urban_rural) |>
    mutate(itn_use = ifelse(year == 2020, itn_use[year == (2020 - 3)], itn_use),
           itn_use = ifelse(year == 2021, itn_use[year == (2021 - 3)], itn_use),
           itn_use = ifelse(year == 2022, itn_use[year == (2022 - 3)], itn_use)) |>
    ungroup() |>
    itn_input_dist() |>
    net_type()
  
  message("IRS")
  # IRS
  if(continent == "Africa"){
    irs_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/",
                                pattern = "irs", 
                                full.names = TRUE))
    irs_raw <- long_pixel(irs_rast, pop_raster, gadm, "irs_cov")
    irs <- aggregate_irs(irs_raw, par_raw)
    rm(irs_rast, irs_raw)
  } else {
    irs <- prevalence_target_irs(prevalence)
  }
  irs <- irs |>
    complete_years(2000:wmr_year) |>
    rescale_irs(population, iso3c) |>
    irs_add_hh_size(iso = iso3c)
  
  message("Treatment")
  # TX
  tx_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                             pattern = "tx", 
                             full.names = TRUE))
  tx_raw <- long_pixel(tx_rast, pop_raster, gadm, "tx_cov")
  tx <- aggregate_tx(tx_raw, par_raw) |>
    tx_act(iso3c) |>
    tx_public(iso3c)
  rm(tx_rast, tx_raw)
  rm(par_raw)
  message("SMC")
  # SMC
  smc_address = "M:/Pete/malaria_sites/data/smc_coverage.csv"
  smc_data <- read.csv(smc_address)
  smc <- tx |>
    select(-tx_cov) |>
    left_join(gadm_df, by = "ID") |>
    left_join(smc_data, by = c("year", "iso3c", "name_1")) |>
    replace_na(list(smc_cov = 0)) |>
    select(ID, urban_rural, year, smc_cov) |>
    mutate(smc_min_age = round(0.25 * 365),
           smc_max_age = round(5 * 365),
           smc_n_rounds = 4,
           smc_drug = "sp_aq")
  message("RTSS")
  # RTSS
  rtss_address = "M:/Pete/malaria_sites/data/rtss_coverage.csv"
  rtss_data <- read.csv(rtss_address)
  rtss <- tx |>
    select(-tx_cov) |>
    left_join(gadm_df, by = "ID")  |>
    left_join(rtss_data, by = c("year", "iso3c", "name_1")) |>
    replace_na(list(rtss_cov = 0)) |>
    select(ID, urban_rural, year, rtss_cov)
  
  
  message("PMC")
  # PMC (formerly IPTi)
  pmc <- tx |>
    select(-tx_cov, -prop_public, -prop_act) |>
    mutate(pmc_cov = 0,
           pmc_drug = "sp")
  
  
  # Interventions
  interventions <- reduce(list(itns, tx,  irs, smc, rtss, pmc), left_join, by = c("ID", "urban_rural", "year")) |>
    complete_years(2000:(wmr_year + 1)) |>
    add_units(gadm_df) |>
    itns_add_insecticide() |>
    irs_add_insecticide()
  
  
saveRDS(interventions, 'interventions.rds')


