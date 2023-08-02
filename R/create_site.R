create_site <- function(iso3c, gadm, wmr_year, admin_level, version_folder,
                        diagnostics = TRUE, overwrite = FALSE){
  message("Site ", iso3c)
  # Create output directory
  outdir <- paste0(version_folder, iso3c, "/")
  
  
  message("Spatial")
  continent <- countrycode(iso3c, "iso3c", "continent")
  gadm <- gadm |>
    mutate(ID = 1:n())
  gadm_df <- st_drop_geometry(gadm)
  
  message("Population")
  # Prepare pop
  pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  pattern = paste0("population_", iso3c), full.names = TRUE))
  pop_raw <- long_pixel(pop_raster, pop_raster, gadm, "pop") |>
    create_urban_rural() |>
    missing_pop() |>
    rescale_raw_pop(iso = iso3c)
  message("PfPr")
  # Pf
  pfpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pfpr", full.names = TRUE))
  pfpr_raw <- long_pixel(pfpr_rast, pop_raster, gadm, "pfpr")
  rm(pfpr_rast)
  message("PvPr")
  # Pv
  pvpr_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", pattern = "pvpr", full.names = TRUE))
  pvpr_raw <- long_pixel(pvpr_rast, pop_raster, gadm, "pvpr")
  rm(pvpr_rast)
  message("Population at risk")
  # Population at risk raw
  par_raw <- get_par_raw(pfpr_raw, pvpr_raw, pop_raw)
  # Population
  population <- aggregate_pop(pop_raw, par_raw)
  rm(pop_raw)
  # Prevalence and population at risk
  prevalence <- aggregate_prevalence(pfpr_raw, pvpr_raw, par_raw)
  rm(pvpr_raw)
  rm(pfpr_raw)
  # Interventions
  message("ITNs")
  # ITNs
  retention_half_life <- itn_retention_half_life(iso3c)
  usage_rate <- itn_usage_rate(iso3c)
  if(continent == "Africa"){
    itn_rast <- rast(list.files("data/raster/", pattern = "itn", full.names = TRUE))
    itn_raw <- long_pixel(itn_rast, pop_raster, gadm, "itn_use")
    itns <- aggregate_itn_use(itn_raw, par_raw)|>
      complete_years(2000:(wmr_year-1))
    rm(itn_rast, itn_raw)
  } else {
    itns <- prevalence_target_itns(prevalence) |>
      complete_years(2000:(wmr_year-1)) |>
      rescale_itns(population = population,
                   iso = iso3c,
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
    irs_rast <- rast(list.files("data/raster/", pattern = "irs", full.names = TRUE))
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
  tx_rast <- rast(list.files("data/raster/", pattern = "tx", full.names = TRUE))
  tx_raw <- long_pixel(tx_rast, pop_raster, gadm, "tx_cov")
  tx <- aggregate_tx(tx_raw, par_raw) |>
    tx_act(iso3c) |>
    tx_public(iso3c)
  rm(tx_rast, tx_raw)
  rm(par_raw)
  message("SMC")
  # SMC
  smc_address = "data/smc_coverage.csv"
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
  rtss_address = "data/rtss_coverage.csv"
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
  message("Vectors")
  # Vectors
  if(continent == "Africa"){
    vectors <- get_vectors_africa(pop_raster, gadm)
  } else {
    vectors <- get_vectors_not_africa(pop_raster, gadm)
  }
  vectors <- vectors |>
    group_by(ID) |>
    mutate(prop = prop / sum(prop)) |>
    ungroup() |>
    add_units(gadm_df)
  
  missing_vectors <- dplyr::anti_join(gadm_df, vectors) 
  if(nrow(missing_vectors) > 0){
    vectors_append <- missing_vectors |>
      select(-ID) |>
      mutate(species = "unknown",
             prop_na = 0,
             prop = 1,
             blood_meal_rates = median(vectors$blood_meal_rates),
             foraging_time = median(vectors$foraging_time),
             Q0 = median(vectors$Q0),
             phi_bednets = median(vectors$phi_bednets),
             phi_indoors = median(vectors$phi_indoors),
             mum = median(vectors$mum))
    vectors <- bind_rows(vectors, vectors_append)
  }
  
  message("Epi")
  # Epi
  prevalence <- prevalence |>
    add_units(gadm_df)
  cases_deaths_data <- read.csv("data/wmr_cases_deaths.csv")
  cases_deaths <- cases_deaths_data[cases_deaths_data$iso3c == iso3c, ]
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
  
  message("Population projection")
  # Population
  population <- population_projection(population, iso = iso3c, reference_year = wmr_year - 1) |>
    add_units(gadm_df)
  message("Seasonality")
  # Season
  rainfall_rast <- rast(list.files("data/raster/", pattern = "rainfall", full.names = TRUE))
  rainfall <- get_rainfall(rainfall_rast, gadm)
  seasonality <- rainfall |>
    replace_na(list(rainfall = 0)) |>
    get_season_coefs() |>
    add_units(gadm_df)
  rainfall <- rainfall |>
    add_units(gadm_df)
  message("Sites")
  # Sites
  sites <- unique(select(population, -c(year, pop, par, par_pf, par_pv)))
  message("Resistance")
  pyrethroid_resistance <- get_pyrethroid_resistance(sites)
  
  # Site file
  site_file <- list(
    country = iso3c,
    version = version_folder,
    admin_level = admin_level,
    sites = sites,
    cases_deaths = cases_deaths,
    prevalence = prevalence,
    interventions = interventions,
    population = population,
    demography = demography,
    vectors = vectors,
    pyrethroid_resistance = pyrethroid_resistance,
    seasonality = seasonality
  )
  
  if(!dir.exists(outdir)){
    dir.create(outdir)
  }
  outfile <- paste0(outdir, iso3c, ".rds")
  if(!file.exists(outfile) | overwrite){
    saveRDS(site_file, outfile)
  }
  # Diagnostics
  if(diagnostics){
    message("Diagnostics")
    diagnostic_dir <- paste0(outdir, "diagnostics/")
    if(!dir.exists(diagnostic_dir)){
      dir.create(diagnostic_dir)
    }
    
    interventions_diagnostic(site_file$interventions, site_file$population, save_address = diagnostic_dir)
    population_diagnostic(site_file$population, save_address = diagnostic_dir)
    vectors_diagnostic(site_file$vectors, save_address = diagnostic_dir)
    prevalence_diagnostic(site_file$prevalence, site_file$population, save_address = diagnostic_dir)
    demography_diagnostic(site_file$demography, save_address = diagnostic_dir)
    seasonality_diagnostic(site_file$seasonality, rainfall, save_address = diagnostic_dir)
    cases_deaths_diagnostic(site_file$cases_deaths, save_address = diagnostic_dir)
    pyrethroid_resistance_diagnostic(site_file$pyrethroid_resistance, save_address = diagnostic_dir)
  }
  
  return(site_file)
}

create_site_safe <- purrr::possibly(create_site, otherwise = NULL)