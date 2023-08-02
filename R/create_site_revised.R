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
  population<- prepare_population(iso3c, gadm)

  message("Prevalence")
  prevalence<- prepare_prevalence(iso3c, gadm)

  message("Interventions")
  interventions<- prepare_interventions(iso3c, gadm)
  
  message("Vectors")
  vectors<- prepare_vectors(iso3c, gadm)
  
  message("Cases and deaths")
  cases_deaths<- prepare_cases_deaths(iso3c)

  message("Sites")
  sites<- prepare_sites(population)
  
  message("Resistance")
  pyrethroid_resistance <- get_pyrethroid_resistance(sites)
  
  message("Seasonality")
  seasonality<- prepare_seasonality(iso3c, gadm)
  
  # Format site file
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

  site_file_diagnostics()
  
  return(site_file)
}

create_site_safe <- purrr::possibly(create_site, otherwise = NULL)