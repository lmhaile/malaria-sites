#*# * Creating site input parameters * #*#

### Inputs #####################################################################
isos <- read.csv("raw_data/wmr/wmr_countries.csv") |>
  dplyr::mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) |>
  dplyr::filter(continent != "Africa") |>
  dplyr::pull(iso3c)
version <- "2023/v1.0/"
test_dir<- 'M:/Lydia/malaria_sites/'
################################################################################

### Package and package sources ################################################
packages <- c("site")
package_load <- sapply(packages, library, character.only = TRUE)
################################################################################
iso<- 'MWI'
### Create site parameters #####################################################
for(iso in isos){
  print(iso)
  
  dir <- paste0(test_dir, version, iso, "/parameters/")
  if(!dir.exists(dir)){
    dir.create(dir, recursive = T)
  }
  
  site_file <- readRDS(paste0(version, iso, "/", iso, ".RDS"))
  site_file$eir <- data.frame()
  # Create input parameters and prevalence targets for each site
  for(i in 1:nrow(site_file$sites)){
    site <- site:::single_site(site_file, i)
    
    if(sum(site$prevalence$pfpr) > 0){
      params_pf <- site_parameters(
        interventions = site$interventions,
        demography = site$demography,
        vectors = site$vectors,
        seasonality = site$seasonality,
        species = "pf",
        eir = NULL,
        overrides = list(human_population = 50000),
        burnin = 10
      )
      params_pf$site <- site$sites
      params_pf$target <- site$prevalence$pfpr
      params_pf$timesteps <- (10 + length(params_pf$target)) * 365
      params_pf$site$spp <- "pf"
      params_pf$name <- paste0(params_pf$site[,-1], collapse = "_")
      saveRDS(params_pf, paste0(dir, "/input_", params_pf$name, ".RDS"))
    }
    if(sum(site$prevalence$pvpr) > 0){
      params_pv <- site_parameters(
        interventions = site$interventions,
        demography = site$demography,
        vectors = site$vectors,
        seasonality = site$seasonality,
        species = "pv",
        eir = NULL,
        overrides = list(human_population = 50000),
        burnin = 10
      )
      params_pv$site <- site$sites
      params_pv$target <- site$prevalence$pvpr
      params_pv$timesteps <- (10 + length(params_pv$target)) * 365
      params_pv$site$spp <- "pv"
      params_pv$name <- paste0(params_pv$site[,-1], collapse = "_")
      saveRDS(params_pv, paste0(dir, "/input_", params_pv$name, ".RDS"))
    }
  }
}
################################################################################


