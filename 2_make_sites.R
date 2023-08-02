#*# * Create site files * #*#

### Inputs #####################################################################
version = "2023/v1.0/"
admin_level <- 1
wmr_year <- 2021
library(countrycode)
isos <- read.csv("raw_data/wmr/wmr_countries.csv") |>
  dplyr::mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) |>
  dplyr::pull(iso3c)
isos <- setdiff(isos, c("BRA", "IND"))
################################################################################

### Package and package sources ################################################
packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)
options(dplyr.summarise.inform = FALSE)
sources <- c("R/aggregations.R", "R/spatial.R", "R/itns.R", "R/irs.R", "R/season.R",
             "R/season.R", "R/population.R", "R/vectors.R", "R/create_site.R", 
             "R/interpolation.R", "R/diagnostics.R", "R/treatment.R")
source_load <- sapply(sources, source)
################################################################################

### Cluster submission #########################################################
package_sources <- conan::conan_sources(c("mrc-ide/peeps",
                                          "mrc-ide/netz",
                                          "mrc-ide/umbrella"))
ctx <- context::context_save(path = "contexts/site_file",
                             sources = sources,
                             packages = packages,
                             package_sources = package_sources)
config <- didehpc::didehpc_config()
obj <- didehpc::queue_didehpc(ctx, config = config)

#test on one ISO
iso<- 'MWI'
isos<- 'MWI'
for(iso in isos){
  gadm <- get_gadm(iso3c = iso, 
                   admin_level = admin_level)
  if(iso == "VNM"){
    gadm <- get_gadm(iso3c = iso, admin_level = admin_level, name1_overwrite = "VARNAME_1")
  }
  
  obj$enqueue(
    create_site(iso3c = iso,
                gadm = gadm,
                wmr_year = wmr_year,
                admin_level = admin_level,
                version_folder = version,
                overwrite = FALSE),
    name = paste("Create site", iso)
  )
  Sys.sleep(2)
}


# Brasil and India both need RAM:
config <- didehpc::didehpc_config(cores = 4) 
obj <- didehpc::queue_didehpc(ctx, config = config)

iso <- "BRA"
gadm <- get_gadm(iso3c = iso, admin_level = admin_level)
obj$enqueue(
  create_site(iso3c = iso,
              gadm = gadm,
              wmr_year = wmr_year,
              admin_level = admin_level,
              version_folder = version,
              overwrite = TRUE),
  name = paste("Create site", iso)
)

iso <- "IND"
admin_level <- 2
gadm <- get_gadm(iso3c = iso, admin_level = admin_level)
obj$enqueue(
  create_site(iso3c = iso,
              gadm = gadm,
              wmr_year = wmr_year,
              admin_level = admin_level,
              version_folder = version,
              overwrite = TRUE),
  name = paste("Create site", iso)
)
################################################################################

### NA checking ################################################################
na_isos <- list.files(version)
for(iso in na_isos){
  sf <- readRDS(paste0(version, iso, "/", iso, ".RDS"))
  nas <- sapply(sf, function(x){
    sum(is.na(x))
  })
  if(sum(nas) > 0){
    print(iso)
    print(nas[nas>0])
  }
}
################################################################################
