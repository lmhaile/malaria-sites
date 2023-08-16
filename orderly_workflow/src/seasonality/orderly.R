# seasonality ------------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate seasonality component of site file')
orderly2::orderly_artefact('seasonality output', 'seasonality.rds')

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm.rds = "gadm.rds"))
orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm_df.rds = "gadm_df.rds"))

source('get_rainfall.R')
source('mean2.R')
source('get_season_coefs.R')
source('add_units.R')

packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

# Season
rainfall_rast <- rast(list.files("M:/Pete/malaria_sites/data/raster/", 
                                 pattern = "rainfall", full.names = TRUE))
rainfall <- get_rainfall(rainfall_rast, gadm)
seasonality <- rainfall |>
  replace_na(list(rainfall = 0)) |>
  get_season_coefs() |>
  add_units(gadm_df)
rainfall <- rainfall |>
  add_units(gadm_df)


saveRDS(seasonality, 'seasonality.rds')
