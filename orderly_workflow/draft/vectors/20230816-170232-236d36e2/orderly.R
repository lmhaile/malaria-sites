# vectors ----------------------------------------------------------------------
#setwd('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/src/vectors/')

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate vectors component of site file')
orderly2::orderly_artefact('vectors output', 'vectors.rds')

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm.rds = "gadm.rds"))

orderly2::orderly_dependency("spatial_data",
                             "latest(parameter:iso == this:iso)",
                             c(gadm_df.rds = "gadm_df.rds"))

pop_raster <- rast(list.files("M:/Pete/malaria_sites/data/raster/",  
                              pattern = paste0("population_", iso), 
                              full.names = TRUE))

gadm<- readRDS('gadm.rds')
gadm_df<- readRDS('gadm_df.rds')
source('get_vectors_africa.R')
source('get_vectors_not_africa.R')

packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

continent <- countrycode(iso, "iso3c", "continent")

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

saveRDS(vectors, 'vectors.RDS')