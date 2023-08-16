# compile sites ----------------------------------------------------------------
orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Compile final site file for country')
orderly2::orderly_artefact('final output', 'site_file.rds')

orderly2::orderly_dependency("population",
                             "latest(parameter:iso == this:iso)",
                             c(population.rds = "population.rds"))
orderly2::orderly_dependency("prevalence",
                             "latest(parameter:iso == this:iso)",
                             c(prevalence.rds = "prevalence.rds"))
orderly2::orderly_dependency("resistance",
                             "latest(parameter:iso == this:iso)",
                             c(resistance.rds = "resistance.rds"))
orderly2::orderly_dependency("interventions",
                             "latest(parameter:iso == this:iso)",
                             c(interventions.rds = "interventions.rds"))
orderly2::orderly_dependency("sites",
                             "latest(parameter:iso == this:iso)",
                             c(sites.rds = "sites.rds"))
orderly2::orderly_dependency("vectors",
                             "latest(parameter:iso == this:iso)",
                             c(vectors.rds = "vectors.rds"))
orderly2::orderly_dependency("seasonality",
                             "latest(parameter:iso == this:iso)",
                             c(seasonality.rds = "seasonality.rds"))
orderly2::orderly_dependency("demography",
                             "latest(parameter:iso == this:iso)",
                             c(demography.rds = "demography.rds"))

# Format site file
site_file <- list(
  country = iso,
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


saveRDS('site_file.rds')