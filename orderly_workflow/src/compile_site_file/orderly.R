# compile sites and diagnostics-------------------------------------------------
orderly2::orderly_parameters(iso = NULL,
                             admin_level = 1)
orderly2::orderly_description('Compile final site file for country and output diagnostics')
orderly2::orderly_artefact('final output', 'site_file.rds')

orderly2::orderly_dependency("sites",
                             "latest(parameter:iso == this:iso)",
                             c(sites.rds = "sites.rds"))
orderly2::orderly_dependency("cases_deaths",
                             "latest(parameter:iso == this:iso)",
                             c(sites.rds = "sites.rds"))
orderly2::orderly_dependency("prevalence",
                             "latest(parameter:iso == this:iso)",
                             c(prevalence.rds = "prevalence.rds"))
orderly2::orderly_dependency("interventions",
                             "latest(parameter:iso == this:iso)",
                             c(interventions.rds = "interventions.rds"))
orderly2::orderly_dependency("population",
                             "latest(parameter:iso == this:iso)",
                             c(population.rds = "population.rds"))
orderly2::orderly_dependency("demography",
                             "latest(parameter:iso == this:iso)",
                             c(demography.rds = "demography.rds"))
orderly2::orderly_dependency("vectors",
                             "latest(parameter:iso == this:iso)",
                             c(vectors.rds = "vectors.rds"))
orderly2::orderly_dependency("resistance",
                             "latest(parameter:iso == this:iso)",
                             c(resistance.rds = "resistance.rds"))
# orderly2::orderly_dependency("seasonality",
#                              "latest(parameter:iso == this:iso)",
#                              c(seasonality.rds = "seasonality.rds"))

sites<- readRDS('sites.rds')
population<- readRDS('population.rds')
prevalence<- readRDS('prevalence.rds')
cases_deaths<- readRDS('cases_deaths.rds')
interventions<- readRDS('interventions.rds')
pyrethroid_resistance<- readRDS('pyrethroid_resistance.rds')
demography<- readRDS('demography.rds')
#seasonality<- readRDS('seasonality.rds')


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
  #seasonality = seasonality
)


saveRDS('site_file.rds')

# diagnostics -----------------------------------------------------------------
diagnostic_dir<- 'C:/Users/lhaile/Documents/malaria-sites/test_plots/'
interventions_diagnostic(site_file$interventions, site_file$population, save_address = diagnostic_dir)
population_diagnostic(site_file$population, save_address = diagnostic_dir)
vectors_diagnostic(site_file$vectors, save_address = diagnostic_dir)
prevalence_diagnostic(site_file$prevalence, site_file$population, save_address = diagnostic_dir)
demography_diagnostic(site_file$demography, save_address = diagnostic_dir)
#seasonality_diagnostic(site_file$seasonality, rainfall, save_address = diagnostic_dir)
cases_deaths_diagnostic(site_file$cases_deaths, save_address = diagnostic_dir)
pyrethroid_resistance_diagnostic(site_file$pyrethroid_resistance, save_address = diagnostic_dir)
