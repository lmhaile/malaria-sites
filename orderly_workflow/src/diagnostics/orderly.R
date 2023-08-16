# site file diagnostics --------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate diagnostics for site file')
orderly2::orderly_artefact('interventions output', 'interventions.rds')

source('diagnostics.R')

interventions_diagnostic(site_file$interventions, site_file$population, save_address = diagnostic_dir)
population_diagnostic(site_file$population, save_address = diagnostic_dir)
vectors_diagnostic(site_file$vectors, save_address = diagnostic_dir)
prevalence_diagnostic(site_file$prevalence, site_file$population, save_address = diagnostic_dir)
demography_diagnostic(site_file$demography, save_address = diagnostic_dir)
seasonality_diagnostic(site_file$seasonality, rainfall, save_address = diagnostic_dir)
cases_deaths_diagnostic(site_file$cases_deaths, save_address = diagnostic_dir)
pyrethroid_resistance_diagnostic(site_file$pyrethroid_resistance, save_address = diagnostic_dir)
