


orderly2::orderly_parameters(iso3c = NULL)
orderly2::orderly_description('Generate cases and deaths component of site file')
orderly2::orderly_artefact('cases and deaths output', 'cases_deaths_output.rds')

cases_deaths_data <- read.csv("wmr_cases_deaths.csv")
cases_deaths <- cases_deaths_data[cases_deaths_data$iso3c == iso3c, ]
  
  
  saveRDS(cases_deaths, 'cases_deaths_output.rds')

  


