# sites ------------------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate sites component of site file')
orderly2::orderly_artefact('sites output', 'sites.rds')

orderly2::orderly_dependency("population",
                             "latest(parameter:iso == this:iso)",
                             c(population.rds = "population.rds"))


library(dplyr)
population<- readRDS('population.rds')


# Sites
sites <- unique(select(population, -c(year, pop, par, par_pf, par_pv)))

saveRDS(sites, 'sites.rds')

