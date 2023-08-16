# resistance  ------------------------------------------------------------------

orderly2::orderly_parameters(iso = NULL)
orderly2::orderly_description('Generate resistance component of site file')
orderly2::orderly_artefact('resistance output', 'resistance.rds')

orderly2::orderly_dependency("sites",
                             "latest(parameter:iso == this:iso)",
                             c(sites.rds = "sites.rds"))


source('get_pyrethroid_resistance.R')


readRDS('sites.RDS')

pyrethroid_resistance <- get_pyrethroid_resistance(sites)


saveRDS(pyrethroid_resistance, 'resistance.rds')