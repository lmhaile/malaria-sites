

orderly2::orderly_parameters(iso3c = NULL)
orderly2::orderly_description('Generate population component of site file')
orderly2::orderly_artefact('population output', 'population.rds')
orderly2::orderly_shared_resource(long_pixel= 'long_pixel.R')




population<- prepare_population(iso3c, gadm)
saveRDS(population, 'population.rds')