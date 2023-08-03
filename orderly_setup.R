# orderly setup
# basic commands for initializing orderly workflow (no need to run more than once)



path<- 'C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/'

orderly2::orderly_init(path)



# test workflow for cases + deaths component of site file
# specify the country you would like to run this report for
id<- orderly2::orderly_run('cases_deaths', list(iso3c= 'NGA'), root= path)


id<- orderly2::orderly_run('initialize_site', list(iso3c= 'NGA'), root= path)

# read in output
test<- read_rds('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/archive/cases_deaths/20230803-212355-34605bd4/cases_deaths_output.rds')
