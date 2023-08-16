# orderly setup
# basic commands for initializing orderly workflow (no need to run more than once)
path<- 'C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/'
setwd(path)
library(dplyr)
library(orderly2)
library(sjmisc)

packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

#orderly2::orderly_init(path)



# read in output
test<- read_rds('C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/archive/cases_deaths/20230803-212355-34605bd4/cases_deaths_output.rds')




# test each orderly report for site
id<- orderly2::orderly_run('cases_deaths', list(iso= 'NGA'), root = path)       # works
id<- orderly2::orderly_run('spatial_data', list(iso= 'NGA'), root = path)       # works
id<- orderly2::orderly_run('population', list(iso= 'NGA'), root = path)         # works
id<- orderly2::orderly_run('prevalence', list(iso= 'NGA'), root = path)         
id<- orderly2::orderly_run('sites', list(iso= 'NGA'), root = path)              # works
id<- orderly2::orderly_run('vectors', list(iso= 'NGA'), root = path)        
id<- orderly2::orderly_run('seasonality', list(iso= 'NGA'), root = path)        

