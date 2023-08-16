# orderly setup
# basic commands for initializing orderly workflow (no need to run more than once)
# orderly create sites workflow ------------------------------------------------
#orderly2::orderly_init(path)

# filepath for workflow
path<- 'C:/Users/lhaile/Documents/malaria-sites/orderly_workflow/'
setwd(path)
source('diagnostics.R')
#packages
packages <- c("terra", "sf", "dplyr", "tidyr", "countrycode", "purrr",
              "peeps", "netz", "umbrella", "ggplot2", "patchwork", "lubridate")
package_load <- sapply(packages, library, character.only = TRUE)

# specify input parameter (country code)
code<- 'NGA'

for (code in codes){
  
  # components of site file
  orderly2::orderly_run('cases_deaths', list(iso= code), root = path)       
  orderly2::orderly_run('spatial_data', list(iso= code), root = path)       
  orderly2::orderly_run('population', list(iso= code), root = path)         
  orderly2::orderly_run('prevalence', list(iso= code), root = path)         
  orderly2::orderly_run('sites', list(iso= code), root = path)              
  orderly2::orderly_run('vectors', list(iso= code), root = path)             
  orderly2::orderly_run('seasonality', list(iso= code), root = path)        
  orderly2::orderly_run('resistance', list(iso= code), root = path)      
  
  # compile site file and diagnostics
  orderly2::orderly_run('compile_site', list(iso= code), root = path)      
  
}

