
prepare_cases_deaths<- function(iso3c, gadm){
  
  cases_deaths_data <- read.csv("M:/Lydia/malaria_sites/data/wmr_cases_deaths.csv")
  cases_deaths <- cases_deaths_data[cases_deaths_data$iso3c == iso3c, ]
  
  return(cases_deaths)
  
}