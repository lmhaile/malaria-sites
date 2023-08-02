prepare_sites<- function(population){
  
  # Sites
  sites <- unique(select(population, -c(year, pop, par, par_pf, par_pv)))
  
  return(sites)
}
