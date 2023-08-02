#*# * Calibrating site EIR * #*#

### Input ######################################################################
isos <- read.csv("raw_data/wmr/wmr_countries.csv") |>
  dplyr::mutate(continent = countrycode::countrycode(iso3c, "iso3c", "continent")) |>
  dplyr::filter(continent != "Africa") |>
  dplyr::pull(iso3c)
version <- "2022/v1.0/"
################################################################################

### Package and package sources ################################################
packages <- c("malariasimulation", "cali", "ggplot2", "RcppRoll")
package_load <- sapply(packages, library, character.only = TRUE)
sources <- "R/fit_site.R"
source(sources)
################################################################################

### Cluster ####################################################################
package_sources <- conan::conan_sources(c("mrc-ide/malariasimulation@dev", "mrc-ide/cali@dev"))
ctx <- context::context_save(path = "contexts/fit_eir",
                             sources = sources,
                             packages = packages,
                             package_sources = package_sources)
config <- didehpc::didehpc_config(use_workers = TRUE)
## Manually install the dependencies, so that the pkgdepends does not
## get confused:
obj <- didehpc::queue_didehpc(ctx, provision = "later")
obj$install_packages("mrc-ide/individual")
obj$install_packages("mrc-ide/malariaEquilibrium")
obj$install_packages("mrc-ide/malariasimulation@dev")
obj$install_packages("mrc-ide/cali@dev")
## Once this is done once, the queue can be used as normal:
obj <- didehpc::queue_didehpc(ctx, config = config)

workers <- obj$submit_workers(300)
# t <- obj$enqueue(sessionInfo())
# t$wait(100)
for(iso in isos){
  print(iso)
  input <- list.files(path = paste0(version, iso, "/parameters/"), pattern = "*.RDS", full.names = TRUE)
  continent <- countrycode::countrycode(iso, "iso3c", "continent")
  low <- 0.001
  high <- 1000
  tolerance <- 0.05
  if(continent != "Africa"){
    low <- 0.0001
    high <- 20
    tolerance <- 0.005
  }
  # Can check for missing
  if(FALSE){
    todo <- stringr::str_replace(list.files(path = paste0(version, iso, "/parameters/"), pattern = "*.RDS", full.names = FALSE), ".RDS", "")
    done <- stringr::str_replace(list.files(path = paste0(version, iso, "/eir_fits/"), pattern = "*.csv", full.names = FALSE), ".csv", "")
    left <- which(!todo %in% paste0("input_", done))
    input <- input[left]
  }
  
  for(par_address in input){
    p <- readRDS(par_address)
    obj$enqueue(
      fit_site(p = p,
               version = version,
               tolerance = tolerance,
               low = low,
               high = high),
      name = p$name
    )
  }
}
#tsk <- obj$task_list()
#table(obj$task_status())
#obj$task_delete(tsk)
# obj$stop_workers()
################################################################################