fit_site <- function(p, version, tolerance = 0.1, low = 0.001, high = 1000){
  print(p$name)
  
  iso <- p$site$iso3c
  target <- p$target
  
  if(p$site$spp == "pf"){
    # Run the calibration
    c1 <- calibrate(
      parameters = p,
      target = target,
      summary_function = annual_pfpr_summary,
      tolerance = tolerance,
      weights = c(rep(0, 10), seq(0.1, 1, 0.1)),
      low = low, high = high,
      maxiter = 20
    )
  }
  
  if(p$site$spp == "pv"){
    # Run the calibration
    c1 <- calibrate(
      parameters = p,
      target = target,
      summary_function = annual_pvpr_summary,
      tolerance = tolerance,
      weights = c(rep(0, 10), seq(0.1, 1, 0.1)),
      low = low, high = high,
      maxiter = 20
    )
  }
  
  # Run the diagnostic
  p <- p |>
    malariasimulation::set_equilibrium(init_EIR = c1)
  sim <- malariasimulation::run_simulation(p$timesteps, p)
  # Throw away burn in
  sim <- sim[sim$timestep > (p$burnin * 365),]
  sim$timestep <- sim$timestep - p$burnin * 365
  
  if(p$site$spp == "pf"){
    pd_sim <- data.frame(timestep = sim$timestep,
                         prevalence = sim$n_detect_730 / sim$n_730_3649)
  }
  if(p$site$spp == "pv"){
    pd_sim <- data.frame(timestep = sim$timestep,
                         prevalence = sim$n_detect_365_36499 / sim$n_365_36499)
  }
  
  pd_sim$smooth_prevalence <- RcppRoll::roll_mean(pd_sim$prevalence, n = 365, align = "center", fill = NA)
  pd_target <- data.frame(timestep = 365 * (0:19 + 0.5),
                          prevalence = target)
  
  diagnostic <- ggplot() + 
    geom_line(data = pd_sim, aes(x = timestep, y = prevalence)) +
    geom_line(data = pd_sim, aes(x = timestep, y = smooth_prevalence), col = "magenta4", size = 1) +
    geom_point(data = pd_target, aes(x = timestep, y = prevalence), col = "springgreen2", size = 3) +
    scale_x_continuous(breaks = 0:20 * 365, labels = 2000 + 0:20, name = "Year") +
    ylab(paste(p$site$spp, "prevalence")) +
    theme_bw() +
    ggtitle(paste(p$name, signif(c1, 5)))
  
  diagnostic_dir <- paste0(version, iso, "/diagnostics/")
  diagnostic_save_name <- paste0(diagnostic_dir, "calibration_", p$name, ".png")
  print(diagnostic_save_name)
  ggsave(diagnostic_save_name, diagnostic, height = 7, width = 10)
  
  # Save the fit
  output_dir <- paste0(version, iso, "/eir_fits/")
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  output <- p$site
  output[,"eir"] <- c1
  
  write.csv(output, paste0(output_dir, p$name, ".csv"), row.names = FALSE)
}

annual_pfpr_summary <- function(x){
  year <- ceiling(x$timestep / 365)
  pfpr <- x$n_detect_730_3649 / x$n_730_3649
  annual_pfpr <- tapply(pfpr, year, mean)
  # Ignore burnin
  annual_pfpr <- annual_pfpr[-(1:10)]
  # We need to penalise so that low-transmission areas aren't fitted as no-transmission
  if(annual_pfpr[length(annual_pfpr)] == 0){
    annual_pfpr[length(annual_pfpr)] <- -5
  }
  return(annual_pfpr)
}

annual_pvpr_summary <- function(x){
  year <- ceiling(x$timestep / 365)
  pvpr <- x$n_detect_365_36499 / x$n_365_36499
  annual_pvpr <- tapply(pvpr, year, mean)
  # Ignore burnin
  annual_pvpr <- annual_pvpr[-(1:10)]
  # We need to penalise so that low-transmission areas aren't fitted as no-transmission
  if(annual_pvpr[length(annual_pvpr)] == 0){
    annual_pvpr[length(annual_pvpr)] <- -5
  }
  return(annual_pvpr)
}