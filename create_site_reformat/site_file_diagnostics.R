site_file_diagnostics<- function(diagnostics){
  
  # Diagnostics
  if(diagnostics){
    message("Diagnostics")
    diagnostic_dir <- paste0(outdir, "diagnostics/")
    if(!dir.exists(diagnostic_dir)){
      dir.create(diagnostic_dir)
    }
    
    interventions_diagnostic(site_file$interventions, site_file$population, save_address = diagnostic_dir)
    population_diagnostic(site_file$population, save_address = diagnostic_dir)
    vectors_diagnostic(site_file$vectors, save_address = diagnostic_dir)
    prevalence_diagnostic(site_file$prevalence, site_file$population, save_address = diagnostic_dir)
    demography_diagnostic(site_file$demography, save_address = diagnostic_dir)
    seasonality_diagnostic(site_file$seasonality, rainfall, save_address = diagnostic_dir)
    cases_deaths_diagnostic(site_file$cases_deaths, save_address = diagnostic_dir)
    pyrethroid_resistance_diagnostic(site_file$pyrethroid_resistance, save_address = diagnostic_dir)
  }
  
  
  
}


interventions_diagnostic <- function(interventions, population, facet_size = 3, save_address = NULL){
  
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(interventions)]
  interventions$id <- apply(interventions[,group], 1, paste, collapse = "_")
  
  pd_sub <- interventions |> 
    select(id, country, iso3c, any_of(group), urban_rural, year,
           itn_use, irs_cov, tx_cov, smc_cov, rtss_cov, pmc_cov, itn_input_dist) |>
    tidyr::pivot_longer(c(itn_use, irs_cov, tx_cov, smc_cov, rtss_cov, pmc_cov, itn_input_dist),
                        names_to = "intervention", values_to = "cov")
  
  interventions_plot_sub <- ggplot(pd_sub, aes(x = year,
                                               y = cov,
                                               col = intervention,
                                               lty = urban_rural)) +
    geom_line() +
    facet_wrap(~ id, ncol = 6) +
    theme_bw()
  
  pd_country <- pd_sub |>
    left_join(population) |>
    group_by(year, intervention) |>
    summarise(cov = weighted.mean(cov, par))
  
  interventions_plot <- ggplot(pd_country, aes(x = year,
                                               y = cov,
                                               col = intervention)) +
    geom_line() +
    theme_bw()
  
  n_facets <- length(unique(interventions$id))
  rows <- ceiling(n_facets / 6)
  pheight <- min(rows * facet_size, 100)
  pwidth <- min(min(n_facets, 6) * facet_size, 100)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "interventions_plot_sub.png"),
           interventions_plot_sub, height = pheight, width = pwidth, limitsize = FALSE)
    ggsave(paste0(save_address, "interventions_plot.png"),
           interventions_plot, height = facet_size, width = facet_size * 1.5, limitsize = FALSE)
  }
  return(NULL)
}

population_diagnostic <- function(population, un_pop_address = "data/un_pop.csv",
                                  save_address = NULL, facet_size = 3.5){
  
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(population)]
  population$id <- apply(population[,group], 1, paste, collapse = "_")
  
  iso <- population$iso3c[1]
  
  un <- read.csv(un_pop_address) |>
    filter(iso3c == iso) |>
    rename(un_pop = pop)
  
  population <- population |>
    group_by(id, across(any_of(c("name_1", "name_2"))), urban_rural, year) |>
    summarise(
      pop = sum(pop),
      par = sum(par),
      par_pf = sum(par_pf),
      par_pv = sum(par_pv)
    )
  
  p1 <- ggplot(population, aes(x = year, y = pop, col = urban_rural)) +
    geom_line() +
    ylab("population") +
    scale_color_manual(values = c("green", "brown"), name = "") +
    facet_wrap(~ id, scales = "free_y", ncol = 6) +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  p2 <- ggplot(population, aes(x = year, y = par, col = urban_rural)) +
    geom_line() +
    ylab("population at risk (pf or pv") +
    scale_color_manual(values = c("green", "brown"), name = "") +
    facet_wrap(~ id, scales = "free_y", ncol = 6) +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  p3 <- ggplot(population, aes(x = year, y = par_pf, col = urban_rural)) +
    geom_line() +
    ylab("population at risk (pf") +
    scale_color_manual(values = c("green", "brown"), name = "") +
    facet_wrap(~ id, scales = "free_y", ncol = 6) +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  p4 <- ggplot(population, aes(x = year, y = par_pv, col = urban_rural)) +
    geom_line() +
    ylab("population at risk (pv") +
    scale_color_manual(values = c("green", "brown"), name = "") +
    facet_wrap(~ id, scales = "free_y", ncol = 6) +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  n_facets <- nrow(unique(population[,group]))
  rows <- ceiling(n_facets / 6)
  pheight <- min(rows * facet_size, 100)
  pwidth <- min(min(n_facets, 6) * facet_size, 100)
  
  population <- population |>
    group_by(year) |>
    summarise(
      pop = sum(pop),
      par = sum(par),
      par_pf = sum(par_pf),
      par_pv = sum(par_pv)
    )
  
  p5 <- ggplot() +
    geom_line(data = population, aes(x = year, y = pop)) +
    geom_point(data = un, aes(x = year, y = un_pop), col = "deeppink", size = 0.2) +
    ylab("population") +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "population.png"), p1, width = pwidth, height = pheight, limitsize = FALSE)
    ggsave(paste0(save_address, "population_at_risk.png"), p2, width = pwidth, height = pheight, limitsize = FALSE)
    ggsave(paste0(save_address, "population_at_risk_pf.png"), p3, width = pwidth, height = pheight, limitsize = FALSE)
    ggsave(paste0(save_address, "population_at_risk_pv.png"), p4, width = pwidth, height = pheight, limitsize = FALSE)
    ggsave(paste0(save_address, "population_total.png"), p5, width = facet_size, height = facet_size, limitsize = FALSE)
  }
  
  return(NULL)
}

vectors_diagnostic <- function(vectors, grouping, save_address = NULL, facet_size = 3){
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(vectors)]
  vectors$id <- apply(vectors[,group], 1, paste, collapse = "_")
  
  pd <- vectors |>
    dplyr::select(cols = -c(blood_meal_rates, foraging_time, 
                            Q0, phi_bednets, phi_indoors, mum))
  
  p <- ggplot(vectors, aes(x = id, y = prop, fill = species)) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  pwidth <- min(nrow(pd) / length(unique(vectors$species)) * (facet_size / 3), 100)
  pheight <- min(facet_size, 100)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "vectors.png"), p, width = pwidth, height = pheight, limitsize = FALSE)
  }
  
  return(NULL)
}

prevalence_diagnostic <- function(epi, population, save_address = NULL, facet_size = 3){
  
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(epi)]
  epi$id <- apply(epi[,group], 1, paste, collapse = "_")
  
  pd_sub <- epi |> 
    select(id, country, iso3c, any_of(group), urban_rural, year,
           pfpr, pvpr) |>
    tidyr::pivot_longer(c(pfpr, pvpr),
                        names_to = "species", values_to = "prevalence")
  
  prevalence_plot_sub <- ggplot(pd_sub, aes(x = year,
                                            y = prevalence,
                                            col = species,
                                            lty = urban_rural)) +
    geom_line() +
    facet_wrap(~ id, ncol = 6) +
    theme_bw()
  
  pd_country <- pd_sub |>
    left_join(population) |>
    group_by(year, species) |>
    summarise(prevalence = weighted.mean(prevalence, par))
  
  prevalence_plot <- ggplot(pd_country, aes(x = year,
                                            y = prevalence,
                                            col = species)) +
    geom_line() +
    theme_bw()
  
  n_facets <- length(unique(epi$id))
  rows <- ceiling(n_facets / 6)
  pheight <- min(rows * facet_size, 100)
  pwidth <- min(min(n_facets, 6) * facet_size, 100)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "prevalence_plot_sub.png"),
           prevalence_plot_sub, height = pheight, width = pwidth, limitsize = FALSE)
    ggsave(paste0(save_address, "prevalence_plot.png"),
           prevalence_plot, height = facet_size, width = facet_size * 1.5, limitsize = FALSE)
  }
  return(NULL)
}

demography_diagnostic <- function(demography, save_address = NULL, facet_size = 3){
  
  p <- ggplot(demography, aes(x = age_upper, group = age_upper, y = mortality_rate)) +
    geom_boxplot(coef = 100) +
    xlim(0, 100) +
    ylab("Mortality rate") + 
    xlab("Upper bound of age group") +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "demography.png"), p, height = facet_size, width = facet_size, limitsize = FALSE)
  }
  
  return(NULL)
}

seasonality_diagnostic <- function(seasonality, rainfall, save_address = NULL, facet_size = 3){
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(seasonality)]
  seasonality$id <- apply(seasonality[,group], 1, paste, collapse = "_")
  rainfall$id <- apply(rainfall[,group], 1, paste, collapse = "_")
  
  predict <- apply(seasonality[,c("id", "g0", "g1", "g2", "g3", "h1", "h2", "h3")], 1, function(x){
    p <- fourier_predict(as.numeric(x[-1]), 1:365, floor = 0.001)
    p$id <- x[1]
    return(p)
  }) |>
    bind_rows()
  
  p <- ggplot() + 
    geom_point(data = rainfall, aes(x = day, y = rainfall), alpha = 0.2) +
    geom_line(data = predict, aes(x = t, y = profile), col = "deeppink") +
    facet_wrap(~ .data[["id"]], ncol = 6) +
    theme_bw() +
    theme(aspect.ratio = 1)
  
  n_facets <- length(unique(seasonality$id))
  rows <- ceiling(n_facets / 6)
  pheight <- min(rows * facet_size, 100)
  pwidth <- min(min(n_facets, 6) * facet_size, 100)
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "seasonality.png"), p, height = pheight, width = pwidth, limitsize = FALSE)
  }
  
  return(NULL)
}

cases_deaths_diagnostic <- function(cases_deaths, save_address = NULL, facet_size = 3){
  cases_plot <- ggplot(cases_deaths, 
                       aes(x = year, y = wmr_cases,
                           ymin = wmr_cases_l, ymax = wmr_cases_u)) +
    geom_linerange() + 
    geom_point() +
    ylab("WMR cases") +
    xlab("Year") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    theme_bw()
  deaths_plot <- ggplot(cases_deaths, 
                        aes(x = year, y = wmr_deaths,
                            ymin = wmr_deaths_l, ymax = wmr_deaths_u)) +
    geom_linerange() + 
    geom_point() +
    ylab("WMR deaths") +
    xlab("Year") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    theme_bw()
  incidence_plot <- ggplot(cases_deaths, 
                           aes(x = year, y = incidence,
                               ymin = incidence_l, ymax = incidence_u)) +
    geom_linerange() + 
    geom_point() +
    ylab("WMR clinical incidence\n(cases per person per year)") +
    xlab("Year") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    theme_bw()
  mortality_plot <- ggplot(cases_deaths, 
                           aes(x = year, y = mortality,
                               ymin = mortality_l, ymax = mortality_u)) +
    geom_linerange() + 
    geom_point() +
    ylab("WMR mortality rate\n(deaths per person per year)") +
    xlab("Year") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    theme_bw()
  
  cases_deaths_plot <- (cases_plot / incidence_plot) | (deaths_plot / mortality_plot) 
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "cases_deaths.png"), cases_deaths_plot,
           height = facet_size * 2, width = facet_size * 2.5, limitsize = FALSE)
  }
}

pyrethroid_resistance_diagnostic <- function(resistance, save_address = NULL, facet_size = 3){
  group <- c("name_1", "name_2")
  group <- group[group %in% colnames(resistance)]
  resistance$id <- apply(resistance[,group], 1, paste, collapse = "_")
  
  pyrethroid_resistance_plot <- ggplot(resistance, aes(x = year, y = pyrethroid_resistance, group = id)) +
    geom_line() +
    theme_bw()
  
  if(!is.null(save_address)){
    ggsave(paste0(save_address, "pyrethroid_resistance.png"), pyrethroid_resistance_plot,
           height = facet_size, width = facet_size, limitsize = FALSE)
  }
}