get_season_coefs <- function(rainfall){
  season_coefs <- rainfall |>
    group_by(ID) |>
    summarise(coef = as.data.frame(t(umbrella::fit_fourier(rainfall = rainfall, t = day, floor = 0.001)$coefficients))) |>
    unpack(coef)
  return(season_coefs)
}

