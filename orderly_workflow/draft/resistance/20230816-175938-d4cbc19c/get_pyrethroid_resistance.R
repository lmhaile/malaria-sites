get_pyrethroid_resistance <- function(sites, pyrethroid_resistance_address = "pyrethroid_resistance.csv"){
  pr <- read.csv(pyrethroid_resistance_address)
  
  pyrethroid_resistance <- sites |>
    left_join(data.frame(year = 2000:2050), by = character()) |>
    left_join(pr, by = c("iso3c", "name_1", "year")) |>
    group_by(year) |>
    mutate(pyrethroid_resistance = ifelse(is.na(pyrethroid_resistance), median(pyrethroid_resistance, na.rm = TRUE), pyrethroid_resistance )) |>
    ungroup() |>
    replace_na(list(pyrethroid_resistance = 0))
  
  return(pyrethroid_resistance)
}