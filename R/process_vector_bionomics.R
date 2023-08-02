library(dplyr)


vector_bionomics_raw <- read.csv("raw_data/vector_bionomics/processed_species_bionomics.csv")
vector_bionomics_other <- vector_bionomics_raw |>
  filter(!is.na(species),
         species != "Group",
         species != "arabiensis") |>
  rename(phi_indoors = indoor_biting,
         phi_bednets = inbed_biting,
         Q0 = anthropophagy) |>
  mutate(blood_meal_rates = malariasimulation::gamb_params$blood_meal_rates,
         foraging_time = malariasimulation::gamb_params$foraging_time,
         mum = malariasimulation::gamb_params$mum) |>
  select(species, blood_meal_rates, foraging_time, 
         Q0, phi_bednets, phi_indoors, mum)

vector_bionomics_ssa <- bind_rows(
  malariasimulation::arab_params,
  malariasimulation::gamb_params,
  malariasimulation::fun_params
) |>
  mutate(species = c("arabiensis", "gambiae", "funestus"))

vector_bionomics <- bind_rows(vector_bionomics_ssa, vector_bionomics_other)

write.csv(vector_bionomics, "data/vector_bionomics.csv", row.names = FALSE)
