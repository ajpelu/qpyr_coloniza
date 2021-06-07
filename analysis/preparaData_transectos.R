# Prepara datos cultivos 
# Read transects data and assign crop code 
library("tidyverse")

transectos <- read_csv("data/info_replicates_transect.csv")
transectos_crops <- 
  transectos %>% 
    mutate(crop_code = case_when(
      localidad == "CANAR" & replicate == "R1" ~ "CA1",
      localidad == "CANAR" & replicate == "R2" ~ "CA2",
      localidad == "CANAR" & replicate == "R3" ~ "CA3",
      localidad == "SANJUAN" & replicate == "R1" ~ "SJ1",
      localidad == "SANJUAN" & replicate == "R2" ~ "SJ2"
    )) 
  
write_csv(transectos_crops, "data/transectos_crops.csv")
