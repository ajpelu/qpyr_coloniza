# Generate descriptive table for croplands

library(tidyverse)
library(sf)

crop_sp <- read_sf("data/geoinfo/aux_ajpelu_cultivos_56.shp") %>%
    mutate(crop_code = case_when(
      loc == "CANAR" & cultivo == "CULT_1" ~ "CA1",
      loc == "CANAR" & cultivo == "CULT_2" ~ "CA2",
      loc == "CANAR" & cultivo == "CULT_3" ~ "CA3",
      loc == "SANJUAN" & cultivo == "CULT_1" ~ "SJ1",
      loc == "SANJUAN" & cultivo == "CULT_2" ~ "SJ2"
    ))
  
area <- crop_sp %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(crop_code, area=shape_area)
  
age <- read_csv("data/age_cultivos.csv")
transectos <- read_csv("data/transectos_crops.csv")
  
# get minimun and maximum elevation 
elev <- transectos %>% 
  group_by(crop_code) %>% 
  summarise(elev_min = min(altitud), 
            elev_max = max(altitud)) %>% 
  unite("elevation", elev_min, elev_max, sep = "-")

# Get n of transects by crop code   
ncrops <- transectos %>% 
  group_by(crop_code, tipo) %>% 
  count() %>% 
  pivot_wider(names_from = tipo, values_from = n) %>% 
  rename("Abandonment Cropland"="CLARO",
         "Edge" = "CLARO_BORDE", 
         "Forest" = "ROBLEDAL")
  
tabla_descriptivos_crops <- age %>% 
  dplyr::select(-code, -cultivo, -subtipo) %>% 
  inner_join(elev) %>% 
  inner_join(area) %>% 
  inner_join(ncrops) %>% 
  mutate(area = round(area/10000, 2))
  
write_csv(out, "data/tabla_descriptivos_crops.csv")
 


