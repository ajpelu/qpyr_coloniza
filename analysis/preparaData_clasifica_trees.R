# Classify tree data 
library("tidyverse")

raw_data <- read_csv("data/migrame_observations.csv")

# filter data with NA in altura and NA in diametro base 
raw_data <- raw_data %>% 
  filter(!(is.na(altura) & is.na(diametro_base))) %>% 
  mutate(diametro_base = replace_na(diametro_base, 0))



## Compute tree class based on Plieninger et al. 
# seedling_small h < 30 
# seedling_large 30 < h < 130 
# sapling_small h > 130 & dbh < 2.5 
# sapling_large h > 130 & dbh >= 2.5 
# tree_small 7.5 < dbh < 12.5 
# tree_medium 12.5 =< dbh < 22.5 
# tree_large dbh >= 22.5    




# Add variable of tree_class 
raw_data$tree_class <- ifelse(raw_data$diametro_base >= 22.5, "tree_large",
                             ifelse(raw_data$diametro_base >= 12.5, "tree_medium",
                                    ifelse(raw_data$diametro_base >= 7.5, "tree_small",
                                           ifelse(raw_data$diametro_base >= 2.5, "sapling_large",
                                                  ifelse(raw_data$diametro_base < 2.5 & raw_data$altura >= 130, "sapling_small",
                                                         ifelse(raw_data$diametro_base < 2.5 & raw_data$altura >= 30, "seedling_large", "seedling_small")
                                                  )
                                           )
                                    )
                             )
)

# Add variable of tree type (adult or juvenile)
raw_data$tree_type <- ifelse(raw_data$tree_class %in% c("tree_large", "tree_medium", "tree_small"),
  "adult", "juvenile"
)

# classify each 30 cm up to 150 and every 250 cm 
raw_data <- raw_data %>% 
  mutate(size_category = cut(altura, 
                             breaks = c(-Inf, seq(30,150, by=30), seq(500,2000,250)),
                             labels = c(paste0("size",seq(30,150,30)),
                                        paste0("size",seq(500,2000,250)))
                             ))

# Use only data for our transects 
transectos_crops <- read_csv("data/transectos_crops.csv")


tree_data_crops <- raw_data %>% 
  filter(nombre %in% transectos_crops$nombre) %>% 
  inner_join(
    (transectos_crops %>% 
       dplyr::select(nombre, crop_code, localidad, tipo))
  )

write_csv(tree_data_crops, "data/tree_data_crops.csv")





