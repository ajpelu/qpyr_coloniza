# Compute abundances by class 
library("tidyverse")

# read data 
tree_data_crops <- read_csv("data/tree_data_crops.csv") %>% 
  dplyr::select(nombre, tipo, altitud, localidad, tree_class, crop_code,
                tree_type, diametro_base, altura, especie, size_category) 


# abundance tree class (seedling, sapling, tree)
# n individulas by 30x10 (--> to ind/100 m2); n*100/300 
abundance_tree_class <- tree_data_crops %>% 
  group_by(tree_class, nombre, especie, localidad, crop_code, tipo) %>% 
  count() %>% 
  mutate(n = n/3) 

write_csv(abundance_tree_class, "data/abundance_tree_class.csv")

# abundance by size (30, 60, ...) 
abundanbce_size_class <- tree_data_crops %>% 
  group_by(size_category, nombre, especie, localidad, crop_code, tipo) %>% 
  count() %>% 
  mutate(n = n/3)

write_csv(abundanbce_size_class, "data/abundance_size_class.csv")


