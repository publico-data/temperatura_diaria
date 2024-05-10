library(sf)

a <- read_rds("dia.rds")
heatspots <- read_rds("coordenadas_heatspots.rds") %>% 
  select(-corrdenadas_heatspot) %>% 
  select(-dist)

a <- st_as_sf(a, coords = c("lon", "lat"), crs = 4326)
heatspots$coordenadas_praia <- st_transform(heatspots$coordenadas_praia, crs = 4326)


closest_points <- purrr::map_df(1:nrow(heatspots), function(i) {
  beach_point <- heatspots[i, ]
  distances <- st_distance(a, beach_point$coordenadas_praia)
  min_index <- which.min(distances)
  
  a[min_index, ] %>%
    mutate(nome_praia = beach_point$nome_praia, 
           distance_to_praia = min(distances))
})

closest_points <- closest_points %>% 
  select(nome_praia, geometry) 


h <- closest_points %>% 
  left_join(heatspots, by = c("nome_praia" = "nome_praia"))

#give me all dupicated names in h
duplicated_names <- h %>% 
  group_by(nome_praia) %>% 
  filter(n() > 1) %>% 
  pull(nome_praia)

h[38,] <- NA
h[356,] <- NA
h[105,] <- NA
h[284,] <- NA
h[228,] <- NA
h[229,] <- NA
h[525,] <- NA
h[527,] <- NA
h[577,] <- NA
h[578,] <- NA
h[265,] <- NA
h[317,] <- NA
h[293,] <- NA
h[574,] <- NA


h <- h %>% 
  filter(!is.na(nome_praia))

h <- st_set_geometry(h, "corrdenadas_heatspot")

heatspots$corrdenadas_heatspot <- h$corrdenadas_heatspot

write_rds(heatspots, "coordenadas_heatspots.rds")


