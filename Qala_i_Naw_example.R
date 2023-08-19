
library(raster)
library(dplyr)
library(sf)
sf_use_s2(FALSE)
source("function/gis_functions.R")


worldpop <- raster::raster("input/afg_ppp_2020_UNadj.tif") 

af_buildings <- readRDS("input/AFG_MS_Building_Footprints.RDS") %>% st_transform(crs = 3857) 
af_buildings <- af_buildings[,"geometry"]



qala_e_naw <- st_read("input/Qala_i_Naw/Qala-i-Naw.shp") %>% 
  st_sf() %>% select(geometry) %>% 
  # st_transform(crs = 4326)
  st_transform(crs = 3857)



af_buildings_filtered <- af_buildings %>% 
  st_filter(qala_e_naw, .predicate = st_intersects)

qala_e_naw_4326 <- qala_e_naw %>% st_transform(crs = 4326)

cropped_raster <- raster::crop(worldpop, extent(qala_e_naw_4326))
masked_raster <- raster::mask(cropped_raster, qala_e_naw_4326)


my_total_pop <- calculate_population(masked_raster)


my_grid      <- make_grid(qala_e_naw, my_crs = 3857) %>% st_filter(af_buildings,  .predicate = st_intersects) 

# my_grid_test      <- make_grid(qala_e_naw, my_crs = 3857) %>% st_filter(af_buildings_filtered,  .predicate = st_intersects) 
# 
# mapview::mapview(my_grid_test)


my_buildings <- af_buildings_filtered

# Step 5 - Add area field to the output of the intersection and calculate geometry in order to calculate square meter area of each building segment.
my_buildings$area_sqm <- round(st_area(my_buildings$geometry),2) # %>% as.numeric()


# Step 6 and 7
start_time <- Sys.time()
my_grid2 <- calculate_building_area_per_grid(my_grid = my_grid, my_buildings = my_buildings)
end_time <- Sys.time()
execution_time <- end_time - start_time

# start_time <- Sys.time()
# my_grid3 <- calculate_building_area_per_grid(my_grid = my_grid, my_buildings = my_buildings)
# end_time <- Sys.time()
# execution_time <- end_time - start_time


# Step 8 - Calculate estimated population per cell 
avarage_nr <- my_total_pop / sum(my_grid3$area_sqm) %>% as.numeric()
my_grid3$population <- round(my_grid3$area_sqm *  avarage_nr, 0)

# Test
sum(my_grid3$population)
my_total_pop


# Plot
library(mapview)
library(ggplot2)

Sample <- sample_n(my_grid3, 4, weight = my_grid3$population) #%>% select(-c(Shape_Length))

mapview(qala_e_naw, layer.name = "Qala-i-Naw",  alpha.regions = 0, col.regions = "grey") +
  mapview::mapview(my_grid3, layer.name = "Grid Population",  zcol = "population", col.regions = colorRampPalette(c("yellow", "darkred"))(10)) +
  mapview::mapview(Sample, col.regions = "blue") 


my_grid4 <- my_grid2 %>% filter(population > 0 & area_sqm > 0)
# Create a ggplot2 plot with graduated colors
ggplot() +
  geom_sf(data = my_grid4, aes(fill = population)) +
  scale_fill_gradient(low = "yellow" , high =  "darkred") +
  labs(title = "Qala_i_Naw grid (R)",
       fill = "Population")



from_Acr <- st_read("input/Analyzed/Export_Output.shp") %>% 
  filter(area_sqm > 0 & population > 0)

ggplot() +
  geom_sf(data = from_Acr, aes(fill = population)) +
  scale_fill_gradient(low = "yellow" , high =  "darkred") +
  labs(title = "Qala_i_Naw grid (ArcGIS)",
       fill = "Population")


sum(from_Acr$population)
sum(my_grid3$population)

summary(from_Acr$population)
summary(my_grid3$population)
