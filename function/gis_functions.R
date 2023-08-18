# function to filter layers

library(raster)
library(sf)

# Provincial filter
filter_province <- function(settlements, settlements_boundry_4326, af_buildings, raster,  province){
  settlements <- settlements %>% filter(Prvnce_Eng %in% province)
  settlements_boundry_4326 <- settlements_boundry_4326 %>% 
    filter(Prvnce_Eng %in% province)
  
  af_buildings_filtered <- af_buildings %>% 
    st_filter(settlements_boundry_4326, .predicate = st_intersects)
  
  cropped_raster <- raster::crop(raster, extent(settlements_boundry_4326))
  masked_raster <- raster::mask(cropped_raster, settlements_boundry_4326)
  
  output_list <- list(settlements_filt = settlements,
                      settlements_boundry_filt = settlements_boundry_4326,
                      af_buildings_filt = af_buildings_filtered,
                      population_raster = masked_raster
  )
  return(output_list)
  
}


# District filter
filter_district <- function(settlements, settlements_boundry_4326, af_buildings, raster, district){
  
  settlements <- settlements %>% filter(Dstrct_Eng %in% district)
  settlements_boundry_4326 <- settlements_boundry_4326 %>% 
    filter(Dstrct_Eng %in% district)
  
  af_buildings_filtered <- af_buildings %>% 
    st_filter(settlements_boundry_4326, .predicate = st_intersects)
  
  cropped_raster <- raster::crop(raster, extent(settlements_boundry_4326))
  masked_raster <- raster::mask(cropped_raster, settlements_boundry_4326)
  
  output_list <- list(settlements_filt = settlements,
                      settlements_boundry_filt = settlements_boundry_4326,
                      af_buildings_filt = af_buildings_filtered,
                      population_raster = masked_raster
  )
  return(output_list)
  
}



# Settlement filter

filter_settlment <- function(settlements, settlements_boundry_4326, af_buildings, raster1, my_id){
  
  settlements <- settlements %>% filter(Ref_FID %in% my_id)
  settlements_boundry_4326 <- settlements_boundry_4326 %>% 
    filter(Ref_FID %in% my_id)
  
  # settlements_boundry_32642 <- st_transform(settlements_boundry_4326, crs = st_crs(32642))
  # af_buildings <- st_transform(af_buildings, crs = st_crs(32642))
  
  af_buildings_filtered <- af_buildings %>% 
    st_filter(settlements_boundry_4326, .predicate = st_intersects)
  
  cropped_raster <- raster::crop(raster1, extent(settlements_boundry_4326))
  masked_raster <- raster::mask(cropped_raster, settlements_boundry_4326)
  
  output_list <- list(settlements_filt = settlements,
                      settlements_boundry_filt = settlements_boundry_4326,
                      af_buildings_filt = af_buildings_filtered,
                      population_raster = masked_raster
  )
  return(output_list)
  
}


calculate_population <- function(raster){
  population <- raster::cellStats(raster, sum, na.rm = TRUE)
  return(population)
  print(population)
}


# # Create Grid
# make_grid <- function(poly, cell_size = 100, my_crs = 32642) {
#   # planar coordinates for Afghanistan EPSG:32642
#   my_polygon <- st_transform(poly, crs = st_crs(my_crs))
#   # Create the grid
#   grid <- st_make_grid(my_polygon, cellsize = cell_size) %>%
#     st_sf %>% 
#     st_filter(my_polygon, .predicate = st_intersects)
#   grid$Shape_Length <- round(as.numeric(st_length( st_transform(grid$geometry, crs = 4326))))
#   grid$Shape_Area <- round(as.numeric(st_area(grid$geometry)))
#   return(grid)
# }

# Create Grid - crs 32642
make_grid <- function(poly, cell_size = 100, my_crs = 32642) {
  # planar coordinates for Afghanistan EPSG:32642
  my_polygon <- st_transform(poly, crs = st_crs(my_crs))
  
  # Create the grid
  grid <- st_make_grid(my_polygon, cellsize = cell_size) %>%
    st_sf %>%
    st_filter(my_polygon, .predicate = st_intersects)  %>% 
    st_transform(crs = 4326)
  grid$Shape_Length <- round(as.numeric(st_length(grid$geometry)))
  grid$Shape_Area <- round(as.numeric(st_area(grid$geometry)))
  return(grid)
}

# # Create Grid - crs 4326
# make_grid <- function(poly, cell_size = c(0.000991, 0.000991)) {
#   # planar coordinates for Afghanistan EPSG:32642
#   
#   # Create the grid
#   grid <- st_make_grid(poly, cellsize = cell_size) %>%
#     st_sf %>%
#     st_filter(poly, .predicate = st_intersects) # %>%
#     # st_transform(crs = 4326)
#   grid$Shape_Length <- round(as.numeric(st_length(grid$geometry)))
#   grid$Shape_Area <- round(as.numeric(st_area(grid$geometry)))
#   return(grid)
# }


# Count population per grid
count_ppp <- function(my_grid, my_raster){
  count <- vector()
  for (i in 1:nrow(grid)) {
    cropped_raster <- raster::crop(my_raster, extent(st_sf(my_grid$geometry[i])))
    masked_raster <- raster::mask(cropped_raster, st_sf(my_grid$geometry[i]))
    count <- c(count, calculate_population(masked_raster))  
  }
  return(count)
  
}



calculate_building_area_per_grid <- function(my_grid, my_buildings){
  
  grid_area <- vector()
  for (i in 1:nrow(my_grid)) {
    grid_building_area <- st_filter(my_buildings, my_grid$geometry[i], .predicate = st_intersects) %>% suppressMessages()
    grid_building_area_sum <- sum(grid_building_area$area_sqm, na.rm = T)
    grid_area <- c(grid_area, grid_building_area_sum)
    print(i)
  }
  my_grid$area_sqm <- grid_area
  
  return(my_grid)
}



clip_raster <- function(raster, polygon)  {
  cropped_raster <- raster::crop(raster, extent(polygon))
  masked_raster <- raster::mask(cropped_raster, polygon)
  return(masked_raster)
}


