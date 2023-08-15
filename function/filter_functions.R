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

filter_settlment <- function(settlements, settlements_boundry_4326, af_buildings, raster1, settlement){
  
  settlements <- settlements %>% filter(Stlmnt_Eng %in% settlement)
  settlements_boundry_4326 <- settlements_boundry_4326 %>% 
    filter(Stlmnt_Eng %in% settlement)
  
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


# Create Grid
make_grid <- function(poly, cell_size = 100, my_crs = 32642) {
  # planar coordinates for Afghanistan EPSG:32642
  my_polygon <- st_transform(poly, crs = st_crs(my_crs))
  # Create the grid
  grid <- st_make_grid(my_polygon, cellsize = cell_size) %>%
    st_sf %>% 
    st_filter(my_polygon, .predicate = st_intersects) %>% 
    st_transform(crs = 4326)
  return(grid)
}

# Count population per grid
count_ppp <- function(){
  count <- vector(my_grid, my_raster)
  for (i in 1:nrow(grid)) {
    cropped_raster <- raster::crop(my_raster, extent(st_sf(my_grid$geometry[i])))
    masked_raster <- raster::mask(cropped_raster, st_sf(my_grid$geometry[i]))
    count <- c(count, calculate_population(masked_raster))  
  }
  return(count)
  
}



