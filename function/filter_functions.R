# function to filter layers


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

filter_settlment <- function(settlements, settlements_boundry_4326, af_buildings, raster, settlement){
  
  settlements <- settlements %>% filter(Stlmnt_Eng %in% settlement)
  settlements_boundry_4326 <- settlements_boundry_4326 %>% 
    filter(Stlmnt_Eng %in% settlement)
  
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


calculate_population <- function(raster){
  population <- raster::cellStats(raster, sum, na.rm = TRUE)
  return(population)
  print(population)
}
