library(readxl)
library(tidyverse)
library(osmdata)
library(ggmap)
library(OpenStreetMap)
library(viridis)
# library(stars)
library(sf)
sf_use_s2(FALSE)
library(dplyr)
library(raster)
source("function/filter_functions.R")

# Read Worldpop gridded population
# worldpop <- read_stars("input/afg_ppp_2020_UNadj.tif") %>% select(ppp = 1) 
worldpop <- raster::raster("input/afg_ppp_2020_UNadj.tif") 


# Read settlmemnt boundries
set.seed(12082023)
settlements <- read_excel("input/settlement_pop_5_regions.xlsx") %>% 
  mutate(
    ESPSG_Zone = case_when(
      POINT_X < 66 ~ 32641,
      TRUE ~ 32642
    )
  ) %>% 
  sample_n(50) %>% 
  mutate_at(vars(POINT_Y, POINT_X), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"),
    agr = "constant",
    crs = 4326,        # coordinate system
    stringsAsFactors = FALSE,
    remove = TRUE
  )

# Change coordinate system
buffer = st_transform(settlements, crs = 3857)
# Calculated a buffer of 1KM radius for each settlement
settlements_boundry = st_buffer(buffer, 1000, endCapStyle = "ROUND" , joinStyle = "ROUND" ) %>% st_transform(crs = 3857)

# Change the Coordinate system back to 4326 for the filter
settlements_boundry_4326 <- st_transform(settlements_boundry, crs = 4326)

# Read Building footprints
af_buildings <- readRDS("input/AFG_MS_Building_Footprints.RDS")
# st_write(af_buildings, "input/AFG_MS_Building_Footprints.shp")
af_buildings <- af_buildings[,"geometry"]


# Filter Building footprints only for selected settlements
af_buildings_filtered <- af_buildings %>% st_filter(settlements_boundry_4326, .predicate = st_intersects)

# Join The objects
# merged_sf_filtered <- st_join(af_buildings, settlements_boundry2, join = st_intersects)





# filter a single province
single_province <- filter_province(settlements, 
                                   settlements_boundry_4326,
                                   af_buildings_filtered,
                                   worldpop,
                                   "Balkh"
                                   )
# filter a single district
single_district <- filter_district(single_province$settlements_filt,
                                   single_province$settlements_boundry_filt,
                                   single_province$af_buildings_filt,
                                   worldpop,
                                   "Mazar-e-Sharif"
                                   )
# Filter a single settlment
single_settlement <- filter_settlment(settlements, 
                                      settlements_boundry_4326,
                                      af_buildings_filtered,
                                      worldpop,
                                      53933)





# Create the grid
grid <- make_grid(single_settlement$settlements_boundry_filt, 100)  

# sample grid
grid_sample <- sample_n(grid, 4)

# Count population per grid
population_count <- count_ppp(grid, single_settlement$population_raster)



my_list <- list()

for (sett in single_district$settlements_filt$Stlmnt_Eng) {
  
  sing_sett <- filter_settlment(settlements, 
                                settlements_boundry_4326,
                                af_buildings_filtered,
                                worldpop,
                                sett
                                )
  
  grid <- make_grid(sing_sett$settlements_boundry_filt, 100)
  population_count <- count_ppp(grid, sing_sett$population_raster )
  grid$population <- population_count
  
  my_list[[sett]] <- grid
  
}


balkh <- do.call(rbind, my_list)
mazar <- do.call(rbind, my_list)




# my_list <- list()
# position = 1
# for (i in 1: length(grid)) {
#   aa <- st_intersects(grid[i], single_settlement$settlements_boundry_filt)
#   if(is_empty(aa[[1]])) {
#     my_list[[position]] <- grid[i]
#     position <- position + 1
#   }
# }




# produce the map for a signe district
mapview::mapview(single_settlement$settlements_filt) +
  mapview::mapview(single_settlement$settlements_boundry_filt, alpha.regions = 0) +
  mapview::mapview(single_settlement$af_buildings_filt) +
  mapview::mapview(single_settlement$population_raster) +
  mapview::mapview(grid, alpha.regions = 0) +
  mapview::mapview(grid_sample, alpha.regions = 0.5, col.regions = "red") 



st_area(grid$geometry[1])
st_area(single_settlement$settlements_boundry_filt)

calculate_population(single_settlement$population_raster)







