library(readxl)
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(dplyr)
library(raster)
library(progress)
source("function/gis_functions.R")


# Get Data ----------------------------------------------------------------

# Read Worldpop gridded population
worldpop <- raster::raster("input/GHS_POP_E2020_AFG_4326.tif") 
calculate_population(worldpop)

res(worldpop)

plot(worldpop)

# Read settlmemnt boundries
set.seed(12082023)
settlements <- read_excel("input/settlement_pop_5_regions.xlsx") %>% 
  mutate(
    ESPSG_Zone = case_when(
      POINT_X < 66 ~ 32641,
      TRUE ~ 32642
    )
  ) %>% 
  sample_n(50, weight = Population) %>% 
  mutate_at(vars(POINT_Y, POINT_X), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"),
    agr = "constant",
    crs = 4326,        # coordinate system
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(crs = 3857)


# Calculated a buffer of 1KM radius for each settlement
settlements_boundry <- settlements %>% 
  st_buffer(1000, endCapStyle = "ROUND" , joinStyle = "ROUND" )


# Read Building footprints
af_buildings <- readRDS("input/AFG_MS_Building_Footprints.RDS") %>% 
 dplyr::select(geometry) %>% st_transform(crs = 3857) 

# Filter Building footprints only for selected settlements
af_buildings_filtered <- af_buildings %>% st_filter(settlements_boundry, .predicate = st_intersects)


# Loop through all settlements --------------------------------------------

# All Settlements 
pb <- progress_bar$new(total = nrow(settlements))
my_grid_list <- list()


for (i in  1:nrow(settlements)) {
  pb$tick()
  
  single_settlement <- filter_settlment(settlements, 
                                        settlements_boundry,
                                        af_buildings_filtered,
                                        worldpop,
                                        settlements$Ref_FID[i]) %>% suppressMessages()
  
  my_settlement   <- single_settlement$settlements_filt
  my_sett_boundry <- single_settlement$settlements_boundry_filt
  my_buildings    <- single_settlement$af_buildings_filt
  my_raster       <- single_settlement$population_raster
  
  my_total_pop <- calculate_population(my_raster)
  
  # Step 4 - Intersect buildings layer with grid
  # my_grid      <- make_grid(my_sett_boundry, my_crs = settlements$ESPSG_Zone[i]) %>% 
  my_grid      <- make_grid(my_sett_boundry, my_crs = 3857) %>% 
    st_filter(my_buildings,  .predicate = st_intersects) %>% suppressMessages()
  
  if(nrow(my_grid) > 0){
    # Step 5 - Add area field to the output of the intersection and calculate geometry in order to calculate square meter area of each building segment.
    my_buildings$area_sqm <- st_area(my_buildings$geometry)
    
    
    # Step 6 and 7
    my_grid <- calculate_building_area_per_grid(my_grid = my_grid, my_buildings = my_buildings)
    
    
    # Step 8 - Calculate estimated population per cell 
    
    avarage_nr <- my_total_pop / sum(my_grid$area_sqm) %>% as.numeric()
    
    my_grid$population <- my_grid$area_sqm *  avarage_nr
    
    my_grid$settlement_name <- my_settlement$Stlmnt_Eng
    
    
    my_grid_list[[i]] <- my_grid
  }
  
  
}


super_grid <- do.call(rbind, my_grid_list)
st_crs(super_grid)
mapview::mapview(super_grid)












