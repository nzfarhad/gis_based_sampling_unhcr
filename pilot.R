library(readxl)
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(dplyr)
library(raster)
library(progress)
source("function/gis_functions_2.R")
library(mapview)
library(ggplot2)

# Get Data ----------------------------------------------------------------

# Read gridded population
# gridded_pop <- raster::raster("input/facebook_population_afg_2018-10-01.tif")
gridded_pop <- raster::raster("input/GHS_POP_E2020_AFG_4326.tif")
# gridded_pop <- raster::raster("input/gridded_pop/AFG2017_100m_fullypredicted_c/AFG_2017_pop_per_cell_raster_100m_FP_c.tif") 

# fb_total_afg_pop = 32526561

calculate_population(gridded_pop)
res(gridded_pop)
crs(gridded_pop)



settlements <- st_read("input/pilot/Pilot_4Villages_SpatialJoin/Pilot_4Villages_SpatialJoin.shp") %>% 
  mutate(
    ESPSG_Zone = case_when(
      POINT_X < 66 ~ 32641,
      TRUE ~ 32642
    )
  ) %>% 
  mutate_at(vars(POINT_Y, POINT_X), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"),
    agr = "constant",
    crs = 4326,        # coordinate system
    stringsAsFactors = FALSE,
    remove = TRUE
  ) %>% 
  st_transform(crs = 3857) %>% st_drop_geometry()



# Read settlement boundaries
settlements_boundry <- st_read("input/pilot/Pilot_4Villages_SpatialJoin/Pilot_4Villages_SpatialJoin.shp") %>% 
  st_transform(crs = 3857)  %>%
  st_zm(drop = T)


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
                                        gridded_pop,
                                        settlements$Ref_ID[i]) %>% suppressMessages()
  
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


super_grid <- do.call(rbind, my_grid_list) %>% 
  mutate(
    population = round(population, 0),
    cell_id = 1:n()
  )

super_grid_larger_19 <- super_grid %>% filter(population > 19)

# st_crs(super_grid)
# mapview::mapview(super_grid) +
#   mapview::mapview(settlements_boundry$geometry)
  






# Sample ------------------------------------------------------------------

set.seed(05092023)
sample_list <- list()

for (setlnt in unique(super_grid_larger_19$settlement_name)) {
     single_set <- super_grid_larger_19 %>% filter(settlement_name %in% setlnt) 
     my_sample <- sample_n(single_set, 4)
     sample_list[[setlnt]] <- my_sample
}

Sample <- do.call(rbind, sample_list) %>% 
  mutate(
    Sampled = "Yes"
  ) %>% 
  `rownames<-`(NULL)

Sample_no_geo <- Sample  %>% 
  st_drop_geometry()

super_grid_larger_19 <- super_grid_larger_19 %>% 
  left_join(dplyr::select(Sample_no_geo, cell_id, Sampled)) %>% 
  mutate(
    Sampled = case_when(
      is.na(Sampled) ~ "No",
      TRUE ~ Sampled
    )
  )


# Plot --------------------------------------------------------------------

mapview::mapview(settlements_boundry, layer.name = "Settlement boundary",  alpha.regions = 0, col.regions = "darkblue", color = "darkblue", label = "Stlmnt_Eng") +
  mapview::mapview(super_grid, layer.name = "Unfiltered Grid",  zcol = "population", col.regions = colorRampPalette(c("yellow", "darkred"))(10), hide = T) +
  mapview::mapview(super_grid_larger_19, layer.name = "filtered Grid, 19+",  zcol = "population", col.regions = colorRampPalette(c("yellow", "darkred"))(10)) +
  mapview::mapview(Sample, col.regions = "blue", hide = T) 




# Export ------------------------------------------------------------------

# RDS format
saveRDS(my_grid2, "output/Qala_i_naw_analysis.RDS")

# Shapefile
st_write(super_grid_larger_19, "output/Pilot/GHS_E2020_grid/filtered/pilot_grid_facebook_data.shp")
st_write(super_grid, "output/Pilot/GHS_E2020_grid/unfiltered/pilot_grid_facebook_data.shp")







