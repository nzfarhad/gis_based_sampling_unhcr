library(readxl)
library(tidyverse)
library(osmdata)
library(ggmap)
library(OpenStreetMap)
library(viridis)
# library(stars)
library(sf)
sf_use_s2(FALSE)
source("filter_functions.R")


# Read Worldpop gridded population
# worldpop <- read_stars("input/afg_ppp_2020_UNadj.tif") %>% select(ppp = 1) 
worldpop <- raster::raster("input/afg_ppp_2020_UNadj.tif") 


# Read settlmemnt boundries
set.seed(12082023)
settlements <- read_excel("input/settlement_pop_5_regions.xlsx") %>% 
  sample_n(1249) %>% 
  mutate_at(vars(POINT_Y, POINT_X), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("POINT_X", "POINT_Y"),
    agr = "constant",
    crs = 4326,        # coordinate system
    stringsAsFactors = FALSE,
    remove = TRUE
  )

# Change coordinate system
buffer = st_transform(settlements, crs = 7801)
# Calculated a buffer of 1KM radius for each settlement
settlements_boundry = st_buffer(buffer, 1000, endCapStyle = "ROUND" , joinStyle = "ROUND" )

# Change the Coordinate system back to 4326 for the filter
settlements_boundry_4326 <- st_transform(settlements_boundry, crs = 4326)

# Read Building footprints
af_buildings <- readRDS("input/AFG_MS_Building_Footprints.RDS")


# Filter Building footprints only for selected settlements
af_buildings_filtered <- af_buildings %>% st_filter(settlements_boundry_4326, .predicate = st_intersects)

# Join The objects
# merged_sf_filtered <- st_join(af_buildings, settlements_boundry2, join = st_intersects)




# Produce the map for all settlements
mapview::mapview(settlements) +
  mapview::mapview(settlements_boundry) +
  mapview::mapview(af_buildings_filtered)



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
                                   "Balkh"
                                   )

single_settlement <- filter_settlment(settlements, 
                                      settlements_boundry_4326,
                                      af_buildings_filtered,
                                      worldpop,
                                      "Mirza Hashim")


# cropped_raster <- st_crop(worldpop, single_district$settlements_boundry_filt)



# produce the map for a signe district
mapview::mapview(single_settlement$settlements_filt) +
  mapview::mapview(single_settlement$settlements_boundry_filt, alpha.regions = 0) +
  # mapview::mapview(single_settlement$af_buildings_filt) +
  # mapview::mapview(single_settlement$population_raster) +
  mapview::mapview(grid_points)

calculate_population(single_settlement$population_raster)



##############


ppp1km <-
  st_as_stars(
    st_bbox(ppp100m),
    dx = st_dimensions(ppp100m)$x$delta*10,
    dy = st_dimensions(ppp100m)$y$delta*10) %>%
  st_warp(
    src = ppp100m,
    method = "average",
    use_gdal = TRUE)

ppp1km <- ppp1km %>% select(ppp = 1) %>% mutate(ppp = ppp*100)


ggplot() +
  geom_stars(data = ppp1km) +
  scale_x_continuous(labels = function(x) { str_c(x, "°", if_else(x >= 0, "E", "W")) }) +
  scale_y_continuous(labels = function(y) { str_c(y, "°", if_else(y >= 0, "N", "S")) }) +
  #scale_fill_viridis_b(
  scale_fill_viridis(option = "B",
                     trans = "log10",
                     breaks = 10^(0:3),
                     label = scales::label_comma(accuracy = 1),
                     direction = -1) +
  theme_minimal() +
  labs(
    title = "2020 Afghanistan Population (UNPD-adjusted)",
    fill = "persons/km²",
    x = NULL, y = NULL)




douclust <- function(data, type, cellmin, popmin) {
  data %>% 
    filter(ppp >= cellmin) %>% 
    mutate(
      cluster = 
        st_disjoint(., .) %>% as.dist() %>% hclust(method = "single") %>% cutree(h = 0)) %>% 
    group_by(cluster) %>% 
    summarize(class = type, pop = sum(ppp)) %>% 
    filter(pop >= popmin) %>% 
    select(-cluster)
}



ppp1km_sf <- ppp1km %>% st_as_sf()
#sum(ppp1km_sf$ppp)

workingset <- ppp1km_sf

rural_vlow <- 
  workingset %>% 
  filter(ppp < 50) %>% 
  summarize(class = "Very low density rural grid cells", pop = sum(ppp))



workingset <- workingset %>% filter(ppp >= 50)

urban_centers <- workingset %>% douclust("Urban center", 1500, 50000)


urban_clusters <- workingset %>% douclust("Urban cluster", 300, 5000)

workingset <- workingset %>% st_difference(st_union(urban_centers))


urban_dense_clusters <- workingset %>% douclust("Dense urban cluster", 1500, 5000)

workingset <- workingset %>% st_difference(st_union(urban_dense_clusters))

urban_semidense_clusters <- 
  workingset %>% 
  douclust("Semi-dense urban cluster", 300, 5000) %>% 
  st_filter(
    st_union(st_union(urban_centers), st_union(urban_dense_clusters)),
    .predicate = negate(st_is_within_distance),
    dist = units::set_units(2, km))

workingset <- workingset %>% st_difference(st_union(urban_semidense_clusters))

urban_suburban_cells <- 
  workingset %>% 
  st_filter(urban_clusters, .predicate = st_within) %>% 
  douclust("Suburban or peri-urban grid cells", 0, 0)

workingset <- workingset %>% st_difference(st_union(urban_suburban_cells))

rural_clusters <- workingset %>% douclust("Rural cluster", 300, 500)

workingset <- workingset %>% st_difference(st_union(rural_clusters))

rural_low <- workingset %>% summarize(class = "Low density rural grid cells", pop = sum(ppp))

rm(workingset)

doumap <- 
  rbind(
    urban_centers,
    urban_dense_clusters, urban_semidense_clusters, urban_suburban_cells,
    rural_clusters, rural_low, rural_vlow)



doumap %>% 
  ggplot() + 
  geom_sf(aes(fill = as_factor(class)), color = NA) +
  scale_x_continuous(labels = function(x) { str_c(x, "°", if_else(x >= 0, "E", "W")) }) +
  scale_y_continuous(labels = function(y) { str_c(y, "°", if_else(y >= 0, "N", "S")) }) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Degree of Urbanization", fill = NULL)


save.image("output/Afghanistan_Degree_of_Urbanization.RData")

table(doumap$class, doumap$pop)

sum(doumap$pop[doumap$class == "Rural cluster"])

doumap %>% group_by(class) %>% summarise(pop = sum(pop))


set.seed(1)

# Extract Kabul
kabul <- urban_centers %>% arrange(desc(pop)) %>% slice(2)

nclust <- 100
hh_per_clust <- 10

b <- 
  ppp1km_sf %>% 
  st_filter(kabul, .predice = st_within) %>% 
  mutate(psuid = row_number()) %>% 
  sample_n(nclust, weight = .$ppp) %>% 
  rename(psupop = ppp, psugeometry = geometry) %>% 
  rowwise() %>% 
  mutate(
    ssu = 
      ppp100m %>% 
      st_crop(psugeometry) %>% 
      st_as_sf() %>% 
      mutate(ssuid = row_number()) %>% 
      sample_n(hh_per_clust, weight = .$ppp) %>% 
      rename(ssupop = ppp, ssugeometry = geometry) %>% 
      list()) %>% 
  unnest(ssu) %>% 
  st_as_sf(sf_column_name = "ssugeometry") %>% 
  mutate(tsugeometry = st_sample(., rep(1, nrow(.))))




# Draw a random PSU to use as an example
ex <- 
  s %>% filter(psuid == sample(psuid, 1, prob = psupop)) %>% st_set_geometry("psugeometry")

## Extract bounding box to get bing imagery
bbox <- kabul %>%  st_bbox()

## Extract imagery background
map <- openmap(c(bbox$ymax,bbox$xmin), c(bbox$ymin,bbox$xmax), 
               zoom = 11,
               type =  "bing" ,
               mergeTiles = TRUE)



## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

psubg <- 
  
  ## option for google map background---   
  #   get_googlemap(
  #     center = kabul %>% st_centroid() %>% st_coordinates(),
  #     zoom = 11,
  #     scale = 1,
  #     maptype = "satellite")
  
  #psubg %>% 
  #ggmap() +
  
## Option for bing background
autoplot.OpenStreetMap(map.latlon)  +
  geom_sf(aes(fill = ppp), inherit.aes = FALSE, alpha = .15, 
          data = ppp1km_sf %>% st_filter(kabul, .predicate = st_within)) +
  geom_sf(aes(geometry = psugeometry), inherit.aes = FALSE,
          fill = NA, color = "red", alpha = .15, 
          data = s) +
  geom_sf(aes(geometry = psugeometry), inherit.aes = FALSE,
          color = "blue", fill = NA, alpha = .15,
          data = ex) +
  scale_fill_viridis_c(labels = scales::comma) +
  scale_x_continuous(labels = function(x) { str_c(x, "°", if_else(x >= 0, "E", "W")) }) +
  scale_y_continuous(labels = function(y) { str_c(y, "°", if_else(y >= 0, "N", "S")) }) +
  theme_minimal() +
  labs(title = "Primary Sampling Stage",
       fill = "persons/cell", caption = "1 cell = 1km x 1km",
       x = NULL, y = NULL)

psubg



## Extract bounding box to get bing imagery
bbox <- ex %>%  st_bbox()

## Extract imagery background
map <- openmap(c(bbox$ymax,bbox$xmin), c(bbox$ymin,bbox$xmax), 
               zoom = 16,
               type =  "bing" ,
               mergeTiles = TRUE)



## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


ssubg <- 
  
  ## option for google map background---   
  #   get_googlemap(
  #     center = ex %>% st_union() %>% st_centroid() %>% st_coordinates(),
  #     zoom = 16,
  #     scale = 1,
  #     maptype = "satellite")
  # 
  # ssubg %>% 
  #   ggmap() +
  
## Option for bing background  

autoplot.OpenStreetMap(map.latlon)  +
  
  geom_sf(aes(fill = ppp), inherit.aes = FALSE, alpha = .15, 
          data = ppp100m %>% st_crop(ex) %>% st_as_sf()) +
  geom_sf(aes(geometry = ssugeometry), inherit.aes = FALSE,
          fill = NA, color = "red", alpha = .15, 
          data = s %>% st_filter(ex, .predicate = st_within)) +
  geom_sf(aes(geometry = tsugeometry), inherit.aes = FALSE,
          color = "blue", size = 2,
          data = s %>% st_filter(ex, .predicate = st_within)) +
  scale_fill_viridis_c(labels = scales::comma) +
  scale_x_continuous(labels = function(x) { str_c(x, "°", if_else(x >= 0, "E", "W")) }) +
  scale_y_continuous(labels = function(y) { str_c(y, "°", if_else(y >= 0, "N", "S")) }) +
  theme_minimal() +
  labs(title = "Secondary & Tertiary Sampling Stages",
       fill = "persons/cell", caption = "1 cell = 100m x 100m",
       x = NULL, y = NULL)

ssubg


