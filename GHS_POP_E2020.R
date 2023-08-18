

library(raster)
library(sf)
library(ggplot2)
source("function/gis_functions.R")

# Read GHS_POP Rasters - Mollweide
grid1 <- raster("input/GHS_POP_E2020_GLOBE_R2023A_4326/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R5_C25/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R5_C25.tif")
grid2 <- raster("input/GHS_POP_E2020_GLOBE_R2023A_4326/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R5_C26/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R5_C26.tif")
grid3 <- raster("input/GHS_POP_E2020_GLOBE_R2023A_4326/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R6_C25/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R6_C25.tif")
grid4 <- raster("input/GHS_POP_E2020_GLOBE_R2023A_4326/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R6_C26/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R6_C26.tif")

# Merge the Rasters
merged_raster <- merge(grid1, grid2, grid3, grid4)

# Check Extent, Resolution and projection
extent(merged_raster)
res(merged_raster)
projection(merged_raster)

# Change projection to longlat W84 from Mollweide
# projection <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# merged_raster_longlat <- projectRaster(merged_raster, crs = projection)
  

# Read Afghanistan polygon
afg_boundry <- st_read("input/geoBoundaries-AFG-ADM0.geojson")

# change the projection to Mollweide - only if the raster is not reprojected to loglat
# moll_projection <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# afg_boundry <- st_transform(afg_boundry, crs = moll_projection)

# Clip the raster based on Afghanistan boundaries
cropped_raster <- raster::crop(merged_raster, extent(afg_boundry))
afg_raster <- raster::mask(cropped_raster, afg_boundry)

# Change projection to longlat 4326 from Mollweide
# projection_loglat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# afg_raster_longlat <- projectRaster(afg_raster, crs = projection_loglat)


# Plot 
plot(afg_raster)
plot(afg_boundry, add = T)


# Save as RDS 
saveRDS(afg_raster, "input/GHS_POP_E2020_AFG_4326.RDS")

# Save as TIF
writeRaster(afg_raster, filename = "input/GHS_POP_E2020_AFG_4326.tif", format = "GTiff", overwrite=T)

