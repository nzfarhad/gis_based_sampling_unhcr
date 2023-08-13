library(sf)

# Microsoft building footprint directory path
folder_path <- "input/afg_microsoft_building_footprints"

# List all GeoJSON files
file_paths <- list.files(path = folder_path, pattern = "\\.geojson$", full.names = TRUE)


# Read GeoJSON files into a list of sf objects
sf_list <- lapply(file_paths, st_read)

# Combine the list of sf objects into a single sf object
merged_sf <- do.call(rbind, sf_list)

# Save as an RDS file
saveRDS(merged_sf, "input/AFG_MS_Building_Footprints.RDS")