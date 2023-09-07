
setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

# Define the spatial extent and CRS
min_lon = -125.0
max_lon = -66.0
min_lat = 24.0
max_lat = 50.0
extent <- st_bbox(c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat))
crs <- st_crs("+proj=longlat +datum=WGS84 +no_defs")

# Create a grid of polygons with 0.1-degree resolution
grid <- st_make_grid(extent, cellsize = c(0.1, 0.1), crs = 4326)

# Convert the grid to an sf object
grid_sf <- st_sf(geometry = grid)

# Add a column for the grid cell number
grid_sf <- grid_sf %>%
  mutate(ID = row_number()) %>%
  dplyr::select(ID, geometry)


# Set the CRS of the sf object
#st_crs(grid_sf) <- crs

# Intersect grid_sf with state shapefile to remove extra area. This way, all of
# the grid cells are the same size
# intersection <- st_intersection(grid_sf, state2000)
# grid_sf <- grid_sf[intersection, ]
# 
# # Plot to confirm it worked
# ggplot() +
#   geom_sf(
#     data = state2000,
#     color = "red",
#     lwd = 0.5
#   ) +
#   geom_sf(
#     data = grid_sf,
#     lwd = 0.1,
#     fill = NA,
#     alpha = 0
#   )
# 
# Output
filepath_out <- paste0(path_data_int, "/grid/grid_latlong_0p1_degree.shp")
st_write(grid_sf, filepath_out, delete_dsn = T)


