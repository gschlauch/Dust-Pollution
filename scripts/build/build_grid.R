
# Get the extent from the Great Plains states
# note: no agreement on what these states/regions are. Here are some. try to find
# better definition

# New Mexico: 35
# Texas: 48
# Oklahoma: 40
# Kansas: 20
# Nebraska: 31
# South Dakota: 46
# North Dakota: 38
# Iowa: 19
# Montana: 30
# Wyoming: 56
# Colorado: 08
# Missouri: 29

# state2000 <- st_read(paste0(path_int_census, "/2000/state/US_state_2000.shp")) %>%
#   filter(statefp %in% c("35", "48", "40", "20", "31", "46", "38", "19", "30", "56", "08", "29"))
# xmin <- extent(state2000)[1] - 0.1
# xmax <- extent(state2000)[2] + 0.1
# ymin <- extent(state2000)[3] - 0.1
# ymax <- extent(state2000)[4] + 0.1

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
filepath_out <- paste0(path_int_grid, "/grid_latlong_0p1_degree.shp")
st_write(grid_sf, filepath_out, delete_dsn = T)


