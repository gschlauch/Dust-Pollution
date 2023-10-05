#-------------------------------------------------------------------------------
# Intersect the NWS forecast zones with counties
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Intersect forecast zones with counties ---------------------------------------

# Load the county shapefile
county_2020_shp <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp"))
county_2020_df <- as.data.frame(county_2020_shp) %>% dplyr::select(-geometry)

# Initialize final zone-to-county 1:m crosswalk
xwalk <- data.frame()

# Loop through forecast zones
dirpath <- paste0(path_data_raw, "/nws/forecast_zones")
files <- list.files(dirpath, full.names = TRUE)
for (file in files[1]) {

  # Unzip and load the file
  unzip(files[1], exdir = path_temp)
  filename <- list.files(path_temp, pattern = ".shp")
  zone_shp <- st_read(paste0(path_temp, "/", filename)) %>%
    dplyr::select(STATE, ZONE, NAME) %>%
    dplyr::rename(zone_state = STATE, zone_fips = ZONE, zone_name = NAME) %>%
    mutate_at(c("zone_state", "zone_fips", "zone_name"), ~tidy_string(.)) %>%
    mutate(zone_filename = filename) %>%
    st_transform("WGS84") %>%
    st_make_valid()
  zone_df <- as.data.frame(zone_shp) %>% dplyr::select(-geometry)
  
  # For each zone, get the indices of the counties that it intersects with
  intersections <- st_intersects(zone_shp, county_2020_shp)
  
  # Create a zone-to-county 1:m crosswalk
  for (i in 1:nrow(zone_shp)) {
    zone <- zone_df[i, ]
    counties <- county_2020_df[intersections[[i]], ]
    df <- bind_cols(zone, counties)
    xwalk <- bind_rows(xwalk, df)
  }
  
  # Delete unzipped files
  unzipped_files <- list.files(path_temp, full.names = TRUE)
  file.remove(unzipped_files)

}

i <- 80
zone_shp_temp <- zone_shp[i, ]
county_shp_temp <- county_2020_shp[intersections[[i]], ]
map <- ggplot() +
  geom_sf(
    data = county_shp_temp,
    lwd = 0.5
  ) +
  geom_sf(
    data = zone_shp_temp,
    lwd = 0.5,
    alpha = 0,
    color = "red"
  )
ggsave(paste0("/users/garyschlauch/downloads/map.png"))



# Intersect storm warnings with counties ---------------------------------------

