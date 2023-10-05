#-------------------------------------------------------------------------------
# Intersect the NWS forecast zones and storm warnings with counties
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Intersect forecast zones with counties ---------------------------------------

# Load the county shapefile
county_2020_shp <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp")) %>%
  mutate(county_area = st_area(.)) %>%
  dplyr::rename(c_statefp = statefp, c_countyfp = countyfp, county_name = county)
county_2020_shp$county_index <- 1:nrow(county_2020_shp)

# Initialize zone-to-county 1:m crosswalk that I will create below
xwalk <- data.frame()

# Loop through forecast zones
zone_files <- list.files(
  paste0(path_data_raw, "/nws/forecast_zones"), 
  full.names = TRUE
  )
for (i in 1:1) { #(i in 1:length(zone_files)) {
  
  print(paste0("File ", i, "/", length(zone_files)))
  
  # Remove files in the temporary directory
  temp_files <- list.files(path_temp, full.names = TRUE)
  file.remove(temp_files)

  # Unzip, load, and clean the zones file
  unzip(zone_files[i], exdir = path_temp)
  zone_shp_filename <- list.files(path_temp, pattern = ".shp")
  zone_shp <- st_read(paste0(path_temp, "/", zone_shp_filename)) %>%
    dplyr::select(STATE, ZONE, NAME) %>%
    dplyr::rename(zone_state = STATE, zone_fips = ZONE, zone_name = NAME) %>%
    mutate_at(c("zone_state", "zone_fips", "zone_name"), ~tidy_string(.)) %>%
    filter(!(zone_state %in% c("ak", "vi", "pr", "gu", "hi", "mp", "tt", "as"))) %>%
    filter(!(is.na(zone_fips))) %>%
    mutate(zone_filename = zone_shp_filename) %>%
    st_transform("WGS84") %>%
    st_make_valid()
  
  # Obtain polygons that are the intersection of zones and counties
  intersections_shp <- st_intersection(zone_shp, county_2020_shp)
  
  # Compute the percent of area overlap between the zones and counties. That is
  # (area_overlap / area_county) * 100
  intersections_shp <- intersections_shp %>%
    mutate(
      intersection_area = st_area(.),
      pct_county_overlap_with_zone = as.numeric((intersection_area / county_area) * 100)
    )
  
  # Convert to a dataframe
  intersections_df <- as.data.frame(intersections_shp) %>%
    dplyr::select(zone_state, zone_fips, zone_name, zone_filename, 
                  c_statefp, c_countyfp, county_name, county_index, 
                  pct_county_overlap_with_zone)
  
  # Add to the final crosswalk
  xwalk <- bind_rows(xwalk, intersections_df)

}

row.names(xwalk) <- NULL

# # Quick maps to check the output
# distinct_zones <- xwalk %>% distinct(zone_state, zone_fips)
# for (i in 1:nrow(distinct_zones)) {
#   zone_state_str <- distinct_zones$zone_state[i]
#   zone_fips_str <- distinct_zones$zone_fips[i]
#   zone_shp_temp <- zone_shp %>% 
#     filter(zone_state == zone_state_str, zone_fips == zone_fips_str)
#   county_shp_temp <- xwalk %>% 
#     dplyr::filter(zone_state == zone_state_str, zone_fips == zone_fips_str) %>%
#     dplyr::select(county_index, pct_county_overlap_with_zone) %>%
#     left_join(county_2020_shp, by = "county_index") %>%
#     mutate(overlap_threshold = ifelse(pct_county_overlap_with_zone >= 5, 1, 0))
#   
#   map <- ggplot() +
#     geom_sf(
#       data = county_shp_temp,
#       aes(fill = overlap_threshold, geometry = geometry),
#       lwd = 0.5,
#       color = "green"
#     ) +
#     geom_sf(
#       data = zone_shp_temp,
#       lwd = 1,
#       alpha = 0,
#       color = "red"
#     )
#   ggsave(paste0("/users/garyschlauch/downloads/map_", i, ".png"))
# }

# Save the crosswalk
write_csv(paste0(path_int, "/walks/xwalk_NWS_forecast_zones_to_counties.csv"))


