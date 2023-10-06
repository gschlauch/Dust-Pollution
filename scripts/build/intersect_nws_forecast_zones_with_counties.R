#-------------------------------------------------------------------------------
# Intersect the NWS forecast zones and storm warnings with counties
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Intersect forecast zones with counties ---------------------------------------

# Initialize vector of state abbreviations
state_abbreviations <- c(
  "AL", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
  "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
  "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
  "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

# Load the county shapefile
county_shp <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp")) %>%
  dplyr::select(-cntyname)

# Initialize a zone-to-county 1:m crosswalk. This will contain all zone files
# and is updated in the for loop below
xwalk <- data.frame()

# Get forecast zone files
zone_files <- list.files(
  paste0(path_data_raw, "/nws/forecast_zones"), 
  full.names = TRUE
)

# Loop through forecast zones
for (i in 1:1) { #(i in 1:length(zone_files)) {
  
  print(paste0("File ", i, "/", length(zone_files)))
  
  # Remove files in the temporary directory
  temp_files <- list.files(path_temp, full.names = TRUE)
  file.remove(temp_files)
  
  # Unzip, load, and clean the zones file
  print("Cleaning zone shapefile...")
  unzip(zone_files[i], exdir = path_temp)
  zone_shp_filename <- list.files(path_temp, pattern = ".shp")
  zone_shp <- st_read(paste0(path_temp, "/", zone_shp_filename)) %>%
    dplyr::select(STATE, ZONE, NAME) %>%
    dplyr::rename(zone_state = STATE, zone_fips = ZONE, zone_name = NAME) %>%
    mutate_at(c("zone_state", "zone_fips", "zone_name"), ~tidy_string(.)) %>%
    mutate(zone_state = str_to_upper(zone_state)) %>%
    filter(!(zone_state %in% c("ak", "vi", "pr", "gu", "hi", "mp", "tt", "as"))) %>%
    filter(!(is.na(zone_fips))) %>%
    mutate(zone_filename = zone_shp_filename) %>%
    st_transform("WGS84") %>%
    st_make_valid()
  
  # Loop through states
  start_time <- Sys.time()
  for (state in state_abbreviations) {
    
    print(paste0("Processing for : ", state))
    
    # Filter the zone shapefile to just that state
    zone_shp_state <- zone_shp %>% filter(zone_state == state)
    
    # Filter the county shapefile to just that state and the surrounding states
    county_shp_state <- county_shp %>% 
      filter(stabv %in% get_neighboring_states(state))
    
    # Get a list of county indices that intersect at all with each zone
    intersections_list <- st_intersects(zone_shp_state, county_shp_state)
    
    # For each zone, get the polygon that is the intersection of zone-counties
    intersections_shp <- st_intersection(
      zone_shp_state[1, ], county_shp_state[intersections_list[[1]], ]
    )
    if (nrow(zone_shp_state) > 1) {
      for (k in 2:nrow(zone_shp_state)) {
        intersections_shp_k <- st_intersection(
          zone_shp_state[k, ], county_shp_state[intersections_list[[k]], ]
        )
        intersections_shp <- bind_rows(intersections_shp, intersections_shp_k)
      }
    }
    
    # Compute the percent of area overlap between the zones and counties. That is
    # (area_overlap / area_county) * 100
    intersections_shp <- intersections_shp %>%
      mutate(
        intersection_area = st_area(.),
        pct_county_overlap = as.numeric((intersection_area / cntyarea) * 100)
      )
    
    # Convert to a dataframe
    intersections_df <- as.data.frame(intersections_shp) %>%
      dplyr::select(zone_state, zone_fips, zone_name, zone_filename, stabv, 
                    stfp, cntyfp, pct_county_overlap)
    
    # Add to the final crosswalk
    xwalk <- bind_rows(xwalk, intersections_df)
    
  }
}

row.names(xwalk) <- NULL
write_csv(paste0(path_int, "/walks/xwalk_NWS_forecast_zones_to_counties.csv"))

# Quick maps to check the output
xwalk_temp <- xwalk %>% 
  arrange(desc(pct_county_overlap)) %>%
  filter(row_number() %in% c(1:10, 90:110, 500:510, 200:210))
distinct_zones <- xwalk_temp %>% distinct(zone_state, zone_fips)
for (i in 22:22) { #(i in 1:nrow(distinct_zones)) {
  
  zone_state_i <- distinct_zones$zone_state[i]
  zone_fips_i <- distinct_zones$zone_fips[i]
  zone_shp_temp <- zone_shp %>%
    filter(zone_state == zone_state_i, zone_fips == zone_fips_i)
  county_shp_temp <- xwalk %>%
    dplyr::filter(zone_state == zone_state_i, zone_fips == zone_fips_i) %>%
    dplyr::select(stfp, cntyfp, pct_county_overlap) %>%
    left_join(county_shp, by = c("stfp", "cntyfp")) %>%
    mutate(overlap_threshold = ifelse(pct_county_overlap >= 5, 1, 0))

  map <- ggplot() +
    geom_sf(
      data = county_shp_temp,
      aes(fill = overlap_threshold, geometry = geometry),
      lwd = 0.5,
      color = "green"
    ) +
    geom_sf(
      data = zone_shp_temp,
      lwd = 0.5,
      alpha = 0,
      color = "red"
    )
  ggsave(paste0("/users/garyschlauch/downloads/map_", i, ".png"))
}


