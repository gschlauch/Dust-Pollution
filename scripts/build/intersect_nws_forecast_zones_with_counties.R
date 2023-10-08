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
xwalk_df <- data.frame()

# Get forecast zone files
zone_files <- list.files(
  paste0(path_data_raw, "/dust_storms/forecast_zones"), 
  full.names = TRUE
)

# Loop through forecast zones
for (i in 5:length(zone_files)) {# (i in 1:length(zone_files)) {
  
  print(paste0("File ", i, "/", length(zone_files)))
  
  # Remove files in the temporary directory
  temp_files <- list.files(path_temp, full.names = TRUE)
  file.remove(temp_files)
  
  # Unzip and load
  print("Prepping zone shapefile...")
  unzip(zone_files[i], exdir = path_temp)
  zone_shp_filename <- list.files(path_temp, pattern = ".shp")
  zone_shp <- st_read(paste0(path_temp, "/", zone_shp_filename)) 
  
  # Some of the shapefiles are missing a CRS. Since all of the other shapefiles
  # had NAD83 as the CRS, I guess that the missing CRS is NAD83
  crs <- st_crs(zone_shp)[[1]]
  if (!is.na(crs)) {
    if (crs != "NAD83") {
      stop("The CRS is not NAD83")
    }
  } else {
    zone_shp <- zone_shp %>% st_set_crs("NAD83")
  }
  
  # Basic tidying
  zone_shp <- zone_shp %>%
    dplyr::select(STATE, ZONE, NAME) %>%
    dplyr::rename(zone_state = STATE, zone_fips = ZONE, zone_name = NAME) %>%
    mutate_at(c("zone_state", "zone_fips", "zone_name"), ~tidy_string(.)) %>%
    mutate(zone_state = str_to_upper(zone_state)) %>%
    filter(!(zone_state %in% c("ak", "vi", "pr", "gu", "hi", "mp", "tt", "as"))) %>%
    filter(!(is.na(zone_fips))) %>%
    mutate(zone_filename = zone_shp_filename) %>%
    st_transform("WGS84") %>%
    st_make_valid()
  
  # Union the geometries for zones that are listed twice. This is annoying since
  # st_union isn't working for some of the geometries. However, the ones it isn't
  # working for are already unique geometries (ie, only have 1 row in zone_shp).
  # So I split the process into two parts here.
  print("Unioning geometries within state-fips-name")
  n_distinct_zones <- nrow(distinct(as.data.frame(zone_shp), zone_state, zone_fips, zone_name))
  zone_shp <- zone_shp %>% 
    group_by(zone_state, zone_fips, zone_name, zone_filename) %>%
    dplyr::mutate(obs = n()) %>%
    ungroup()
  zone_shp_1 <- zone_shp %>% filter(obs == 1)
  zone_shp_2 <- zone_shp %>% 
    filter(obs > 1) %>%
    group_by(zone_state, zone_fips, zone_name, zone_filename) %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_make_valid()
  zone_shp <- bind_rows(zone_shp_1, zone_shp_2)
  if (nrow(zone_shp) != n_distinct_zones) {
    stop("I don't have the correct number of zones after doing the union")
  }
  
  # Loop through states
  for (state in state_abbreviations) {
    
    print(paste0("Processing for : ", state))
    
    # Filter the zone shapefile to just that state
    zone_shp_state <- zone_shp %>% filter(zone_state == state)
    
    # Get the number of unique zones to compare at the end of this loop
    n_unique_zone_shp_state <- as.data.frame(zone_shp_state) %>% 
      dplyr::select(zone_state, zone_fips, zone_name) %>%
      n_distinct()
    
    # Filter the county shapefile to just that state and the surrounding states
    county_shp_state <- county_shp %>% 
      filter(stabv %in% get_neighboring_states(state))
    
    # Get a list of county indices that intersect at all with each zone. For some
    # annoying reason, I can't get one of the geometries to be valid for 
    # "MN" in the file "z_01ap14.shp". to handle this case, I set sf_use_s2 to false
    if (state == "MN" & zone_shp_filename == "z_01ap14.shp") {
      zone_shp_state_bad <- zone_shp_state[96, ]
      zone_shp_state <- zone_shp_state[-96, ]
    }
    
    intersections_list <- st_intersects(zone_shp_state, county_shp_state)
    
    # For each zone, get the polygon that is the intersection of zone-counties
    intersections_shp <- st_intersection(
      zone_shp_state[1, ], county_shp_state[intersections_list[[1]], ]
    )
    if (nrow(zone_shp_state) > 1) {
      for (k in 2:nrow(zone_shp_state)) {
        
        # These ones gives an error for whatever reason but it works when I use s2.
        # I also get a warning if I do the intersection in latitude/longitude, so
        # I transform to planar as well
        bad_cases <- 
          (k == 76 & state == "NY" & zone_shp_filename == "z_01de10.shp") |
          (k == 94 & state == "NC" & zone_shp_filename == "z_01de10.shp")
        if (bad_cases) {
          sf_use_s2(FALSE)
          intersections_shp_k <- st_intersection(
            st_transform(zone_shp_state[k, ], 3857), 
            st_transform(county_shp_state[intersections_list[[k]], ], 3857)
          ) %>% st_transform(4326)
          sf_use_s2(TRUE)
        } else { # Do it normally for the rest
          intersections_shp_k <- st_intersection(
            zone_shp_state[k, ], county_shp_state[intersections_list[[k]], ]
          )
        }
        intersections_shp <- bind_rows(intersections_shp, intersections_shp_k)
      }
    }
    
    # Add in the "bad" geometries
    # This one works if I project to a different CRS
    if (state == "MN" & zone_shp_filename == "z_01ap14.shp") {
      zone_shp_state_bad <- zone_shp_state_bad %>% st_transform(3857) %>% st_make_valid()
      county_shp_state_bad <- county_shp_state %>% st_transform(3857)
      intersections_shp_bad <- st_intersection(zone_shp_state_bad, county_shp_state_bad) %>%
        st_transform(4326)
      intersections_shp <- bind_rows(intersections_shp, intersections_shp_bad)
    }
    
    # Compute the percent of area overlap between the zones and counties. That is
    # (area_overlap / area_county) * 100
    intersections_shp <- intersections_shp %>%
      st_make_valid() %>%
      mutate(
        intersection_area = st_area(.),
        pct_county_overlap = as.numeric((intersection_area / cntyarea) * 100)
      )
    
    # Convert to a dataframe
    intersections_df <- as.data.frame(intersections_shp) %>%
      dplyr::select(zone_state, zone_fips, zone_name, zone_filename, stabv, 
                    stfp, cntyfp, pct_county_overlap)
    
    # Check that pct_county_overlap is between 0 and 1
    values <- intersections_df$pct_county_overlap
    if (any(is.na(values)) | any(values < 0) | any(values > 100.1)) {
      stop("pct_county_overlap is not between 0 and 100 or is missing")
    }
    intersections_df <- intersections_df %>%
      mutate(pct_county_overlap = ifelse(pct_county_overlap > 100, 100, pct_county_overlap))
    
    # Check that the data are uniquely identified by zone-county
    check_df_unique_by(intersections_df, zone_state, zone_fips, stfp, cntyfp)
    
    # Check that I have the correct number of unique zones comparing the shapefile
    # and final dataframe
    n_unique_intersections_df <- intersections_df %>%
      dplyr::select(zone_state, zone_fips) %>%
      n_distinct()
    if (n_unique_zone_shp_state != n_unique_intersections_df) {
      stop("The number of unique zones is incorrect in the final dataframe compared to the shapefile")
    }
    
    # Append to the final crosswalk
    xwalk_df <- bind_rows(xwalk_df, intersections_df)
    
  }
}

row.names(xwalk_df) <- NULL
write_csv(xwalk_df, paste0(path_data_int, "/Crosswalks/xwalk_NWS_forecast_zones_to_counties.csv"))



# # Quick maps to check the output
# zone_filename_i <- "z_01ap08.shp"
# distinct_zones <- xwalk_df %>% 
#   filter(zone_filename == zone_filename_i) %>%
#   distinct(zone_state, zone_fips)
# rand <- sample(1:nrow(distinct_zones), 20, replace = F)
# for (i in rand) {
# 
#   zone_state_i <- distinct_zones$zone_state[i]
#   zone_fips_i <- distinct_zones$zone_fips[i]
#   zone_shp_temp <- zone_shp %>%
#     filter(zone_state == zone_state_i, zone_fips == zone_fips_i)
#   county_shp_temp <- xwalk_df %>%
#     dplyr::filter(zone_state == zone_state_i, zone_fips == zone_fips_i) %>%
#     dplyr::select(stfp, cntyfp, pct_county_overlap) %>%
#     left_join(county_shp, by = c("stfp", "cntyfp")) %>%
#     mutate(overlap_threshold = ifelse(pct_county_overlap >= 5, 1, 0))
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
#       lwd = 0.5,
#       alpha = 0,
#       color = "red"
#     )
#   ggsave(paste0("/users/garyschlauch/downloads/map_", i, ".png"))
# }

