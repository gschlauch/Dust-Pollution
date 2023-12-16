#-------------------------------------------------------------------------------
# Process NWS dust storm warnings warnings data
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Intersect warnings with counties ---------------------------------------------

# Load the county shapefile
county_shp <- st_read(paste0(path_data_int, "/shapefiles/census/2010/county/US_county_2010.shp")) %>%
  dplyr::select(-cntyname)

# Initialize a warning-to-county 1:m crosswalk. This will contain all warning files
# and is updated in the for loop below
xwalk_df <- data.frame()

# Loop through storm warning files
years <- as.character(2006:2022)
for (year in years) {
  
  print(paste0("Year: ", year))
  
  print("Loading and prepping warnings shapefile")
  filename <- paste0(year, "/wwa_",  year, "01010000_", year, "12312359.shp")
  warning_shp <- st_read(paste0(path_data_raw, "/dust_storms/NWS/storm_warnings/", filename)) %>%
    filter(PHENOM == "DS", SIG == "W") %>% #DU
    dplyr::select(WFO, INIT_ISS, EXPIRED, PHENOM, SIG, geometry) %>%
    st_transform(5070) %>% # better to use a projection for this purpose
    st_make_valid() %>%
    dplyr::distinct() # remove duplicates
  names(warning_shp) <- str_to_lower(names(warning_shp))
  print(nrow(warning_shp))
  
  # Union warnings that have multiple rows in the shapefile. I split this into 
  # two parts (only 1 row vs >1 row for a given warning) for speed and to make 
  # sure nothing goes weird with the warnings that
  # only have 1 row when doing the union
  n_distinct_warnings <- as.data.frame(warning_shp) %>%
    dplyr::distinct(wfo, init_iss, expired, phenom, sig) %>%
    nrow()
  warning_shp <- warning_shp %>% 
    group_by(wfo, init_iss, expired, phenom, sig) %>%
    dplyr::mutate(obs = n()) %>%
    ungroup()
  warning_shp_1 <- warning_shp %>% filter(obs == 1)
  warning_shp_2 <- warning_shp %>% filter(obs > 1) %>%
    group_by(wfo, init_iss, expired, phenom, sig) %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_make_valid()
  warning_shp <- bind_rows(warning_shp_1, warning_shp_2)
  if (nrow(warning_shp) != n_distinct_warnings) {
    stop("I don't have the correct number of warnings after doing the union")
  }
  
  # Get a list of county indices that intersect at all with each warning polygon
  print("Intersecting warnings with counties (binary)")
  intersections_list <- st_intersects(warning_shp, county_shp)
  
  # For each warning, get the polygon that is the intersection of warning-counties
  print("Obtaining actual intersection polygons")
  intersections_shp <- st_intersection(
    warning_shp[1, ], county_shp[intersections_list[[1]], ]
  )
  if (nrow(warning_shp) > 1) {
    for (k in 2:nrow(warning_shp)) {
      intersections_shp_k <- st_intersection(
        warning_shp[k, ], county_shp[intersections_list[[k]], ]
      )
      intersections_shp <- bind_rows(intersections_shp, intersections_shp_k)
    }
  }

  # Compute the percent of area overlap between the warnings and counties. That is
  # (area_overlap / area_county) * 100
  print("Computing areas")
  intersections_shp <- intersections_shp %>%
    st_make_valid() %>%
    mutate(
      intersection_area = st_area(.),
      pct_county_overlap = as.numeric((intersection_area / cntyarea) * 100)
    )
  
  print("Wrapping up")
  intersections_df <- as.data.frame(intersections_shp) %>%
    dplyr::select(-c(cntyarea, intersection_area, geometry))
  
  # Check that pct_county_overlap is between 0 and 1
  values <- intersections_df$pct_county_overlap
  if (any(is.na(values)) | any(values < 0) | any(values > 100.1)) {
    stop("pct_county_overlap is not between 0 and 100 or is missing")
  }
  intersections_df <- intersections_df %>%
    mutate(pct_county_overlap = ifelse(pct_county_overlap > 100, 100, pct_county_overlap))
  
  # Check that the data are uniquely identified by warning-county
  check_df_unique_by(intersections_df, wfo, init_iss, expired, phenom, sig, stfp, cntyfp)
  
  # Add the filename
  intersections_df <- intersections_df %>% mutate(warning_filename = filename)
  
  # Append to the final crosswalk
  xwalk_df <- bind_rows(xwalk_df, intersections_df)
  
}

row.names(xwalk_df) <- NULL

# Create R date-time variables (all times are already in UTC)
xwalk_df_final <- xwalk_df %>%
  mutate(
    warning_start_datetime = ymd_hm(init_iss),
    warning_end_datetime = ymd_hm(expired)
  ) %>%
  dplyr::select(-c(init_iss, expired, obs))

# Drop possible duplicates for a given county-warning within and across files. 
# This can occur when two WFOs issue a warning for different parts of a county. 
# In these cases, I choose the larger county overlap area. I don't care about which
# WFO issued the warning
xwalk_df_final <- xwalk_df_final %>%
  dplyr::select(-c(wfo, warning_filename)) %>%
  group_by(
    stfp, cntyfp, warning_start_datetime, warning_end_datetime, phenom, sig
  ) %>%
  arrange(desc(pct_county_overlap)) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Output
write_csv(xwalk_df_final, paste0(path_data_int, "/Dust_storms/NWS_dust_storm_warnings_cleaned.csv"))


