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
state_shp <- st_read(paste0(path_data_int, "/census/2020/state/US_state_2020.shp"))

# Get forecast zone files
zone_files <- list.files(
  paste0(path_data_raw, "/dust_storms/forecast_zones"), 
  full.names = TRUE
)

# Initialize dataframe for number of distinct zones
num_unique_zones_df <- data.frame(
  zone_filename = rep(NA, length(zone_files)),
  unique_zones = rep(NA, length(zone_files))
)

# Loop through forecast zones
for (i in 55:length(zone_files)) {
  
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
      stop("The non-missing default CRS is not NAD83")
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
    filter(!(zone_state %in% c("AK", "VI", "PR", "GU", "HI", "MP", "TT", "AS"))) %>%
    filter(!(is.na(zone_fips))) %>%
    mutate(zone_filename = zone_shp_filename) %>%
    st_transform(5070) %>% # better to use a projection for this purpose
    st_make_valid() %>%
    distinct() # remove potential duplicate geometries
  
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
    stop("After doing the union, there isn't 1 row per zone")
  }
  
  
  
  
  num_unique_zones_df[i, "zone_filename"] <- zone_shp_filename
  num_unique_zones_df[i, "unique_zones"] <- n_distinct_zones

  map <- ggplot() +
    geom_sf(
      data = state_shp,
      lwd = 0.5,
      color = "green"
    ) +
    geom_sf(
      data = zone_shp,
      lwd = 0.1,
      alpha = 0,
      color = "red"
    )
  ggsave(paste0("/users/garyschlauch/downloads/map_", i, ".png"))
  
}

num_unique_zones_df