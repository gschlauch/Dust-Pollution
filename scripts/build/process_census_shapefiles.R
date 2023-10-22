#-------------------------------------------------------------------------------
# Clean Census shapefiles
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# QA functions -----------------------------------------------------------------

# Check number of rows correct
qa_check_nrows <- function(shp, target_num, id_vars) {
  if (nrow(shp) != target_num) {
    stop("Incorrect number of units")
  }
  if (n_distinct(shp[[id_vars[1]]], shp[[id_vars[2]]]) != target_num) {
    stop("Incorrect number of unique units")
  }
}

# Check CRS is as expected
qa_check_crs <- function(shp, target_crs) {
  if (st_crs(shp)[[1]] != target_crs) {
    stop("Unexpected CRS")
  }
}

# Check the id_vars are as expected
qa_check_id_vars <- function(shp) {
  
  df <- as.data.frame(shp) %>% dplyr::select(-geometry)
  
  # Check all id_vars are strings
  sapply(df, is.character) %>% all()

  # Check id_vars have correct number of digits
  if (all(nchar(df$stfp) != 2)) {
    stop("Check stfp var")
  }

  if ("cntyfp" %in% names(df)) {
    if (all(nchar(df$cntyfp) != 3)) {
      stop("Check cntyfp var")
    }
  }
}

target_crs <- "USA_Contiguous_Albers_Equal_Area_Conic"

# Process 2020 County boundaries -----------------------------------------------

filepath <- paste0(path_data_raw, "/census/2020/county/US_county_2020.shp")
shp <- st_read(filepath) %>%
  dplyr::rename(stfp = STATEFP, cntyfp = COUNTYFP, cntyname = NAME) %>%
  mutate(
    cntyfp = str_trim(str_to_lower(cntyfp)),
    stabv = get_state_abbreviation_from_fips(stfp),
    cntyname = str_trim(str_to_lower(cntyname))
    ) %>%
  arrange(stfp, cntyfp) %>%
  dplyr::select(stabv, stfp, cntyfp, cntyname, geometry)

shp <- shp %>%
  dplyr::filter(!(stfp %in% c("60", "64", "66", "68", "69", "70", "72", "74", "78")))

target_num <- 3143 #https://www.census.gov/geographies/reference-files/time-series/geo/tallies.2010.html#list-tab-1626061381
qa_check_nrows(shp, target_num = target_num, id_vars = c("stfp", "cntyfp"))
qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- shp %>%
  dplyr::filter(!(stfp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$stfp) != 49) {
  stop("Check number of states")
}

# shp <- st_transform(shp, "WGS84") %>%
#   st_make_valid()
shp <- st_transform(shp, 5070) %>%
  st_make_valid()

shp <- shp %>%
  mutate(cntyarea = st_area(.)) %>%
  dplyr::select(everything(), geometry)

filepath_out <- paste0(path_data_int, "/census/2020/county/US_county_2020.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)

# Process 2020 state boundaries ------------------------------------------------

filepath <- paste0(path_data_raw, "/census/2020/state/US_state_2020.shp")
shp <- st_read(filepath) %>%
  dplyr::rename(stabv = STUSPS, stfp = GEOID) %>%
  mutate_at(c("stabv", "stfp"), ~ str_trim(.)) %>%
  filter(!(stfp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78"))) %>%
  dplyr::select(stabv, stfp, geometry)

qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

# shp <- st_transform(shp, "WGS84") %>%
#   st_make_valid()
shp <- st_transform(shp, 5070) %>%
  st_make_valid()

filepath_out <- paste0(path_data_int, "/census/2020/state/US_state_2020.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)

# # Mapper --------------------------------------------------
# 
# state2000 <- st_read(paste0(path_data_int, "/census/2000/state/US_state_2000.shp"))
# cnty2000 <- st_read(paste0(path_data_int, "/census/2000/county/US_county_2000.shp"))
# tract2000 <- st_read(paste0(path_data_int, "/census/2000/tract/US_tract_2000.shp"))
# puma2000 <- st_read(paste0(path_data_int, "/census/2000/puma/US_puma_2000.shp"))
# 
# state2010 <- st_read(paste0(path_data_int, "/census/2010/state/US_state_2010.shp"))
# cnty2010 <- st_read(paste0(path_data_int, "/census/2010/county/US_county_2010.shp"))
# tract2010 <- st_read(paste0(path_data_int, "/census/2010/tract/US_tract_2010.shp"))
# puma2010 <- st_read(paste0(path_data_int, "/census/2010/puma/US_puma_2010.shp"))
# 
# map <- ggplot() +
#   geom_sf(
#     data = cnty2000,
#     color = "red",
#     lwd = 0.5
#   ) +
#   geom_sf(
#     data = tract2000,
#     lwd = 0.1,
#     alpha = 0
#   )
# 
# file_out <- paste0("/users/garyschlauch/downloads/map.png")
# ggsave(file_out)


