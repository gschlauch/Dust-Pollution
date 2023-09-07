#-------------------------------------------------------------------------------
# Clean Census shapefiles
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

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

  # Check names
  if (!all(names(df) %in% c("statefp", "countyfp", "tractfp", "puma"))) {
    stop("Check column names")
  }

  # Check all id_vars are strings
  sapply(df, is.character) %>% all()

  # Check id_vars have correct number of digits
  if (all(nchar(df$statefp) != 2)) {
    stop("Check statefp var")
  }

  if ("puma" %in% names(df)) {
    if (all(nchar(df$puma) != 5)) {
      stop("Check puma var")
    }
  }

  if ("countyfp" %in% names(df)) {
    if (all(nchar(df$countyfp) != 3)) {
      stop("Check countyfp var")
    }
  }

  if ("tractfp" %in% names(df)) {
    if (all(nchar(df$tractfp) != 6)) {
      stop("Check tractfp var")
    }
  }
}


target_crs <- "USA_Contiguous_Albers_Equal_Area_Conic"

# Process 2000 State boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2000/state/US_state_2000.shp")
shp <- st_read(filepath) %>%
  mutate(statefp = str_sub(NHGISST, end = -2)) %>%
  dplyr::select(statefp, geometry) %>%
  arrange(statefp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

# Also make a conus shapefile
shp_agg <- st_union(shp)

shp <- st_transform(shp, 4326)
shp_agg <- st_transform(shp_agg, 4326)

filepath_out <- paste0(path_data_int, "/census/2000/state/US_state_2000.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)

filepath_out <- paste0(path_data_int, "/census/2000/conus/US_conus_2000.shp")
st_write(shp_agg, filepath_out, delete_dsn = T)
rm(shp_agg, filepath_out)


# Process 2010 State boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2010/state/US_state_2010.shp")
shp <- st_read(filepath) %>%
  dplyr::select(STATEFP10, geometry) %>%
  dplyr::rename(statefp = STATEFP10) %>%
  arrange(statefp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- st_transform(shp, 4326)

filepath_out <- paste0(path_data_int, "/census/2010/state/US_state_2010.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)

# 
# # Process 2000 PUMA boundaries --------------------------------------------------
# filepath <- paste0(path_data_raw, "/census/2000/PUMA/US_puma_2000.shp")
# shp <- st_read(filepath) %>%
#   dplyr::select(STATEFP, PUMA, geometry) %>%
#   arrange(STATEFP, PUMA)
# names(shp) <- tolower(names(shp))
# 
# target_num <- 2071 #https://en.wikipedia.org/wiki/Public_Use_Microdata_Area
# qa_check_nrows(shp, target_num = target_num, id_vars = c("statefp", "puma"))
# qa_check_crs(shp, target_crs)
# qa_check_id_vars(shp)
# 
# shp <- shp %>%
#   dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
# if (n_distinct(shp$statefp) != 49) {
#   stop("Check number of states")
# }
# 
# shp <- st_transform(shp, 4326)
# 
# filepath_out <- paste0(path_data_int, "/census/2000/puma/US_puma_2000.shp")
# st_write(shp, filepath_out, delete_dsn = T)
# rm(filepath, shp, filepath_out)
# 
# 
# # Process 2010 PUMA boundaries --------------------------------------------------
# filepath <- paste0(path_data_raw, "/census/2010/PUMA/ipums_puma_2010.shp")
# shp <- st_read(filepath) %>%
#   dplyr::select(STATEFIP, PUMA, geometry) %>%
#   dplyr::rename(STATEFP = STATEFIP) %>%
#   arrange(STATEFP, PUMA)
# names(shp) <- tolower(names(shp))
# 
# target_num <- 2378 # http://proximityone.com/puma2010.htm
# qa_check_nrows(shp, target_num = target_num, id_vars = c("statefp", "puma"))
# qa_check_crs(shp, target_crs)
# qa_check_id_vars(shp)
# 
# shp <- shp %>%
#   dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
# if (n_distinct(shp$statefp) != 49) {
#   stop("Check number of states")
# }
# 
# shp <- st_transform(shp, 4326)
# 
# filepath_out <- paste0(path_data_int, "/census/2010/puma/US_puma_2010.shp")
# st_write(shp, filepath_out, delete_dsn = T)
# rm(filepath, shp, filepath_out)
# 

# Process 2000 County boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2000/County/US_county_2000.shp")
shp <- st_read(filepath) %>%
  dplyr::select(NHGISST, NHGISCTY, geometry) %>%
  dplyr::mutate(
    statefp = str_sub(NHGISST, end = -2),
    countyfp = str_sub(NHGISCTY, end = -2)
  ) %>%
  dplyr::select(-c(NHGISST, NHGISCTY)) %>%
  arrange(statefp, countyfp)

target_num <- 3141 #https://www.census.gov/geographies/reference-files/time-series/geo/tallies.2000.html#list-tab-1626061381
qa_check_nrows(shp, target_num = target_num, id_vars = c("statefp", "countyfp"))
qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

shp <- st_transform(shp, 4326)

filepath_out <- paste0(path_data_int, "/census/2000/county/US_county_2000.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)


# Process 2010 County boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2010/County/US_county_2010.shp")
shp <- st_read(filepath) %>%
  dplyr::select(STATEFP10, COUNTYFP10, geometry) %>%
  dplyr::rename(statefp = STATEFP10, countyfp = COUNTYFP10) %>%
  arrange(statefp, countyfp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("60", "64", "66", "68", "69", "70", "72", "74", "78")))

target_num <- 3143 #https://www.census.gov/geographies/reference-files/time-series/geo/tallies.2010.html#list-tab-1626061381
qa_check_nrows(shp, target_num = target_num, id_vars = c("statefp", "countyfp"))
qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

shp <- st_transform(shp, 4326)

filepath_out <- paste0(path_data_int, "/census/2010/county/US_county_2010.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)


# Process 2000 Tract boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2000/Tract/US_tract_2000.shp")
shp <- st_read(filepath) %>%
  dplyr::mutate(
    statefp = str_sub(NHGISST, end = -2),
    countyfp = str_sub(NHGISCTY, end = -2),
    tractfp = str_sub(GISJOIN, -6, -1)
  ) %>%
  dplyr::select(statefp, countyfp, tractfp, geometry) %>%
  arrange(statefp, countyfp, tractfp)

qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

shp <- st_transform(shp, 4326)

filepath_out <- paste0(path_data_int, "/census/2000/tract/US_tract_2000.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)


# Process 2010 Tract boundaries --------------------------------------------------
filepath <- paste0(path_data_raw, "/census/2010/Tract/US_tract_2010.shp")
shp <- st_read(filepath) %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, geometry) %>%
  dplyr::rename(statefp = STATEFP10, countyfp = COUNTYFP10, tractfp = TRACTCE10) %>%
  arrange(statefp, countyfp, tractfp)

qa_check_crs(shp, target_crs)
qa_check_id_vars(shp)

shp <- shp %>%
  dplyr::filter(!(statefp %in% c("02", "15", "60", "64", "66", "68", "69", "70", "72", "74", "78")))
if (n_distinct(shp$statefp) != 49) {
  stop("Check number of states")
}

shp <- st_transform(shp, 4326)

filepath_out <- paste0(path_data_int, "/census/2010/tract/US_tract_2010.shp")
st_write(shp, filepath_out, delete_dsn = T)
rm(filepath, shp, filepath_out)

# Mapper --------------------------------------------------

state2000 <- st_read(paste0(path_data_int, "/census/2000/state/US_state_2000.shp"))
cnty2000 <- st_read(paste0(path_data_int, "/census/2000/county/US_county_2000.shp"))
tract2000 <- st_read(paste0(path_data_int, "/census/2000/tract/US_tract_2000.shp"))
puma2000 <- st_read(paste0(path_data_int, "/census/2000/puma/US_puma_2000.shp"))

state2010 <- st_read(paste0(path_data_int, "/census/2010/state/US_state_2010.shp"))
cnty2010 <- st_read(paste0(path_data_int, "/census/2010/county/US_county_2010.shp"))
tract2010 <- st_read(paste0(path_data_int, "/census/2010/tract/US_tract_2010.shp"))
puma2010 <- st_read(paste0(path_data_int, "/census/2010/puma/US_puma_2010.shp"))

map <- ggplot() +
  geom_sf(
    data = cnty2000,
    color = "red",
    lwd = 0.5
  ) +
  geom_sf(
    data = tract2000,
    lwd = 0.1,
    alpha = 0
  )

file_out <- paste0("/users/garyschlauch/downloads/map.png")
ggsave(file_out)


