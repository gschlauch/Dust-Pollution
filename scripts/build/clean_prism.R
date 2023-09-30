#-------------------------------------------------------------------------------
# Process Schlenker and Roberts (2009) PRISM data
# Written by: Garrison Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

# # Create a crosswalk of PRISM grid cell numbers to my grid cell numbers --------
# 
# # Get the lat/lons of the PRISM grid cells
# # (they are 2.5 miles x 2.5 miles, so this will be a close enough approx later on)
# files <- list.files(paste0(path_data_raw, "/prism/year2008"))
# n <- length(files)
# 
# df_latlons <- data.frame(matrix(ncol = 3, nrow = 0))
# names(df_latlons) <- c("grid_num", "lat", "lon")
# 
# for (i in 1:n) {
#   
#   print(paste0("Processing file ", i, "/", n))
#   
#   df <- read_dta(file, col_select = "gridNumber") %>%
#     distinct(gridNumber) %>%
#     dplyr::rename(grid_num = gridNumber)
# 
#   # Apply transformation received from correspondence with Wolfram Schlenker
#   df$lat <- 49.9375 + (1 / 48) - (ceiling(df$grid_num / 1405) / 24)
#   df$lon <- -125 + (((df$grid_num - 1) %% 1405) / 24)
#   
#   df_latlons <- rbind(df_latlons, df)
#   
# }

# Create a crosswalk of PRISM grid cell numbers to my grid cell numbers --------


# Figure out which counties Schlenker is using ---------------------------------

fips_codes <- list.files(path = paste0(path_data_raw, "/prism/year2008"))
df <- as_tibble(fips_codes) %>%
  mutate(value = str_sub(value, 5, -5)) %>%
  mutate(value = ifelse(nchar(value) == 4, paste0("0", value), value)) %>%
  mutate(statefp = str_sub(value, 1, 2),
         countyfp = str_sub(value, 3, -1)) %>%
  dplyr::select(-value)

all(nchar(df$statefp) == 2)
all(nchar(df$countyfp) == 3)

county2000_df <- st_read(paste0(path_data_int, "/census/2000/county/US_county_2000.shp")) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

df_join <- stata.merge(df, county2000_df, by = c("statefp", "countyfp"))
count(df_join, "merge")
df_join %>%
  filter(merge == 2)


county2010_df <- st_read(paste0(path_data_int, "/census/2010/county/US_county_2010.shp")) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

df_join <- stata.merge(df, county2010_df, by = c("statefp", "countyfp"))
count(df_join, "merge")





df <- read_csv("/Users/garyschlauch/Downloads/nhgis0013_csv/nhgis0013_ts_nominal_county.csv") %>%
  filter(!(STATE %in% c("Alaska", "Alaska Territory", "Hawaii", "Hawaii Territory", "Puerto Rico")))

dplyr::count(df, STATE) %>% nrow()

df %>%
  filter(!is.na(A00AA2010)) %>%
  nrow()
