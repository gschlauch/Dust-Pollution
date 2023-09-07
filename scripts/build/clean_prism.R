#-------------------------------------------------------------------------------
# Process Schlenker and Roberts (2009) PRISM data
# Written by: Garrison Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

# Create a crosswalk of PRISM grid cell numbers to my grid cell numbers --------

# Get the lat/lons of the PRISM grid cells
# (they are 2.5 miles x 2.5 miles, so this will be a close enough approx later on)
files <- list.files(paste0(path_data_raw, "/prism/year2008"))
n <- length(files)

df_latlons <- data.frame(matrix(ncol = 3, nrow = 0))
names(df_latlons) <- c("grid_num", "lat", "lon")

for (i in 1:n) {
  
  print(paste0("Processing file ", i, "/", n))
  
  df <- read_dta(file, col_select = "gridNumber") %>%
    distinct(gridNumber) %>%
    dplyr::rename(grid_num = gridNumber)

  # Apply transformation received from correspondence with Wolfram Schlenker
  df$lat <- 49.9375 + (1 / 48) - (ceiling(df$grid_num / 1405) / 24)
  df$lon <- -125 + (((df$grid_num - 1) %% 1405) / 24)
  
  df_latlons <- rbind(df_latlons, df)
  
}

# Create a crosswalk of PRISM grid cell numbers to my grid cell numbers --------





