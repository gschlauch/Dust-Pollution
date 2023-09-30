#-------------------------------------------------------------------------------
# Combine the VIIRS dust data
# Written by: Gary Schlauch
# Combine the daily dust data from VIIRS
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Functions --------------------------------------------------------------------

clean_dust_file <- function(filepath) {
  df <- read_csv(filepath) %>%
    mutate_all(as.integer) %>%
    mutate(date = ymd(str_sub(filepath, -12, -5)))
  return(df)
}

# Combine dust data ------------------------------------------------------------

# Get the full filepaths of the processed dust data
year_start <- 2012
year_end <- 2022
years <- as.character(year_start:year_end)

filepaths <- c()
for (year in years) {
  dir <- paste0(path_data_int, "/viirs/", year)
  files <- list.files(dir, full.names = TRUE, pattern = ".csv")
  filepaths <- c(filepaths, files)
}

# Append the dust data
df_dust <- clean_dust_file(filepaths[1])
for (filepath in filepaths[-1]) {
  
  df <- read_csv(filepath) %>%
    mutate_all(as.integer) %>%
    mutate(date = ymd(str_sub(filepath, -12, -5)))
  df_dust <- bind_rows(df_dust, df)
}
rm(df)

df_dust <- df_dust %>%
  dplyr::rename(grid_id_0p1latlon = ID)

# Output
write_csv(df_dust, paste0(path_data_int, "/viirs/panel_dustgrid_0p1deg_latlon.csv"))


