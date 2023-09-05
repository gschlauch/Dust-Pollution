#-------------------------------------------------------------------------------
# Combine the VIIRS dust data
# Written by: Gary Schlauch
# Combine the daily dust data from VIIRS
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

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
  dir <- paste0(path_int_viirs, "/", year)
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

df_dust %>%
  dplyr::rename(grid_id_0p1latlon = ID)


