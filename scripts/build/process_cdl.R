
#-------------------------------------------------------------------------------
# Clean CDL data
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

# Adjust the census unit and year
census_unit <- "county"
census_year <- "2000"
shp_name <- paste0("US_", census_unit, "_", census_year)

# Define a function for whether an area is cropland or not
create_binary_cropland <- function(x) {
  ifelse(x %in% c(1:60, 66:77, 204:254), 1, 0)
}

# Get raster file paths
subdirectories <- list.dirs(paste0(path_data_raw, "/usda/cdl"), full.names = TRUE, recursive = FALSE)
raster_file_paths <- c()
for (subdir in subdirectories) {
  files <- list.files(subdir, full.names = TRUE, pattern = ".tif$")
  raster_file_paths <- c(raster_file_paths, files)
}

# Get the year of the raster file
year_names <- str_sub(raster_file_paths, -17, -14)

# Load a sample CDL raster to get the projection
sample_raster <- raster(raster_file_paths[1])

# Load shapefile for the units we want to compute % cropland for
shp_full <- st_read(paste0(path_int_census, "/", census_year, "/", census_unit, "/", shp_name, ".shp")) %>%
  st_transform(projection(sample_raster))

# Loop over states in the shapefile
states <- unique(shp_full$statefp)





states <- c("44", "09")

i <- 1
n <- length(states)
for (state in states) {

  # Select the state from the shapefile
  print(paste0("Loading shapefile for state ", i, "/", n))
  shp <- shp_full %>%
    filter(statefp == state)

  # Load a sample CDL raster to get a pixel-to-census ID crosswalk
  sample_raster <- raster(raster_file_paths[1])
  
  # Crop the raster
  sample_raster <- crop(sample_raster, shp)
  
  # Get a shapefile unit ID-to-grid cell crosswalk, along with the coverage_fraction
  # of each grid cell
  print(paste0("Getting ID-to-grid cell crosswalk for state ", i, "/", n))
  weights <- exactextractr::exact_extract(
    sample_raster, shp, include_cell = TRUE
  )
  weights <- dplyr::bind_rows(weights, .id = "ID")
  weights <- weights %>% dplyr::select(-value) %>% dplyr::rename(weight = coverage_fraction)
  rm(sample_raster)
  
  # Load and stack all the cdl rasters across years
  print("Stacking all rasters")
  r <- raster::stack(raster_file_paths)
  
  # Crop rasters to match the shapefile extent
  print("Cropping rasters")
  r <- raster::crop(r, shp)
  names(r) <- paste0("t", year_names)
  
  # Get values from the raster stack
  print("Prepping for merge")
  vals <- as.data.table(getValues(r))
  rm(r)
  vals$cell <- 1:dim(vals)[1]
  vals <- melt(vals, id.vars = c("cell"))
  colnames(vals)[2] <- "year"
  
  # Merge crosswalk onto raster stack so we know which pixels connect to which
  # polygons (same across year-months)
  print("Merging raster values for each year onto census ID-raster cell crosswalk")
  valsM <- merge(vals, weights, by = c("cell"), allow.cartesian = T)
  setcolorder(valsM, c("ID", "year", "cell", "value", "weight"))
  rm(vals, weights)
  
  # Generate binary indiator for whether a value is cropland or not
  valsM <- as.data.frame(valsM)
  valsM <- valsM %>%
    mutate(is_cropland = create_binary_cropland(value)) %>%
    dplyr::select(-value)
  
  # Do a weighted mean over cells for each ID (Census unit ID) and year-month
  print("Computing weighted mean over cells (currently set to use equal weights)")
  means <- valsM %>%
    group_by(ID, year) %>%
    summarize(pct_cropland = weighted.mean(is_cropland, weight, na.rm = T) * 100) %>%
    ungroup()
  rm(valsM)
  
  # Merge the shapefile information back onto the calculated data
  print("Getting shapefile ID info")
  shp$ID <- 1:nrow(shp)
  out <- merge(means, shp, by = "ID", all.x = T) %>%
    as.data.frame() %>%
    dplyr::select(-c(ID, geometry))
  rm(means, shp)
  
  # Tidy
  out <- out %>%
    mutate(year = as.numeric(str_sub(year, 2, ))) %>%
    dplyr::select(statefp, countyfp, year, pct_cropland) %>%
    arrange(statefp, countyfp, year)

  # Add a leading character to the Census id variables to ensure they write as strings
  if (shp_name == "US_state_2000") {
    out <- out %>%
      mutate(statefp = paste0("G", statefp))
  }
  if (shp_name %in% c("US_county_2000", "US_county_2010")) {
    out <- out %>%
      mutate(statefp = paste0("G", statefp),
             countyfp = paste0("G", countyfp))
  }
  if (shp_name %in% c("US_tract_2000", "US_tract_2010")) {
    out <- out %>%
      mutate(statefp = paste0("G", statefp),
             countyfp = paste0("G", countyfp),
             tractfp = paste0("G", tractfp))
  }
  
  # Output
  print("Saving...")
  file_out <- paste0(path_temp, "/", shp_name, "_s", state, "_cdl.csv")
  write_csv(out, file_out)
  rm(out)
  
  i <- i + 1

}

# Append the files for each state
temp_files <- list.files(path_temp, full.names = TRUE, pattern = "_cdl.csv$")
df_final <-  read_csv(temp_files[1])
for (f in temp_files[-1]){
  df <- read_csv(f)
  df_final <- rbind(df_final, df)
  file.remove(f)
}
file.remove(temp_files[1])

# Output
write_csv(paste0(path_int_usda, "/cdl/", shp_name, "_cdl.csv"))

