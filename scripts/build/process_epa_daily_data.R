#-------------------------------------------------------------------------------
# Process daily EPA pollution data for PM10 and PM2.5
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

# Functions --------------------------------------------------------------------

# Clean a single PM10 file
clean_pm10_data <- function(df) {
  df <- df %>%
    dplyr::select(`Site ID`, SITE_LATITUDE, SITE_LONGITUDE, POC, Date, `Daily Mean PM10 Concentration`) %>%
    dplyr::rename(date = Date, site_id = `Site ID`, poc = POC, pm10_mass = `Daily Mean PM10 Concentration`,
                  site_lat = SITE_LATITUDE, site_lon = SITE_LONGITUDE) %>%
    mutate(date = lubridate::mdy(date))
  return(df)
}

# Clean a single PM 2.5 file
clean_pm25_data <- function(df) {
  df <- df %>%
    dplyr::select(`Site ID`, SITE_LATITUDE, SITE_LONGITUDE, POC, Date, `Daily Mean PM2.5 Concentration`) %>%
    dplyr::rename(date = Date, site_id = `Site ID`, poc = POC, pm25_mass = `Daily Mean PM2.5 Concentration`,
                  site_lat = SITE_LATITUDE, site_lon = SITE_LONGITUDE) %>%
    mutate(date = lubridate::mdy(date))
  return(df)
}

# Check that there are no NA values in the PM concentrations
qa_all_pm_nonmissing <- function(df, pm_varname) {
  # pm_varname (str)
  n_missing <- sum(is.na(df[[pm_varname]]))
  if (n_missing != 0) {
    stop("Some PM concentrations are missing")
  }
}

# Check that each site has the same lat/lon over time
qa_stable_coords <- function(df) {
  n_sites <- n_distinct(df$site_id)
  n_sites_coords <-  n_distinct(df$site_id, df$site_lat, df$site_lon)
  if (n_sites != n_sites_coords) {
    stop("Some sites change lat/lons over time")
  }
}

# Get the number of observations by site-year-pollutant


# Create the PM10 panel --------------------------------------------------------

filepath_base <- paste0(path_raw_epa, "/mass/pm10/daily")
files <- list.files(filepath_base, pattern = "^pm10_daily_.*\\.csv$")

# Initialize dataframe to store results
df_pm10 <- read_csv(paste0(filepath_base, "/", files[1])) %>%
  clean_pm10_data()

# Loop through the rest of the files
n <- length(files)
for (i in 2:n) {
  print(file)
  df <- read_csv(paste0(filepath_base, "/", files[i])) %>%
    clean_pm10_data()
  df_pm10 <- bind_rows(df_pm10, df)
}

# Run QA functions
check_df_unique_by(df_pm10, site_id, poc, date)
qa_all_pm_nonmissing(df_pm10, "pm10_mass")
qa_stable_coords(df_pm10)

# Average the results across monitors for each station-day
df_pm10 <- df_pm10 %>%
  group_by(site_id, site_lat, site_lon, date) %>%
  dplyr::summarize(pm10_mass = mean(pm10_mass, na.rm = T)) %>%
  ungroup()

rm(df, filepath_base, files, i, n)

# Create the PM2.5 panel -------------------------------------------------------

filepath_base <- paste0(path_raw_epa, "/mass/pm25/daily")
files <- list.files(filepath_base, pattern = "^pm25_daily_.*\\.csv$")

# Initialize dataframe to store results
df_pm25 <- read_csv(paste0(filepath_base, "/", files[1])) %>%
  clean_pm25_data()

# Loop through the rest of the files
n <- length(files)
for (i in 2:n) {
  print(file)
  df <- read_csv(paste0(filepath_base, "/", files[i])) %>%
    clean_pm25_data()
  df_pm25 <- bind_rows(df_pm25, df)
}

# Run QA functions
check_df_unique_by(df_pm25, site_id, poc, date)
qa_all_pm_nonmissing(df_pm25, "pm25_mass")
qa_stable_coords(df_pm25)

# Average the results across monitors for each station-day
df_pm25 <- df_pm25 %>%
  group_by(site_id, site_lat, site_lon, date) %>%
  dplyr::summarize(pm25_mass = mean(pm25_mass, na.rm = T)) %>%
  ungroup()

rm(df, filepath_base, files, i, n)

# Combine the two panels -------------------------------------------------------

# Check that the site lat/lons are the same across the two panels
df <- rbind.fill(df_pm10, df_pm25)
qa_stable_coords(df)
rm(df)

# Merge the two panels
df_combined <- full_join(df_pm10, df_pm25, by = c("site_id", "site_lat", "site_lon", "date"))
rm(df_pm10, df_pm25)

# Output site-day panel
write_csv(df_combined, paste0(path_int_epa, "/panel_site_day_PMmass.csv"))

# Get the number of observations by site-year-pollutant
df_combined <- df_combined %>%
  mutate(year = year(date))

df_pm10_obs <- df_combined %>%
  dplyr::filter(!is.na(pm10_mass)) %>%
  group_by(site_id, year) %>%
  dplyr::summarize(n_obs_by_site_yr_pm10 = n()) %>%
  dplyr::ungroup()

df_pm25_obs <- df_combined %>%
  dplyr::filter(!is.na(pm25_mass)) %>%
  group_by(site_id, year) %>%
  dplyr::summarize(n_obs_by_site_yr_pm25 = n()) %>%
  dplyr::ungroup()

df_obs <- full_join(df_pm10_obs, df_pm25_obs, by = c("site_id", "year")) %>%
  mutate(
    n_obs_by_site_yr_pm10 = replace_na(n_obs_by_site_yr_pm10, 0),
    n_obs_by_site_yr_pm25 = replace_na(n_obs_by_site_yr_pm25, 0),
  )

# Make a balanced panel
start_year <- 2012
end_year <- 2022
n_years <- (end_year - start_year) + 1

df_obs_balanced <- df_obs %>%
  distinct(site_id) %>%
  slice(rep(1:n(), each = n_years)) %>%
  dplyr::group_by(site_id) %>%
  dplyr::mutate(year = 2012:2022) %>%
  dplyr::ungroup()

df_obs_balanced <- left_join(df_obs_balanced, df_obs, by = c("site_id", "year"))
df_obs_balanced <- df_obs_balanced %>%
  mutate_at(c("n_obs_by_site_yr_pm10", "n_obs_by_site_yr_pm25"), ~ifelse(is.na(.), 0, .))

# Output site-year panel
write_csv(df_combined, paste0(path_int_epa, "/panel_site_year_num_obs_by_pollutant.csv"))

