setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Functions --------------------------------------------------------------------

# Check that the NASS State and county fips codes are only missing for "other" counties
check_na_expected_nass <- function(df) {
  if (any(is.na(df$`State ANSI`)) == T) {
    stop("Some state codes are missing")
  }

  other_county <- (str_sub(df$County, 1, 5) == "OTHER")
  if (all(is.na(df$`County ANSI`) == other_county) == F) {
    stop("Check the county codes")
  }
}

# Check the NASS state and county codes and names uniquely identify observations
check_nass_names_match_codes <- function(df) {
  n_state_ansi <- distinct(df, state_ansi) %>% nrow()
  n_state_name <- distinct(df, state) %>% nrow()
  if (n_state_ansi != n_state_name) {
    stop("Check state names vs codes")
  }

  n_county_ansi <- distinct(df, state_ansi, county_ansi) %>% nrow()
  n_county_name <- distinct(df, state, county) %>% nrow()
  if (n_county_ansi != n_county_name) {
    stop("Check county names vs codes")
  }
}

# Process yields data
process_nass_yields_data <- function(directory, filename) {

  df <- read_csv(paste0(directory, "/", filename))

  # Check the NA values are where expected
  check_na_expected_nass(df)

  # Clean the data
  df <- df %>%
    filter(str_sub(County, 1, 5) != "OTHER") %>%
    dplyr::select(
      Year, State, `State ANSI`, County, `County ANSI`, Commodity,
      `Data Item`, Value
    ) %>%
    dplyr::rename(
      state_ansi = `State ANSI`, county_ansi = `County ANSI`,
      data_item = `Data Item`
    ) %>%
    mutate_at(
      c("State", "County", "Commodity", "data_item", "state_ansi", "county_ansi", "Value"),
      ~ tidy_string(.)
    )
  names(df) <- tolower(names(df))
  check_df_unique_by(df, state_ansi, county_ansi, year)
  check_nass_names_match_codes(df)

  return(df)
}

# Process NASS yield data ------------------------------------------------------

# Load the county dataframe
county2020_df <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp")) %>%
  as_tibble() %>%
  dplyr::select(-geometry)

# Initialize yield dataframe
nass_yield_df_all <- data.frame()

# Combine yield and county census data
filenames <- list.files(paste0(path_data_raw, "/crop_yields/nass"))
for (filename in filenames) { # Loop through yield files

  print(filename)

  # Process yield data
  yield_df <- process_nass_yields_data(
    directory = paste0(path_data_raw, "/crop_yields/nass/"),
    filename = filename
  )

  # Merge county and yield data
  df <- stata.merge(
    county2020_df,
    yield_df,
    by = c("stfp" = "state_ansi", "cntyfp" = "county_ansi")
  )

  # Check that the merge was successful
  if (any(df$merge == 2)) {
    stop("There are counties in the yields data that aren't in the county dataframe")
  } else {
    df <- df %>%
      filter(merge == 3) %>%
      dplyr::select(-merge)
  }

  # Append to the yield data frame
  nass_yield_df_all <- bind_rows(nass_yield_df_all, df)

  print("Done! Onto the next file")
}

# Convert the yield values to numeric
nass_yield_df_all <- nass_yield_df_all %>%
  mutate(value = ifelse(value == "(d)", "",
    ifelse(str_detect(value, ","), str_replace_all(value, ",", ""),
      value
    )
  )) %>%
  mutate(value = as.numeric(value))

# # Inspect county name mis-matches to confirm merge is working since county fips codes
# # can change over time
# nass_yield_df_all %>%
#   filter(cntyname != county) %>%
#   distinct(cntyname, county) %>%
#   View()

# Reformat the yield variables
nass_yield_df_all <- nass_yield_df_all %>%
  mutate(
    commodity = case_when(
      data_item == "cotton, upland - yield, measured in lb / acre" ~ "cotton",
      data_item == "corn, grain - yield, measured in bu / acre" ~ "corn",
      data_item == "peanuts - yield, measured in lb / acre" ~ "peanuts",
      data_item == "soybeans - yield, measured in bu / acre" ~ "soybeans",
      data_item == "wheat, winter - yield, measured in bu / acre" ~ "winter_wheat",
      data_item == "rice - yield, measured in lb / acre" ~ "rice",
      data_item == "barley - yield, measured in bu / acre" ~ "barley",
      data_item == "lentils - yield, measured in lb / acre" ~ "lentils",
      data_item == "pecans, utilized, in shell - yield, measured in lb / acre" ~ "pecans",
      data_item == "wheat, spring, durum - yield, measured in bu / acre" ~ "sprint_wheat_durum",
      data_item == "wheat, spring, (excl durum) - yield, measured in bu / acre" ~ "sprint_wheat_excl_durum",
      data_item == "chickpeas - yield, measured in lb / acre" ~ "chickpeas",
      TRUE ~ NA
      ),
    yield_unit = case_when(
      data_item == "cotton, upland - yield, measured in lb / acre" ~ "lb/acre",
      data_item == "corn, grain - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "peanuts - yield, measured in lb / acre" ~ "lb/acre",
      data_item == "soybeans - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "wheat, winter - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "rice - yield, measured in lb / acre" ~ "lb/acre",
      data_item == "barley - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "lentils - yield, measured in lb / acre" ~ "lb/acre",
      data_item == "pecans, utilized, in shell - yield, measured in lb / acre" ~ "lb/acre",
      data_item == "wheat, spring, durum - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "wheat, spring, (excl durum) - yield, measured in bu / acre" ~ "bu/acre",
      data_item == "chickpeas - yield, measured in lb / acre" ~ "lb/acre",
      TRUE ~ NA
    ),
    source = "NASS"
    ) %>%
  dplyr::rename(yield = value) %>%
  dplyr::select(-c(data_item, county, state))


# Process CAC yield data -------------------------------------------------------

# Process yield data
cac_yield_df_all <- data.frame()

crops <- list.files(paste0(path_data_raw, "/crop_yields/cac"))
for (crop in crops) {
  files <- list.files(paste0(path_data_raw, "/crop_yields/cac/", crop), full.names = T)
  for (file in files) {
    df <- read_csv(file) %>%
      mutate_at(c("County", "Commodity_Name", "Unit"), ~ tidy_string(.)) %>%
      mutate(
        GEOID = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
        stfp = str_sub(GEOID, 1, 2),
        cntyfp = str_sub(GEOID, 3, 5)
        ) %>%
      dplyr::select(Year, stfp, cntyfp, County, Commodity_Name, Yield, Unit) %>%
      dplyr::rename(yield_unit = Unit, cntyname = County, commodity = Commodity_Name)
    names(df) <- tolower(names(df))
    cac_yield_df_all <- bind_rows(cac_yield_df_all, df)
  }
}

# Use the county names from the census shapefile
cac_yield_df_all <- stata.merge(
  cac_yield_df_all,
  county2020_df,
  by = c("stfp", "cntyfp")
)
if (any(cac_yield_df_all$merge == 1)) {
  stop("Check merge with county shapefile")
} else {
  cac_yield_df_all <- cac_yield_df_all %>% filter(merge == 3) %>% dplyr::select(-merge)
}
cac_yield_df_all <- cac_yield_df_all %>% 
  dplyr::select(-c(cntyname.x)) %>%
  dplyr::rename(cntyname = cntyname.y) %>%
  mutate(source = "CAC")

# # Inspect county name mis-matches
# cac_yield_df_all %>%
#   filter(cntyname.x != cntyname.y) %>%
#   distinct(cntyname.x, cntyname.y) %>%
#   View()

# Combine yields data and output -----------------------------------------------

final_yield_df <- bind_rows(nass_yield_df_all, cac_yield_df_all)

# Output
write_csv(nass_yield_df_all, paste0(path_data_int, "/crops/panel_crop_yields.csv"))




