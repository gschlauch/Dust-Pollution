setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Functions --------------------------------------------------------------------

# Check that the State and county fips codes are only missing for "other" counties
check_na_expected <- function(df) {
  if (any(is.na(df$`State ANSI`)) == T) {
    stop("Some state codes are missing")
  }

  other_county <- (str_sub(df$County, 1, 5) == "OTHER")
  if (all(is.na(df$`County ANSI`) == other_county) == F) {
    stop("Check the county codes")
  }
}

# Check the state and county codes and names uniquely identify observations
check_names_match_codes <- function(df) {
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

# Tidy strings
tidy_string <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
}

# # Create county mapping to clean names to match the census shapefile
# county_mapping <- data.frame(
#   original = c(
#     "de kalb",
#     "saint francis",
#     "du page",
#     "la salle",
#     "st clair",
#     "la porte",
#     "o brien",
#     "saint landry",
#     "prince georges",
#     "queen annes",
#     "st marys",
#     "st joseph",
#     "de soto",
#     "st charles",
#     "ste genevieve",
#     "st francois",
#     "st louis",
#     "dona ana",
#     "st lawrence",
#     "la moure",
#     "leflore",
#     "de witt",
#     "chesapeake city",
#     "suffolk city",
#     "virginia beach city",
#     "st croix"
#   ),
#   replacement = c(
#     "dekalb",
#     "st. francis",
#     "dupage",
#     "lasalle",
#     "st. clair",
#     "laporte",
#     "o'brien",
#     "st. landry",
#     "prince george's",
#     "queen anne's",
#     "st. mary's",
#     "st. joseph",
#     "desoto",
#     "st. charles",
#     "ste. genevieve",
#     "st. francois",
#     "st. louis",
#     "doña ana",
#     "st. lawrence",
#     "lamoure",
#     "le flore",
#     "dewitt",
#     "chesapeake",
#     "suffolk",
#     "virginia beach",
#     "st. croix"
#   )
# )

# Process yields data
process_yields_data <- function(directory, filename) {
  # Load dataframe
  df <- read_csv(paste0(directory, "/", filename))

  # Check the NA values are where expected
  check_na_expected(df)

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
  check_names_match_codes(df)

  return(df)
}

# Process yield data -----------------------------------------------------------

# Load the county dataframe
county2020_df <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp")) %>%
  as_tibble() %>%
  dplyr::select(-geometry)

# Initialize yield dataframe
yield_df_all <- data.frame()

# Combine yield and county census data
filenames <- list.files(paste0(path_data_raw, "/usda/nass/yields"))
for (filename in filenames) { # Loop through yield files

  print(filename)

  # Process yield data
  yield_df <- process_yields_data(
    directory = paste0(path_data_raw, "/usda/nass/yields/"),
    filename = filename
  )

  # Merge county and yield data
  df <- stata.merge(
    county2020_df,
    yield_df,
    by = c("statefp" = "state_ansi", "countyfp" = "county_ansi")
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
  yield_df_all <- bind_rows(yield_df_all, yield_df)

  print("Done! Onto the next file")
}

# Convert the yield values to numeric
yield_df_all <- yield_df_all %>%
  mutate(value = ifelse(value == "(d)", "",
    ifelse(str_detect(value, ","), str_replace_all(value, ",", ""),
      value
    )
  )) %>%
  mutate(value = as.numeric(value))

# Output
write_csv(yield_df_all, paste0(path_data_int, "/usda/panel_crop_yields_2005to2022.csv"))
