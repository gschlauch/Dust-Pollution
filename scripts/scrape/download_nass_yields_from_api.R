setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

library(rnassqs)
NASS_API_KEY <- "5DE689DC-84DC-3F09-8FF8-D2A49FC91620"
nassqs_auth(key = NASS_API_KEY)

# Fetch and clean data from the API --------------------------------------------
fetch_and_clean_data <- function(commodity_desc, state_alpha, year__GE, statisticcat_desc,
                                 prodn_practice_desc, unit_desc) {
  params <- list(
    commodity_desc = commodity_desc,
    year__GE = year__GE,
    state_alpha = state_alpha,
    statisticcat_desc = statisticcat_desc,
    prodn_practice_desc = prodn_practice_desc,
    unit_desc = unit_desc,
    agg_level_desc = "COUNTY",
    source_desc = "SURVEY"
  )

  df <- nassqs(params)
  names(df) <- tolower(names(df))
  df <- df %>%
    dplyr::select(
      source_desc, commodity_desc, class_desc, prodn_practice_desc,
      util_practice_desc, statisticcat_desc, unit_desc, domain_desc,
      agg_level_desc, state_fips_code, state_name, county_code,
      county_name, year, freq_desc, value
    ) %>%
    filter(!(county_name %in% c("OTHER COUNTIES", "OTHER (COMBINED) COUNTIES")))
}

# Download data ----------------------------------------------------------------

# Corn
corn_states <- nassqs_param_values(
  param = "state_alpha",
  commodity_desc = "CORN",
  year__GE = 2005,
  statisticcat_desc = "YIELD",
  prodn_practice_desc = "ALL PRODUCTION PRACTICES",
  unit_desc = "BU / ACRE"
)
corn_df <- fetch_and_clean_data(
  commodity_desc = "CORN",
  state_alpha = corn_states,
  year__GE = 2005,
  statisticcat_desc = "YIELD",
  prodn_practice_desc = "ALL PRODUCTION PRACTICES",
  unit_desc = "BU / ACRE"
)

# Wheat
wheat_states <- nassqs_param_values(
  param = "state_alpha",
  commodity_desc = "WHEAT",
  year__GE = 2005,
  statisticcat_desc = "YIELD",
  prodn_practice_desc = "ALL PRODUCTION PRACTICES",
  unit_desc = "BU / ACRE"
)
wheat_df <- fetch_and_clean_data(
  commodity_desc = "WHEAT",
  state_alpha = corn_states,
  year__GE = 2005,
  statisticcat_desc = "YIELD",
  prodn_practice_desc = "ALL PRODUCTION PRACTICES",
  unit_desc = "BU / ACRE"
)

# Soybeans

# Hay

# Cotton

# Almonds

# Grapes

# Strawberries

# Peanuts

# Tomatoes
  
