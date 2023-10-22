
# dataframe-to-dataframe merge function like Stata
stata.merge <- function(x, y, by) {
  x$new1 <- 1L
  y$new2 <- 2L
  df <- full_join(x, y, by)
  df$merge <- as.integer(rowSums(df[, c("new1", "new2")], na.rm = TRUE))
  df$new1 <- NULL
  df$new2 <- NULL
  return(df)
}

check_df_unique_by <- function(df, ..., stop = T) {
  # inputs:
  # df: a dataframe
  #       identify the dataframe. Set to False if just want to print, not stop
  # ...: sequence of variables in the dataframe (list the vars without quotes separated by commas)
  # stop: True (default) if want to execute stop function when the variables do not uniquely
  #
  vars <- dplyr::quos(...)
  n <- df %>%
    dplyr::distinct(!!!vars) %>%
    nrow()
  
  varnames <- rlang::quo_text(vars)
  varnames <- gsub("~", "", regmatches(varnames, gregexpr("~\\w+", varnames))[[1]])
  varnames <- paste(varnames, collapse = ", ")
  if (n != nrow(df)) {
    message <- paste0("The data are NOT uniquely identified by: ", varnames)
    if (stop == T) {
      stop(message)
    } else {
      print(message)
    }
  } else {
    message <- paste0("The data are uniquely identified by: ", varnames)
    print(message)
  }
}

# Tidy strings
tidy_string <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
}

# Define a function to convert state fips to abbreviation
get_state_abbreviation_from_fips <- function(fips_code) {
  case_when(
    fips_code == '01' ~ 'AL',
    fips_code == '02' ~ 'AK',
    fips_code == '04' ~ 'AZ',
    fips_code == '05' ~ 'AR',
    fips_code == '06' ~ 'CA',
    fips_code == '08' ~ 'CO',
    fips_code == '09' ~ 'CT',
    fips_code == '10' ~ 'DE',
    fips_code == '11' ~ 'DC',
    fips_code == '12' ~ 'FL',
    fips_code == '13' ~ 'GA',
    fips_code == '15' ~ 'HI',
    fips_code == '16' ~ 'ID',
    fips_code == '17' ~ 'IL',
    fips_code == '18' ~ 'IN',
    fips_code == '19' ~ 'IA',
    fips_code == '20' ~ 'KS',
    fips_code == '21' ~ 'KY',
    fips_code == '22' ~ 'LA',
    fips_code == '23' ~ 'ME',
    fips_code == '24' ~ 'MD',
    fips_code == '25' ~ 'MA',
    fips_code == '26' ~ 'MI',
    fips_code == '27' ~ 'MN',
    fips_code == '28' ~ 'MS',
    fips_code == '29' ~ 'MO',
    fips_code == '30' ~ 'MT',
    fips_code == '31' ~ 'NE',
    fips_code == '32' ~ 'NV',
    fips_code == '33' ~ 'NH',
    fips_code == '34' ~ 'NJ',
    fips_code == '35' ~ 'NM',
    fips_code == '36' ~ 'NY',
    fips_code == '37' ~ 'NC',
    fips_code == '38' ~ 'ND',
    fips_code == '39' ~ 'OH',
    fips_code == '40' ~ 'OK',
    fips_code == '41' ~ 'OR',
    fips_code == '42' ~ 'PA',
    fips_code == '44' ~ 'RI',
    fips_code == '45' ~ 'SC',
    fips_code == '46' ~ 'SD',
    fips_code == '47' ~ 'TN',
    fips_code == '48' ~ 'TX',
    fips_code == '49' ~ 'UT',
    fips_code == '50' ~ 'VT',
    fips_code == '51' ~ 'VA',
    fips_code == '53' ~ 'WA',
    fips_code == '54' ~ 'WV',
    fips_code == '55' ~ 'WI',
    fips_code == '56' ~ 'WY',
    TRUE ~ NA_character_  # Default case, return NA if no match
  )
}

# Define a function to convert state name to abbreviation
get_state_abbreviation_from_name <- function(state_name) {
  state_name <- toupper(state_name)  # Convert input to uppercase
  state_abbrev <- case_when(
    state_name == "ALABAMA" ~ "AL",
    state_name == "ALASKA" ~ "AK",
    state_name == "ARIZONA" ~ "AZ",
    state_name == "ARKANSAS" ~ "AR",
    state_name == "CALIFORNIA" ~ "CA",
    state_name == "COLORADO" ~ "CO",
    state_name == "CONNECTICUT" ~ "CT",
    state_name == "DISTRICT OF COLUMBIA" ~ "DC",
    state_name == "DELAWARE" ~ "DE",
    state_name == "FLORIDA" ~ "FL",
    state_name == "GEORGIA" ~ "GA",
    state_name == "HAWAII" ~ "HI",
    state_name == "IDAHO" ~ "ID",
    state_name == "ILLINOIS" ~ "IL",
    state_name == "INDIANA" ~ "IN",
    state_name == "IOWA" ~ "IA",
    state_name == "KANSAS" ~ "KS",
    state_name == "KENTUCKY" ~ "KY",
    state_name == "LOUISIANA" ~ "LA",
    state_name == "MAINE" ~ "ME",
    state_name == "MARYLAND" ~ "MD",
    state_name == "MASSACHUSETTS" ~ "MA",
    state_name == "MICHIGAN" ~ "MI",
    state_name == "MINNESOTA" ~ "MN",
    state_name == "MISSISSIPPI" ~ "MS",
    state_name == "MISSOURI" ~ "MO",
    state_name == "MONTANA" ~ "MT",
    state_name == "NEBRASKA" ~ "NE",
    state_name == "NEVADA" ~ "NV",
    state_name == "NEW HAMPSHIRE" ~ "NH",
    state_name == "NEW JERSEY" ~ "NJ",
    state_name == "NEW MEXICO" ~ "NM",
    state_name == "NEW YORK" ~ "NY",
    state_name == "NORTH CAROLINA" ~ "NC",
    state_name == "NORTH DAKOTA" ~ "ND",
    state_name == "OHIO" ~ "OH",
    state_name == "OKLAHOMA" ~ "OK",
    state_name == "OREGON" ~ "OR",
    state_name == "PENNSYLVANIA" ~ "PA",
    state_name == "RHODE ISLAND" ~ "RI",
    state_name == "SOUTH CAROLINA" ~ "SC",
    state_name == "SOUTH DAKOTA" ~ "SD",
    state_name == "TENNESSEE" ~ "TN",
    state_name == "TEXAS" ~ "TX",
    state_name == "UTAH" ~ "UT",
    state_name == "VERMONT" ~ "VT",
    state_name == "VIRGINIA" ~ "VA",
    state_name == "WASHINGTON" ~ "WA",
    state_name == "WEST VIRGINIA" ~ "WV",
    state_name == "WISCONSIN" ~ "WI",
    state_name == "WYOMING" ~ "WY",
    state_name == "PUERTO RICO" ~ "PR",
    state_name == "VIRGIN ISLANDS" ~ "VI",
    state_name == "GUAM" ~ "GU",
    state_name == "TRUST TERRITORIES" ~ "TT",
    state_name == "AMERICAN SAMOA" ~ "TT",
    state_name == "NORTHERN MARIANA ISLANDS" ~ "MP",
    TRUE ~ NA_character_
  )
  return(state_abbrev)
}

# Define a function to get the neighboring states for each state (including the state itself)
get_neighboring_states <- function(state_abbreviation) {
  
  neighboring_states <- list(
    AL = c("FL", "GA", "TN", "MS"),
    AZ = c("CA", "NV", "UT", "NM", "CO"),
    AR = c("MO", "TN", "MS", "LA", "TX", "OK"),
    CA = c("OR", "NV", "AZ"),
    CO = c("WY", "NE", "KS", "OK", "NM", "UT", "AZ"),
    CT = c("NY", "RI", "MA"),
    DC = c("MD", "VA"),
    DE = c("MD", "NJ", "PA"),
    FL = c("GA", "AL"),
    GA = c("SC", "NC", "TN", "AL", "FL"),
    ID = c("MT", "WY", "UT", "NV", "OR", "WA"),
    IL = c("WI", "IA", "MO", "KY", "IN", "MI"),
    IN = c("MI", "IL", "KY", "OH"),
    IA = c("MN", "WI", "IL", "MO", "NE", "SD"),
    KS = c("NE", "MO", "OK", "CO"),
    KY = c("IN", "IL", "MO", "TN", "OH", "WV", "VA"),
    LA = c("AR", "MS", "TX"),
    ME = c("NH"),
    MD = c("DE", "PA", "VA", "WV", "NJ"),
    MA = c("NY", "CT", "RI", "NH", "VT"),
    MI = c("WI", "IL", "IN", "OH"),
    MN = c("ND", "SD", "IA", "WI"),
    MS = c("AR", "LA", "AL", "TN"),
    MO = c("IA", "IL", "KY", "TN", "AR", "OK", "KS", "NE"),
    MT = c("ND", "SD", "WY", "ID"),
    NE = c("SD", "WY", "CO", "KS", "MO", "IA"),
    NV = c("OR", "ID", "UT", "AZ", "CA"),
    NH = c("VT", "ME", "MA"),
    NJ = c("DE", "PA", "NY", "MD"),
    NM = c("AZ", "UT", "CO", "OK", "TX"),
    NY = c("VT", "MA", "CT", "NJ", "PA"),
    NC = c("VA", "TN", "GA", "SC"),
    ND = c("MT", "SD", "MN"),
    OH = c("MI", "IN", "KY", "WV", "PA"),
    OK = c("KS", "MO", "AR", "TX", "NM", "CO"),
    OR = c("WA", "ID", "NV", "CA"),
    PA = c("NY", "NJ", "DE", "MD", "WV", "OH"),
    RI = c("MA", "CT"),
    SC = c("NC", "GA"),
    SD = c("ND", "MT", "WY", "NE", "IA", "MN"),
    TN = c("KY", "VA", "NC", "GA", "AL", "MS", "AR", "MO"),
    TX = c("OK", "AR", "LA", "NM"),
    UT = c("ID", "WY", "CO", "NM", "AZ", "NV"),
    VT = c("NY", "NH", "MA"),
    VA = c("MD", "NC", "TN", "KY", "WV"),
    WA = c("ID", "OR"),
    WV = c("OH", "PA", "MD", "VA", "KY"),
    WI = c("MI", "IL", "IA", "MN"),
    WY = c("MT", "SD", "NE", "CO", "UT", "ID")
  )
  
  # Retrieve neighboring states based on the input state abbreviation
  if (state_abbreviation %in% names(neighboring_states)) {
    return(c(state_abbreviation, neighboring_states[[state_abbreviation]]))
  } else {
    return(NULL)  # Return NULL for states not found in the list
  }
}