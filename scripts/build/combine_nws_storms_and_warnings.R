setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# ------------------------------------------------------------------------------
# Process dust storm details data

# Define a function to process the storm details (ie, actual storm events) data
clean_storm_details <- function(filename) {
  df <- read_csv(filename)
  names(df) <- tolower(names(df))
  df <- df %>%
    dplyr::select(episode_id, event_id, event_type, state, cz_type, cz_fips, cz_name, begin_date_time, 
                  end_date_time, cz_timezone, episode_narrative, event_narrative) %>%
    mutate(
      cz_state = get_state_abbreviation_from_name(state),
      cz_fips = str_pad(as.character(cz_fips), width = 3, side = "left", pad = "0"),
      storm_start_date = dmy(str_sub(begin_date_time, 1, 9)),
      storm_start_datetime = dmy_hms(begin_date_time),
      storm_end_date = dmy(str_sub(end_date_time, 1, 9)),
      storm_end_datetime = dmy_hms(end_date_time)
    ) %>%
    mutate_at(c("cz_name", "event_narrative", "episode_narrative"), ~ tidy_string(.)) %>%
    filter(!(cz_state %in% c("AK", "VI", "PR", "GU", "HI", "MP", "TT", "AS"))) %>%
    dplyr::select(-c(state, begin_date_time, end_date_time)) %>%
    filter(
      event_type %in% c("Dust Storm", "Blowing Dust", "High Winds Dust Storm", "Dust Storm/High Winds", "Duststorm") |
      str_detect(episode_narrative, "dust storm|duststorm") | 
      str_detect(event_narrative, "dust storm|duststorm")
      )
}

# Process the storm details data using the above function
dirpath <- paste0(path_data_raw, "/dust_storms/storm_details")
files <- list.files(dirpath, pattern = ".csv$")
storm_details_df <- data.frame()
for (file in files) {
  filename <- paste0(dirpath, "/", file)
  df <- clean_storm_details(filename)
  storm_details_df <- bind_rows(storm_details_df, df)
}
check_df_unique_by(storm_details_df, event_id)

# Select events classified as dust storms as the event type. Later, consider
# parsing through event narratives to get dust storms associated with other events
# (e.g., thunderstorm) that were not separately coded as dust storms
storm_details_df <- storm_details_df %>%
  filter(event_type %in% c("Dust Storm", "Blowing Dust", "High Winds Dust Storm", "Dust Storm/High Winds", "Duststorm"))

# Get states (and their neighboring states) with at least one storm
states_and_nbrs_with_storms <- ""
df <- dplyr::distinct(storm_details_df, cz_state)
for (state in df$cz_state) {
  state_and_nbrs <- get_neighboring_states(state)
  states_and_nbrs_with_storms <- c(states_and_nbrs_with_storms, state_and_nbrs)
}
states_and_nbrs_with_storms <- unique(states_and_nbrs_with_storms[-1])

# Remove the three storms that have a county, not a zone. I deal with these later
storm_details_df_counties_not_zones <- storm_details_df %>%
  filter(cz_type != "Z")
storm_details_df <- storm_details_df %>%
  filter(cz_type == "Z") %>%
  dplyr::rename(zone_fips = cz_fips, zone_name = cz_name, zone_state = cz_state) %>%
  dplyr::select(-cz_type)

# Clean the zone names to match those in the zone shapefiles
storm_details_df <- storm_details_df %>%
  dplyr::mutate(
    zone_name = case_when(
      zone_name == "marble canyon and glen canyon" ~ "marble and glen canyons", 
      zone_name == "northeast plateaus and mesas from highway 264 north" ~ "northeast plateaus and mesas hwy 264 northward", 
      zone_name == "northeast plateaus and mesas south of highway 264" ~ "northeast plateaus and mesas south of hwy 264", 
      zone_name == "tohono o odham nation" ~ "tohono oodham nation", 
      zone_name == "tucson metro area" ~ "tucson metro area including tucson/green valley/marana/vail", 
      zone_name == "c maricopa" ~ "greater phoenix area", 
      zone_name == "sw yuma" ~ "yuma/martinez lake and vicinity", 
      zone_name == "nw pinal" ~ "northwest and north central pinal county", 
      zone_name == "south central pinal county" ~ "south central pinal county including eloy/picacho peak state park", 
      zone_name == "southeast pinal county including kearny/mammoth/oracle" ~ "southeast pinal county", 
      zone_name == "eastern cochise county below 5000 feet" ~ "eastern cochise county below 5000 feet including douglas/willcox", 
      zone_name == "lower colorado river valley az" ~ "lower colorado river valley ca", 
      zone_name == "riverside county eastern deserts" ~ "riverside county/eastern deserts", 
      zone_name == "imperial county except the lower colorado river valley" ~ "imperial county",
      zone_name == "w central s.j. valley" ~ "west-central san joaquin valley", 
      zone_name == "e central s.j. valley" ~ "east-central san joaquin valley", 
      zone_name == "sw s.j. valley" ~ "southwestern san joaquin valley", 
      zone_name == "se s.j. valley" ~ "southeastern san joaquin valley", 
      zone_name == "indian wells vly" ~ "indian wells valley", 
      zone_name == "se kern cty desert" ~ "southeastern kern county desert", 
      zone_name == "west side mountains south of highway 198" ~ "west side mountains south of 198", 
      zone_name == "eastern mojave desert" ~ "eastern mojave desert, including the mojave national preserve", 
      zone_name == "n & ne elbert county below 6000 feet / n lincoln county" ~ "north and northeast elbert county below 6000 feet/north lincoln county", 
      zone_name == "pueblo vicinity / pueblo county below 6300 ft" ~ "pueblo vicinity/pueblo county below 6300 feet", 
      zone_name == "bent county" ~ "las animas vicinity/bent county", 
      zone_name == "lamar vicinity / prowers county" ~ "lamar vicinity/prowers county", 
      zone_name == "southwest desert mimbres basin" ~ "southwest desert/mimbres basin", 
      zone_name == "mineral/southern lyon" ~ "mineral and southern lyon counties", 
      zone_name == "greater reno/carson city/minden area" ~ "greater reno-carson city-minden area", 
      zone_name == "western nevada basin and range" ~ "western nevada basin and range including pyramid lake", 
      zone_name == "esmeralda/central nye" ~ "esmeralda and central nye county", 
      zone_name == "lincoln county except the sheep range" ~ "lincoln county", 
      zone_name == "western clark/southern nye" ~ "western clark and southern nye county", 
      zone_name == "lake mead/lake mohave national recreation area" ~ "lake mead national recreation area", 
      zone_name == "southern clark" ~ "southern clark county", 
      zone_name == "humboldt" ~ "humboldt county", 
      zone_name == "sw & sc elko" ~ "southwest and south central elko county", 
      zone_name == "n lander & n eureka" ~ "northern lander county and northern eureka county", 
      zone_name == "southwestern elko" ~ "southwest elko county", 
      zone_name == "central & eastern lake county" ~ "central and eastern lake county", 
      zone_name == "grand ronde valley" ~ "grande ronde valley", 
      zone_name == "van horn & hwy 54 corridor" ~ "van horn and highway 54 corridor", 
      zone_name == "guadalupe mountains of culberson county" ~ "guadalupe mountains", 
      zone_name == "southern wasatch front/lehi/provo/nephi" ~ "southern wasatch front", 
      zone_name == "e kittitas" ~ "kittitas valley", 
      zone_name == "e yakima" ~ "yakima valley", 
      zone_name == "southeast pinal county including kearny/mammoth/oracle" ~ "southeast pinal county", 
      zone_name == "c pima" ~ "tohono oodham nation", 
      zone_name == "e pima" ~ "tohono oodham nation", 
      zone_name == "albuquerque metro area" ~ "middle rio grande valley/albuquerque metro area", 
      zone_name == "s washoe t x se & x sw/storey/e carson city/c&e douglas/nw lyon" ~ "greater reno-carson city-minden area", 
      zone_name == "pershing/churchill/x se washoe/nc&ne lyon" ~ "western nevada basin and range including pyramid lake", 
      zone_name == "emery t nw x ne/c e wayne/p sc carbon" ~ "san rafael swell", 
      zone_name == "benton/franklin/walla walla t se/e klickitat" ~ "lower columbia basin", 
      zone_name == "blue mountain foothills" ~ "foothills of the blue mountains", 
      zone_name == "n columbia/se walla walla t x se" ~ "foothills of the blue mountains",
      zone_name == "central deserts" & zone_fips == "028" ~ "northwest and north central pinal county",
      TRUE ~ zone_name
    )
  )

# Some zone fips change over time for a given zone. For consistency and ease in matching,
# I set the problematic ones to be the same over time. This won't affect the match if
# another zone happened to have this fips because I match on state-fips-name.
storm_details_df <- storm_details_df %>%
  mutate(
    zone_fips = case_when(
      zone_state == "AZ" & zone_name == "tohono oodham nation" ~ "502",
      zone_state == "NM" & zone_name == "southwest desert/mimbres basin" ~ "407",
      zone_state == "OR" & zone_name == "foothills of the blue mountains" ~ "501",
      TRUE ~ zone_fips
    )
  )

# ------------------------------------------------------------------------------
# Prep the zone-to-county crosswalk

# Filter data to only have states with storms (and their neighbors)
filename <- paste0(path_data_int, "/Crosswalks/xwalk_NWS_forecast_zones_to_counties.csv")
xwalk_zones_to_counties <- read_csv(filename) %>%
  filter(zone_state %in% states_and_nbrs_with_storms)

# For each filename, add the file creation date. This will be used later to know
# which zones to apply to which time periods. Creation dates are from this site:
# https://www.weather.gov/source/gis/Shapefiles/WSOM/zone_ch_log.txt
# Note: some files provide multiple dates. Per correspondence with NOAA, I use the
# effective date. For files that provide two dates, the effective date matches the filename.
# For example: z_02jn20.zip (z_02jn20.shp) has dates 25 February 2020 and Effective 02 June 2020.
# the filename clearly corresponds with the effective date. Other files only provide 1 date,
# and in those cases the filename does not match the date. I suspect they are giving the date created
# on the change history site and not the Effective date, which corresponds to the actual filename. Thus,
# unless otherwise noted, I use the date corresponding to the actual filename (e.g., z_02jn20.zip is 02 June 2020).
# In almost every case when only 1 date is provided in the change history, it is earlier than the date indicated by the 
# filename, which lends credence to the filename corresponding to the effective date (which comes after file creation date)
xwalk_zones_to_counties <- xwalk_zones_to_counties %>%
  mutate(
    zone_file_date = case_when(
      zone_filename == "z_01ap08.shp" ~ dmy("01 April 2008"), # 27 November 2007, 2 May 2008: two dates for same file, take earlier one
      zone_filename == "z_01ap14a.shp" ~ dmy("01 April 2014"), # 25 March 2014, Effective 1 April 2014 per SCN 14-08
      zone_filename == "z_01au07.shp" ~ dmy("01 August 2007"), # 2 April 2007
      zone_filename == "z_01de10.shp" ~ dmy("01 December 2010"), # 6 April 2010
      zone_filename == "z_02ap19.shp" ~ dmy("02 April 2019"), # 18 December 2018, Effective 02 April 2019
      zone_filename == "z_02jn20.shp" ~ dmy("02 June 2020"), # 25 February 2020, Effective 02 June 2020
      zone_filename == "z_02oc18.shp" ~ dmy("02 October 2018"), # 23 July 2018, Effective 02 October 2018
      zone_filename == "z_03ap12.shp" ~ dmy("03 April 2012"), # 30 January 2012
      zone_filename == "z_03ap18.shp" ~ dmy("03 April 2018"), # 21 November 2017, Effective 03 April 2018. 
      zone_filename == "z_03de13.shp" ~ dmy("03 December 2013"), # 10 September 2013, Effective 3 December 2013 (weird)
      zone_filename == "z_03de14.shp" ~ dmy("03 December 2014"), # 17 September 2014, Effective 3 December 2014
      zone_filename == "z_03de14c.shp" ~ dmy("08 December 2015"), # 8 December 2015 SPECIAL EXCEPTION SINCE THIS DATE IS AFTER EFFECTIVE DATE
      zone_filename == "z_03my11.shp" ~ dmy("03 May 2011"), # 15 December 2010
      zone_filename == "z_03oc08.shp" ~ dmy("03 October 2008"), # 2 October 2008
      #zone_filename == "z_04ap17.shp" ~ dmy("04 April 2017"), # 19 January 2017, Effective 04 April 2017. THIS FILE HAS MERGING ISSUES, SO I REMOVE IT
      zone_filename == "z_04au11.shp" ~ dmy("04 August 2011"), # 28 March 2011
      zone_filename == "z_04de12.shp" ~ dmy("04 December 2012"), # 7 August 2012
      zone_filename == "z_04fe15.shp" ~ dmy("04 February 2015"), # 21 November 2014, Effective 4 February 2015
      zone_filename == "z_05ap16.shp" ~ dmy("05 April 2016"), # Effective 5 April 2016 
      zone_filename == "z_05de17.shp" ~ dmy("05 December 2017"), # 19 September 2017, Effective 05 December 2017
      zone_filename == "z_05fe14.shp" ~ dmy("05 February 2014"), # 22 November 2013, Effective 5 February 2014
      zone_filename == "z_07ap15.shp" ~ dmy("07 April 2015"), # 23 December 2015, Effective 7 April 2015
      zone_filename == "z_07ap15a.shp" ~ dmy("08 May 2015"), # 8 May 2015
      zone_filename == "z_07jn12.shp" ~ dmy("07 June 2012"), # 7 May 2012
      zone_filename == "z_07my09.shp" ~ dmy("07 May 2009"), # 10 Febrary 2009
      zone_filename == "z_07oc14a.shp" ~ dmy("07 October 2014"), # 25 August 2014, Effective 7 October 2014
      zone_filename == "z_07se22.shp" ~ dmy("07 September 2022"), # 11 JUL 2022, Effective 07 September 2022
      zone_filename == "z_08se21.shp" ~ dmy("08 September 2021"), # Effective 08 September 21
      zone_filename == "z_09se08.shp" ~ dmy("09 September 2008"), # 9 September 2008
      zone_filename == "z_10jl18.shp" ~ dmy("10 July 2018"), # 17 April 2018, Effective 10 July 2018
      zone_filename == "z_10nv15.shp" ~ dmy("10 November 2015"), # 8 October 2015, Effective 10 November 2015
      zone_filename == "z_10nv20.shp" ~ dmy("10 November 2020"), # Effective 10 November 2020
      zone_filename == "z_10oc19.shp" ~ dmy("10 October 2019"), # 18 September 2019, Effective 10 October 2019
      zone_filename == "z_10se19.shp" ~ dmy("10 September 2019"), # 26 June 2019, Effective 10 September 2019
      zone_filename == "z_11oc12.shp" ~ dmy("11 October 2012"), # 10 September 2012
      zone_filename == "z_12jn14.shp" ~ dmy("12 June 2014"), # 25 March 2014, Effective 12 June 2014 per SCN 14-19
      zone_filename == "z_12jn14f.shp" ~ dmy("25 August 2014"), # 25 August 2014, Effective 12 June 2014
      zone_filename == "z_13oc11.shp" ~ dmy("13 October 2011"), # 9 September 2011
      zone_filename == "z_13se22.shp" ~ dmy("13 September 2022"), # Effective 13 September 2022
      zone_filename == "z_15au13.shp" ~ dmy("15 August 2013"), # 15 August 2013
      zone_filename == "z_15de11.shp" ~ dmy("15 December 2011"), # 18 August 2011
      zone_filename == "z_15jl09.shp" ~ dmy("15 July 2009"), # 1 April 2009
      zone_filename == "z_16mr06.shp" ~ dmy("16 March 2006"), # 16 March 2006
      zone_filename == "z_22jl09.shp" ~ dmy("22 July 2009"), # 27 March 2009
      zone_filename == "z_22mr22.shp" ~ dmy("22 March 2022"), # Effective 22 Mar 2022
      zone_filename == "z_23fe12.shp" ~ dmy("23 February 2011"), # 24 October 2011
      zone_filename == "z_30mr21.shp" ~ dmy("30 March 2021"), # Effective 30 March 21
      zone_filename == "z_31my07.shp" ~ dmy("31 May 2007"), # 1 February 2007
      zone_filename == "z_31my11.shp" ~ dmy("31 May 2011"), # 2 February 2011
      zone_filename == "z_01oc15.shp" ~ dmy("01 October 2015"), # Not in change history, just take file date
      zone_filename == "z_02nv15.shp" ~ dmy("02 November 2015"), # Not in change history, just take file date
      zone_filename == "z_03mr20.shp" ~ dmy("03 March 2020"), # Not in change history, just take file date
      zone_filename == "z_03nv15.shp" ~ dmy("03 November 2015"), # Not in change history, just take file date
      zone_filename == "z_11au16.shp" ~ dmy("11 August 2016"), # Not in change history, just take file date
      zone_filename == "z_11mr07.shp" ~ dmy("11 March 2007"), # Not in change history, just take file date
      zone_filename == "z_12ap12.shp" ~ dmy("12 April 2012"), # Not in change history, just take file date
      zone_filename == "z_25sep07.shp" ~ dmy("25 September 2007"), # Not in change history, just take file date
      zone_filename == "z_5sep07.shp" ~ dmy("05 September 2007"), # Not in change history, just take file date
      TRUE ~ NA
      )
    ) %>%
  filter(!is.na(zone_file_date))

n_distinct_file <- xwalk_zones_to_counties %>% dplyr::distinct(zone_file_date) %>% nrow()
n_distinct_file_and_date <- xwalk_zones_to_counties %>% dplyr::distinct(zone_filename) %>% nrow()
if (n_distinct_file != n_distinct_file_and_date) {
  stop("2 or more files share a file date")
}

# # Have more updated version that went into effect and these look like they
# never went into effect
# "z_07oc14.shp"
# "z_01ap14.shp

# Update zone names to be more consistent over time
xwalk_zones_to_counties <- xwalk_zones_to_counties %>%
  dplyr::mutate(
    zone_name = case_when(
      zone_name == "pueblo and vicinity/pueblo county below 6300 ft" ~ "pueblo vicinity/pueblo county below 6300 feet",
      zone_name == "lower columbia basin of washington" ~ "lower columbia basin",
      zone_name == "foothills of the blue mountains of washington" ~ "foothills of the blue mountains",
      zone_name == "lower colorado river valley ca" ~ "lower colorado river valley",
      zone_name == "lower colorado river valley az" ~ "lower colorado river valley",
      zone_name == "middle rio grande valley" ~ "middle rio grande valley/albuquerque metro area",
      zone_name == "southeast pinal county including kearny/mammoth/oracle" ~ "southeast pinal county",
      zone_name == "chuska mountains and defiance plateay" ~ "chuska mountains and defiance plateau",
      zone_name == "tohono o'odham nation including sells" ~ "tohono oodham nation",
      zone_name == "san diego county deserts-including the anza borrego desert state park" ~ "san diego county deserts",
      zone_name == "coachella valley-including the palm springs south coast desert district" ~ "coachella valley",
      zone_name == "lower columbia basin of oregon" & zone_state == "OR" ~ "lower columbia basin",
      TRUE ~ zone_name
      )
    )

# Some zone fips change over time for a given zone. For consistency and ease in matching,
# I set the problematic ones to be the same over time. This won't affect the match if
# another zone happened to have this fips because I match on state-fips-name.
xwalk_zones_to_counties <- xwalk_zones_to_counties %>%
  mutate(
    zone_fips = case_when(
      zone_state == "AZ" & zone_name == "tohono oodham nation" ~ "502",
      zone_state == "NM" & zone_name == "southwest desert/mimbres basin" ~ "407",
      zone_state == "OR" & zone_name == "foothills of the blue mountains" ~ "501",
      TRUE ~ zone_fips
    )
  )

# For each zone file, get the unique zones. This will be used for merging with the
# storm details data. At the end, I will then merge back to get the counties affected for each
# storm
distinct_zonefile_by_zone_df <- xwalk_zones_to_counties %>%
  dplyr::distinct(zone_file_date, zone_filename, zone_state, zone_fips, zone_name) %>%
  arrange(zone_file_date, zone_filename, zone_state, zone_fips, zone_name)


# ------------------------------------------------------------------------------
# Merge the zones in the storm details data to the zones in the zone shapefiles data

# Initialize dataframe to store merge results of storm details-to-zone shapefiles
df_merged_storms_to_zones <- data.frame()

# Get the unique file dates from the zone shapefiles
file_dates <- unique(distinct_zonefile_by_zone_df$zone_file_date)

for (i in 1:length(file_dates)) {
  
  # Keep the rows in the storm details data that occur BEFORE this date and ON/AFTER
  # the previous date. We have to treat the tail ends of the date range separately though
  if (i == 1) { # All storm dates BEFORE the second set of zones went into effect
    df_storms <- storm_details_df %>%
      filter(storm_start_date < file_dates[i+1])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  } else if (i == length(file_dates)) { # All storm dates ON/AFTER the last set of zones went into effect
    df_storms <- storm_details_df %>%
      filter(storm_start_date >= file_dates[i])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  } else { # All storm dates ON/AFTER this set of zones went into effect and BEFORE the next set of zones went into effect
    df_storms <- storm_details_df %>%
      filter(storm_start_date >= file_dates[i], storm_start_date < file_dates[i+1])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  }
  
  # Merge the two dataframes
  if (nrow(df_storms > 0)) {
    df <- stata.merge(
      df_storms, df_zones, by = c("zone_state", "zone_fips", "zone_name")
      ) %>%
      arrange(merge, zone_state, zone_fips, zone_name)
    
    n <- df %>% filter(merge == 1) %>% nrow()
    if (n > 0) {
      stop("There are zones in the storms data that aren't in the storms shapefile")
    } else {
      df <- df %>%
        filter(merge == 3) %>%
        dplyr::select(-merge)
      df_merged_storms_to_zones <- bind_rows(df_merged_storms_to_zones, df)
    }
  }
  
}

if (nrow(df_merged_storms_to_zones) != nrow(storm_details_df)) {
  stop("The merged data should have the same number of rows as the storm details data")
}

# ------------------------------------------------------------------------------
# Create a storm details-to-county crosswalk by merging the storm details-forecast
# zone shapefiles data to the forecast zone shapefiles-to-counties data

# Get the counties associated with each forecast zone-zone set in the storm details data
df_merged_storms_to_zones_distinct <- df_merged_storms_to_zones %>%
  dplyr::distinct(zone_state, zone_fips, zone_name, zone_filename)
df_merged_zones_to_counties <- left_join(
  df_merged_storms_to_zones_distinct, xwalk_zones_to_counties, 
    by = c("zone_state", "zone_fips", "zone_name", "zone_filename")
    ) %>%
  dplyr::select(-zone_file_date)

# Now, we have a many-to-many merge problem. The storm details data can have multiple
# storms for a given zone-zone set, and the zones-to-counties data can have multiple
# counties for a given zone. So merging the two on zone gives a m:m merge. Instead,
# I loop through each storm (so only 1 zone) and merge that to the zone-to-counties data
df_merged_storms_to_counties <- data.frame()
for (i in 1:nrow(df_merged_storms_to_zones)) {
  
  df <- left_join(
    df_merged_storms_to_zones[i, ], df_merged_zones_to_counties,
    by = c("zone_state", "zone_fips", "zone_name", "zone_filename")
  )
    
  df_merged_storms_to_counties <- bind_rows(df_merged_storms_to_counties, df)
  
}

# Filter out counties without < 1% area overlap with a given zone. These
# often result from tiny border overlaps between a zone and a county
df_merged_storms_to_counties <- df_merged_storms_to_counties %>%
  filter(pct_county_overlap >= 1) %>%
  dplyr::select(-c(zone_file_date, zone_filename, zone_state, zone_fips, zone_name))

# Prep for merging with the storm warnings data by creating a county-date panel


# Prep the storm warnings data for merging -------------------------------------

# Load the warnings data
dust_storm_warnings_df <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_dust_storm_warnings_cleaned.csv"))

# Filter out counties without < 1% area overlap with a given warning polygon. These
# often result from tiny border overlaps
dust_storm_warnings_df <- dust_storm_warnings_df %>%
  filter(pct_county_overlap >= 1)

# I only want 1 observation for each county-warning type-date. Thus, for each
# county-warning type-date, choose the earliest warning start time
dust_storm_warnings_df <- dust_storm_warnings_df %>%
  group_by(stfp, cntyfp, storm_type, warning_start_date) %>%
  arrange(warning_start_datetime) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Merge in the storm warnings data ---------------------------------------------

df_merged_storms_warnings_counties <- df_merged_storms_to_counties

# Output ------------------------- ---------------------------------------------

# County-by-storm panel
panel_county_by_storm <- df_merged_storms_warnings_counties %>%
  dplyr::select(stabv, stfp, cntyfp, pct_county_overlap, episode_id, event_id,
                storm_start_date, storm_start_datetime,
                storm_end_date, storm_end_datetime) %>%
  arrange(stabv, stfp, cntyfp, storm_start_date, episode_id, event_id)
file_out <- paste0(path_data_int, "/Dust_storms/panel_county x storm_dust storms and warnings")
write_csv(panel_county_by_storm, file_out)

# County-by-year panel

