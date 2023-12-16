setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Function to adjust timezones
adjust_timezone <- function(tz_str) {
  case_when(
    tz_str == "EST"                                          ~ "Etc/GMT+5",
    tz_str == "CST" | tz_str == "CST-6"                      ~ "Etc/GMT+6",
    tz_str == "MST" | tz_str == "MST-7" | tz_str == "PDT-7"  ~ "Etc/GMT+7",
    tz_str == "PST" | tz_str == "PST-8"                      ~ "Etc/GMT+8",
    TRUE                                                     ~ NA
  )
}

# ------------------------------------------------------------------------------
# Process the dust storm details data

# Append the raw storm details data
dirpath <- paste0(path_data_raw, "/dust_storms/NWS/storm_details")
files <- list.files(dirpath, pattern = ".csv$")
storm_details_df <- data.frame()
for (i in 1:length(files)) {
  filename <- paste0(dirpath, "/", files[i])
  df <- read_csv(
    filename,
    col_select = c(
      EPISODE_ID, EVENT_ID, EVENT_TYPE, STATE, CZ_TYPE, CZ_FIPS, CZ_NAME, WFO,
      BEGIN_DATE_TIME, END_DATE_TIME, CZ_TIMEZONE
    )
  )
  storm_details_df <- bind_rows(storm_details_df, df)
}

# Fix the storm end date on this observation, which appears to be an error. The
# dates say the storm lasted from 01-APR-11 to 09-APR-11, which is far too long.
# Also, the event narrative references a single day
storm_details_df <- storm_details_df %>%
  dplyr::mutate(END_DATE_TIME = ifelse(
    EPISODE_ID == "50747" & EVENT_ID == "299447", "01-APR-11 18:30:00", END_DATE_TIME
  )
  )

# Clean the storms data
names(storm_details_df) <- tolower(names(storm_details_df))
storm_details_df <- storm_details_df %>%
  filter(event_type == "Dust Storm") %>%
  dplyr::mutate(
    cz_state = get_state_abbreviation_from_name(state),
    cz_fips = str_pad(as.character(cz_fips), width = 3, side = "left", pad = "0"),
    cz_name = tidy_string(cz_name),
    cz_timezone = adjust_timezone(cz_timezone),
    storm_start_date_local = dmy(str_sub(begin_date_time, 1, 9)),
    storm_end_date_local = dmy(str_sub(end_date_time, 1, 9)),
    # storm_start_time_local = hms(str_sub(begin_date_time, 11, 18)),
    # storm_end_time_local = hms(str_sub(end_date_time, 11, 18)),
    storm_start_datetime = dmy_hms(begin_date_time),
    storm_end_datetime = dmy_hms(end_date_time)
  ) %>%
  filter(!(cz_state %in% c("AK", "VI", "PR", "GU", "HI", "MP", "TT", "AS"))) %>%
  dplyr::select(-c(state, event_type, begin_date_time, end_date_time))

# Get states (and their neighboring states) with at least one dust storm
states_and_nbrs_with_storms <- ""
states_with_storms <- dplyr::distinct(storm_details_df, cz_state)
for (state in states_with_storms$cz_state) {
  state_and_nbrs <- get_neighboring_states(state)
  states_and_nbrs_with_storms <- c(states_and_nbrs_with_storms, state_and_nbrs)
}
states_and_nbrs_with_storms <- unique(states_and_nbrs_with_storms[-1])

# Remove the handful of storms that have a county, not a zone. I deal with these later
storm_details_df_counties_not_zones <- storm_details_df %>%
  filter(cz_type != "Z")
storm_details_df <- storm_details_df %>%
  filter(cz_type == "Z") %>%
  dplyr::rename(zone_fips = cz_fips, zone_name = cz_name, zone_state = cz_state) %>%
  dplyr::select(-cz_type)

# Clean the forecast zone names to match those in the forecast zone shapefiles.
# Note that I could just match on zone state and zone fips, but I also try to
# reconcile the names to be extra sure that the zone fips aren't unknowingly changing
# over time between the SED data and forecast zones shapefiles, since they do 
# change over time for some zones. Below, I match on zone state-fips-name
storm_details_df <- storm_details_df %>%
  dplyr::mutate(
    zone_name = case_when(
      #### names look extremely similar and the fips match
      zone_name == "marble canyon and glen canyon" ~ "marble and glen canyons",
      zone_name == "northeast plateaus and mesas from highway 264 north" ~ "northeast plateaus and mesas hwy 264 northward",
      zone_name == "northeast plateaus and mesas south of highway 264" ~ "northeast plateaus and mesas south of hwy 264",
      zone_name == "tohono o odham nation" ~ "tohono oodham nation",
      zone_name == "sw yuma" ~ "yuma/martinez lake and vicinity",
      zone_name == "nw pinal" ~ "northwest and north central pinal county",
      zone_name == "tucson metro area" ~ "tucson metro area including tucson/green valley/marana/vail",
      zone_name == "south central pinal county" ~ "south central pinal county including eloy/picacho peak state park",
      zone_name == "southeast pinal county including kearny/mammoth/oracle" ~ "southeast pinal county",
      zone_name == "eastern cochise county below 5000 feet" ~ "eastern cochise county below 5000 feet including douglas/willcox",
      zone_name == "lower colorado river valley az" ~ "lower colorado river valley ca",
      zone_name == "riverside county eastern deserts" ~ "riverside county/eastern deserts",
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
      zone_name == "e kittitas" ~ "kittitas valley",
      zone_name == "e yakima" ~ "yakima valley",
      zone_name == "southeast pinal county including kearny/mammoth/oracle" ~ "southeast pinal county",
      zone_name == "albuquerque metro area" ~ "middle rio grande valley/albuquerque metro area",
      zone_name == "s washoe t x se & x sw/storey/e carson city/c&e douglas/nw lyon" ~ "greater reno-carson city-minden area",
      zone_name == "pershing/churchill/x se washoe/nc&ne lyon" ~ "western nevada basin and range including pyramid lake",
      zone_name == "emery t nw x ne/c e wayne/p sc carbon" ~ "san rafael swell",
      zone_name == "blue mountain foothills" ~ "foothills of the blue mountains",
      zone_name == "n columbia/se walla walla t x se" ~ "foothills of the blue mountains",
      zone_name == "lower rio grande valley / part of c and e socorro" & zone_fips == "015" ~ "lower rio grande valley",
      zone_name == "n elko cnty" & zone_fips == "031" ~ "northern elko county",
      zone_name == "x e elko" & zone_fips == "033" ~ "extreme eastern elko county",
      zone_name == "grand flat and arches" & zone_fips == "027" ~ "arches/grand flat",
      zone_name == "lower garfield & asotin" & zone_fips == "032" ~ "lower garfield and asotin counties",
      zone_name == "e central sj valley" & zone_fips == "090" ~ "east-central san joaquin valley",
      # #### manually check these by looking up locations and comparing to maps of counties/zones
      zone_name == "c & e la paz" & zone_fips == "021" ~ "west central deserts",
      zone_name == "s gila/ne maricopa/x n pinal" & zone_fips == "024" ~ "southern gila/tonto nf foothills",
      zone_name == "c&e yuma" & zone_fips == "026" ~ "southwest deserts",
      zone_name == "graham/s greenlee" & zone_fips == "030" ~ "upper gila river valley",
      zone_name == "nc riverside" & zone_fips == "030" ~ "joshua tree national park",
      zone_name == "x e riverside / e imperial" & zone_fips == "031" ~ "lower colorado river valley",
      zone_name == "e riverside t x e" & zone_fips == "032" ~ "riverside county/eastern deserts",
      zone_name == "w & c imperial" & zone_fips == "033" ~ "imperial county",
      zone_name == "apple and yucca valleys" & zone_fips == "060" ~ "apple and lucerne valleys",
      zone_name == "c riverside" & zone_fips == "061" ~ "coachella valley",
      zone_name == "sw & w kootenai" & zone_fips == "002" ~ "coeur d'alene area",
      zone_name == "w benewah/w latah" & zone_fips == "003" ~ "idaho palouse",
      zone_name == "n nez perce county" & zone_fips == "026" ~ "lewiston area",
      zone_name == "central deserts" & zone_fips == "028" ~ "northwest and north central pinal county",
      zone_name == "sw sj valley" & zone_fips == "091" ~ "southwestern san joaquin valley",
      zone_name == "san diego county coasts" & zone_fips == "043" ~ "san diego county coastal areas",
      zone_name == "e central sj valley" & zone_fips == "e central sj valley" ~ "east-central san joaquin valley",
      zone_name == "northwest plateau / san juan except x sw and se / nc mckinley" & zone_fips == "001" ~ "northwest plateau",
      # It appears that 048 and 049 from earlier years got combined into 048.
      # Plotting 048 zone, it is the southwest of san bernadino county and
      # northwest of riverside county
      (zone_name == "san bernardino county valley/the inland empire" & zone_fips == "048") |
        (zone_name == "x sw san bernardino" & zone_fips == "048") |
        (zone_name == "riverside county valley/the inland empire" & zone_fips == "049") |
        (zone_name == "w riverside t x nw" & zone_fips == "049") ~ "san bernardino and riverside county valleys -the inland empire",
      # If c stands for central, this may not be correct since the SD county deserts
      # are in the east of the county. however, the storms probably occur in the deserts,
      # so this may be correct
      zone_name == "c san diego" & zone_fips == "062" ~ "san diego county deserts",
      #### check again, fips matched
      zone_name == "c maricopa" ~ "greater phoenix area",
      zone_name == "imperial county except the lower colorado river valley" ~ "imperial county",
      zone_name == "southern wasatch front/lehi/provo/nephi" ~ "southern wasatch front",
      zone_name == "benton/franklin/walla walla t se/e klickitat" ~ "lower columbia basin",
      zone_name == "c pima" ~ "tohono oodham nation",
      zone_name == "e pima" ~ "tohono oodham nation",
      zone_name == "westcentral mountains / mckinley except nc and ne / cibola except x e / n catron / x sw sanduval / x nw bernalillo / x nw socorro" & zone_fips == "008" ~ "west central mountains",
      TRUE ~ zone_name
    )
  )

# UNCOMMENT IF USE STORMS DATA PRIOR TO 2006
# 
# # Adjust the zone fips on these ones
# storm_details_df <- storm_details_df %>%
#   dplyr::mutate(
#     zone_fips = case_when(
#       zone_name == "san bernardino and riverside county valleys -the inland empire" & 
#         zone_fips == "049" &
#         year(storm_start_date_local) <= 2005 ~ "048",
#       TRUE ~ zone_fips
#     )
#   )
# 
# # Have to drop this one since I can't identify its exact location
# storm_details_df <- storm_details_df %>%
#   filter(!(zone_state == "CA" & zone_name == "san bernardino t sw & x se" & storm_start_date == date("2002-03-13")))
# 

# Some zone fips change over time for a given zone. For consistency and ease in matching,
# I set the problematic ones to be the same over time. This won't affect the match if
# another zone happened to have this fips because I match later on using
# state, zone fips, and zone name.
storm_details_df <- storm_details_df %>%
  dplyr::mutate(
    zone_fips = case_when(
      zone_state == "AZ" & zone_name == "tohono oodham nation" ~ "502",
      zone_state == "NM" & zone_name == "southwest desert/mimbres basin" ~ "407",
      zone_state == "OR" & zone_name == "foothills of the blue mountains" ~ "501",
      TRUE ~ zone_fips
    )
  )

# ------------------------------------------------------------------------------
# Prep the forecast zone-to-county crosswalk

# Filter the forecast zones-to-counties crosswalk to states (and their neighbors)
# with at least one storm
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
  dplyr::mutate(
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
      # zone_filename == "z_04ap17.shp" ~ dmy("04 April 2017"), # 19 January 2017, Effective 04 April 2017. THIS FILE HAS MERGING ISSUES, SO I REMOVE IT
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

n_distinct_file <- xwalk_zones_to_counties %>%
  dplyr::distinct(zone_file_date) %>%
  nrow()
n_distinct_file_and_date <- xwalk_zones_to_counties %>%
  dplyr::distinct(zone_filename) %>%
  nrow()
if (n_distinct_file != n_distinct_file_and_date) {
  stop("2 or more files share a file date")
}

# # Have more updated version that went into effect and these look like they
# never went into effect, so I exclude them: "z_07oc14.shp", "z_01ap14.shp

# Update the forecast zone names to be more consistent over time
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
  dplyr::mutate(
    zone_fips = case_when(
      zone_state == "AZ" & zone_name == "tohono oodham nation" ~ "502",
      zone_state == "NM" & zone_name == "southwest desert/mimbres basin" ~ "407",
      zone_state == "OR" & zone_name == "foothills of the blue mountains" ~ "501",
      TRUE ~ zone_fips
    )
  )

# For each zone file, get the unique zones. This will be used for merging with the
# storm details data. At the end, I will then merge back to get the counties affected
# by each storm event
distinct_zonefile_by_zone_df <- xwalk_zones_to_counties %>%
  dplyr::distinct(zone_file_date, zone_filename, zone_state, zone_fips, zone_name) %>%
  arrange(zone_file_date, zone_filename, zone_state, zone_fips, zone_name)


# ------------------------------------------------------------------------------
# Merge the forecast zones in the storm details data with the forecast zones in the
# zone shapefiles data to help create a storm-to-county dataset in the next section

# Initialize dataframe to store merge results of storm details-to-zone shapefiles
df_merged_storms_to_zones <- data.frame()

# Get the unique file dates from the zone shapefiles
file_dates <- unique(distinct_zonefile_by_zone_df$zone_file_date)

for (i in 1:length(file_dates)) {
  # Keep the rows in the storm details data that occur BEFORE this date and ON/AFTER
  # the previous date. We have to treat the tail ends of the date range separately though
  if (i == 1) { # All storm dates BEFORE the second set of zones went into effect.
                # I use the earliest zone file I have for storms that went occurred before
                # that zone file went into effect. Only 3 months lag for 2006, zones don't
                # change much over time.
    df_storms <- storm_details_df %>%
      filter(storm_start_date_local < file_dates[i + 1])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  } else if (i == length(file_dates)) { # All storm dates ON/AFTER the last set of zones went into effect
    df_storms <- storm_details_df %>%
      filter(storm_start_date_local >= file_dates[i])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  } else { # All storm dates ON/AFTER this set of zones went into effect and BEFORE the next set of zones went into effect
    df_storms <- storm_details_df %>%
      filter(storm_start_date_local >= file_dates[i], 
             storm_start_date_local < file_dates[i + 1])
    df_zones <- distinct_zonefile_by_zone_df %>%
      filter(zone_file_date == file_dates[i])
  }

  # Merge the two dataframes
  if (nrow(df_storms > 0)) {
    df <- stata.merge(
      df_storms, df_zones,
      by = c("zone_state", "zone_fips", "zone_name")
    ) %>%
      arrange(merge, zone_state, zone_fips, zone_name)

    n1 <- df %>% filter(merge == 1) %>% nrow()
    if (n1 > 0) {
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
# Create a storm-to-county crosswalk by merging the storm-by-forecast zone
# data with the forecast zone-by-county data

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
# counties for a given zone-zone set. So merging the two on zone gives a m:m merge. Instead,
# I loop through each row in the storms data (so I only have 1 zone there) and
# merge that to the zone-to-counties data
df_merged_storms_to_counties <- data.frame()
for (i in 1:nrow(df_merged_storms_to_zones)) {
  df <- left_join(
    df_merged_storms_to_zones[i, ], df_merged_zones_to_counties,
    by = c("zone_state", "zone_fips", "zone_name", "zone_filename")
  )

  df_merged_storms_to_counties <- bind_rows(df_merged_storms_to_counties, df)
}

# Add back in the handful of storms that were provided counties instead of forecast zones
# in the raw storm details data
storm_details_df_counties_not_zones <- storm_details_df_counties_not_zones %>%
  dplyr::rename(cntyfp = cz_fips, stabv = cz_state) %>%
  dplyr::mutate(stfp = case_when(
    stabv == "AR" ~ "05", stabv == "KS" ~ "20", TRUE ~ NA
  )) %>%
  dplyr::select(-c(cz_type, cz_name))
df_merged_storms_to_counties <- bind_rows(
  df_merged_storms_to_counties, storm_details_df_counties_not_zones
)

# Check that there are no duplicates
check_df_unique_by(df_merged_storms_to_counties, episode_id, event_id, stfp, cntyfp)

# Reconcile duplicate reports for the same dust storm in a county --------------



# ADD LOOP HERE IF WANT TO DO ITERATIONS ON pct_county_overlap THRESHOLD: 5%, 10%, 25%.
# NOTE: I ALSO NEED TO ITERATE IN THE COMBINE_NWS_STORMS_AND_WARNINGS FILE SINCE
# THAT'S WHERE I FILTER THE WARNINGS DATA AS WELL




# Filter out county observations that don't meet the 1% overlap threshold with the
# forecast zone used to assign the county a storm
df_merged_storms_to_counties <- df_merged_storms_to_counties %>%
  filter(pct_county_overlap >= 1)

# Convert datetimes to UTC to standardize them and to match the storm warnings data
# later on. Note that I preserve storm_start_date_local and storm_end_date_local
# to have the correct date(s) the storm actually occurred
df_merged_storms_to_counties <- df_merged_storms_to_counties %>%
  dplyr::mutate(
    hours_to_add = as.numeric(str_sub(cz_timezone, -1, -1)),
    storm_start_datetime_utc = storm_start_datetime + hours(hours_to_add),
    storm_end_datetime_utc = storm_end_datetime + hours(hours_to_add)
  )

# Tidy
df_merged_storms_to_counties <- df_merged_storms_to_counties %>%
  dplyr::select(stabv, stfp, cntyfp, storm_start_date_local, storm_end_date_local,
                storm_start_datetime_utc, storm_end_datetime_utc) %>%
  arrange(stabv, cntyfp, storm_start_datetime_utc, storm_end_datetime_utc)

# Reconcile duplicate storm reports for a given county. This can occur because
# the same storm was reported multiple times in a given forecast zone, or if
# the same storm crossed multiple forecast zones that a county overlaps with. As
# a rule, I reconcile reports that occur within 8 hours of each other
# for a given county
time_diff_threshold <- 8
df <- df_merged_storms_to_counties

# Step 1: for storms that start at the same time, take the max end time
df <- df %>%
  group_by(stabv, cntyfp, storm_start_datetime_utc) %>%
  dplyr::mutate(
    rownum = row_number(),
    storm_end_datetime_utc = max(storm_end_datetime_utc)
    ) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  dplyr::select(-rownum)

# Step 2: similar to step 1, but for storms that end at the same time, take the 
# min start time
df <- df %>%
  group_by(stabv, cntyfp, storm_end_datetime_utc) %>%
  dplyr::mutate(
    rownum = row_number(),
    storm_start_datetime_utc = min(storm_start_datetime_utc)
  ) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  dplyr::select(-rownum)

# Step 3: combine storms that start/end within time_diff_threshold of each other.
# ie, if storm X and Y are in county A and storm X ends within 8 hours of when
# storm Y begins, combine the two storms by taking their min(start time) and
# max(end time)
df <- df %>%
  group_by(stabv, cntyfp) %>%
  arrange(stabv, cntyfp, storm_start_datetime_utc) %>%
  # Figure out which storms should be combined
  dplyr::mutate(
    time_diff = difftime(
      storm_start_datetime_utc, lag(storm_end_datetime_utc), units = "hours"
      ),
    # TRUE (=1) if condition met, +1 each time another true is encountered
    group_flag = cumsum(time_diff > time_diff_threshold | is.na(time_diff))
    ) %>%
  ungroup() %>%
  # Combine the storms (I make separate variables to more easily inspect that
  # things are being done correct)
  group_by(stabv, cntyfp, group_flag) %>%
    dplyr::mutate(
      rownum = row_number(),
      storm_start_date_local_cmb = min(storm_start_date_local),
      storm_start_datetime_utc_cmb = min(storm_start_datetime_utc),
      storm_end_date_local_cmb = max(storm_end_date_local),
      storm_end_datetime_utc_cmb = max(storm_end_datetime_utc),
    ) %>%
    ungroup() %>%
    filter(rownum == 1) %>%
  # Tidy
  dplyr::select(-c(rownum, group_flag, time_diff, storm_start_date_local,
                   storm_end_date_local, storm_start_datetime_utc, 
                   storm_end_datetime_utc)) %>%
  dplyr::rename(
    storm_start_date_local = storm_start_date_local_cmb,
    storm_start_datetime_utc = storm_start_datetime_utc_cmb,
    storm_end_date_local = storm_end_date_local_cmb,
    storm_end_datetime_utc = storm_end_datetime_utc_cmb
  )
  
# Output
write_csv(df, paste0(path_data_int, "/Dust_storms/NWS_county_dust_storms_cleaned.csv"))
