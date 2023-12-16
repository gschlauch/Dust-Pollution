setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Combine NWS storms and warnings ----------------------------------------------

# Load the data
df_storms <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_county_dust_storms_cleaned.csv")) %>%
  mutate(warning = NA, lead_time = NA)
df_warnings <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_dust_storm_warnings_cleaned.csv")) %>%
  filter(pct_county_overlap >= 1)

# For each county-storm, check if its start time falls within the period of an 
# active warning in that county. If so, we say that the storm was given a warning.
# Also compute the number of hours in advance warnings are issued

# Loop through storms
n <- nrow(df_storms)
for (i in 1:n) {
  
  print(paste(i, "/", n))
  
  # Get info on storm i
  df_storms_i <- df_storms[i, ]
  state_i <- df_storms_i[["stabv"]]
  county_i <- df_storms_i[["cntyfp"]]
  start_time_i <- df_storms_i[["storm_start_datetime_utc"]]
  
  # Filter the warnings to the state and county of storm i
  df_warnings_i <- df_warnings %>% 
    filter(stabv == state_i, cntyfp == county_i)
  
  # Determine whether the storm falls within the warning window. If the storm falls
  # within multiple warnings for a county (which can happen if a warning was issued
  # for different forecast zones/times that overlap a county), use the earliest
  # warning to compute the lead time between when the warning was issued and
  # when the storm hit
  warning <- 0
  lead_time <- NA
  if (nrow(df_warnings_i) > 0) {
    df_warnings_i_1 <- df_warnings_i %>%
      dplyr::mutate(
        start_time_diff = difftime(warning_start_datetime_utc, start_time_i, units = "hours"),
        end_time_diff = difftime(warning_end_datetime_utc, start_time_i, units = "hours"),
        warning = ifelse(start_time_diff <= 0 & end_time_diff >= 0, 1, 0)
      ) %>%
      filter(warning == 1)
    
    if (nrow(df_warnings_i_1) > 0) {
      warning <- 1
      lead_time <- min(as.numeric(df_warnings_i_1[["start_time_diff"]]))
    }

  }
  
  # Write the data to the county-storms dataset
  df_storms[i, "warning"] <- warning
  df_storms[i, "lead_time"] <- lead_time
  
}

# Output the county-storm data
write_csv(df_storms, paste0(path_data_int, "/dust_storms/NWS_ds_and_dswarnings_combined_stormlevel.csv"))

# Aggregate the storms and warnings data to the county-quarter level -----------

# Sum the number of storms by county-quarter
df_storms <- df_storms %>%
  dplyr::mutate(
    dummy = 1,
    year = year(storm_start_date_local),
    quarter = quarter(storm_start_date_local)
  ) 

df_storms <- df_storms %>%
  group_by(stabv, stfp, cntyfp, year, quarter) %>%
  dplyr::mutate(
    rownum = row_number(),
    num_ds = sum(dummy),
    num_ds_nw = sum(dummy[warning == 0]),
    num_ds_w = sum(dummy[warning == 1])
  ) %>%
  ungroup() %>%
  filter(rownum == 1) %>%
  dplyr::select(stabv, stfp, cntyfp, year, quarter, num_ds, num_ds_nw, num_ds_w)

# First, I compute the total number of warnings that were issued in a county-quarter.
# This is made more complicated by the fact that multiple warnings can be issued 
# for the same event at slightly different times (eg, if two different 
# forecast zones issued a warning and the county overlaps with both forecast 
# zones). I follow the same method as I did when dealing with duplicate storm 
# reports in process_nws_storms_data.R

# To get the most accurate date the warning started/ended in local time, I 
# convert the UTC times to MST (most if not all warnings are issued in the West 
# or Midwest)
df_warnings <- df_warnings %>%
  dplyr::mutate(
    warning_start_date_mst = as_date(warning_start_datetime_utc - hours(7)),
    warning_end_date_mst = as_date(warning_start_datetime_utc - hours(7))
    )
time_diff_threshold <- 8

# Step 1: for warnings that start at the same time, take the max end time
df_warnings <- df_warnings %>%
  group_by(stabv, cntyfp, warning_start_datetime_utc) %>%
  dplyr::mutate(
    rownum = row_number(),
    warning_end_datetime_utc = max(warning_end_datetime_utc)
  ) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  dplyr::select(-rownum)

# Step 2: similar to step 1, but for warnings that end at the same time, take the 
# min start time
df_warnings <- df_warnings %>%
  group_by(stabv, cntyfp, warning_end_datetime_utc) %>%
  dplyr::mutate(
    rownum = row_number(),
    warning_start_datetime_utc = min(warning_start_datetime_utc)
  ) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  dplyr::select(-rownum)

# Step 3: combine warnings that start/end within time_diff_threshold of each other.
# ie, if warnings X and Y are in county A and warning X ends within 8 hours of when
# warning Y begins, combine the two warnings by taking their min(start time) and
# max(end time)
df_warnings <- df_warnings %>%
  group_by(stabv, cntyfp) %>%
  arrange(stabv, cntyfp, warning_start_datetime_utc) %>%
  # Figure out which storms should be combined
  dplyr::mutate(
    time_diff = difftime(
      warning_start_datetime_utc, lag(warning_end_datetime_utc), units = "hours"
    ),
    # TRUE (=1) if condition met, +1 each time another true is encountered
    group_flag = cumsum(time_diff > time_diff_threshold | is.na(time_diff))
  ) %>%
  ungroup() %>%
  # Combine the storms
  group_by(stabv, cntyfp, group_flag) %>%
  dplyr::mutate(
    rownum = row_number(),
    warning_start_date_mst_cmb = min(warning_start_date_mst),
    warning_start_datetime_utc_cmb = min(warning_start_datetime_utc),
    warning_end_date_mst_cmb = max(warning_end_date_mst),
    warning_end_datetime_utc_cmb = max(warning_end_datetime_utc)
  ) %>%
  ungroup() %>%
  filter(rownum == 1) %>%
  # Tidy
  dplyr::select(-c(rownum, group_flag, time_diff, warning_start_date_mst,
                   warning_end_date_mst, warning_start_datetime_utc, 
                   warning_end_datetime_utc)) %>%
  dplyr::rename(
    warning_start_date_mst = warning_start_date_mst_cmb,
    warning_start_datetime_utc = warning_start_datetime_utc_cmb,
    warning_end_date_mst = warning_end_date_mst_cmb,
    warning_end_datetime_utc = warning_end_datetime_utc_cmb
  )

# Sum the number of warnings by county-quarter
df_warnings <- df_warnings %>%
  dplyr::mutate(
    year = year(warning_start_date_mst),
    quarter = quarter(warning_start_date_mst)
  ) %>%
  # If later add blowing dust alerts, can add that to the count below
  dplyr::count(stabv, stfp, cntyfp, year, quarter, name = "num_warnings")

# Merge the storms and warnings
df_storms_warnings <- full_join(df_storms, df_warnings, 
                                by = c("stabv", "stfp", "cntyfp", "year", "quarter")) %>%
  mutate_at(vars(num_ds, num_ds_nw, num_ds_w, num_warnings), ~replace_na(., 0)) %>%
  dplyr::mutate(num_f_warnings = num_warnings - num_ds_w)

# Output
write_csv(
  df_storms_warnings, 
  paste0(path_data_int, "/dust_storms/NWS_ds_and_dswarnings_combined_quarterly.csv")
  )


