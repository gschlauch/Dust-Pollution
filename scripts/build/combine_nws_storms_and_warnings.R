
# 
# # Prep the storm warnings data for merging -------------------------------------
# 
# # Load the warnings data
# dust_storm_warnings_df <- read_csv(paste0(path_data_int, "/Dust_storms/NWS_dust_storm_warnings_cleaned.csv")) %>%
#   filter(phenom == "DS") %>%
#   dplyr::select(-phenom)
# 
# # Filter out counties below the pct_overlap_threshold area overlap with a given warning
# # polygon
# dust_storm_warnings_df <- dust_storm_warnings_df %>%
#   filter(pct_county_overlap >= pct_overlap_threshold)
# 
# # Convert all datetimes to MST to get the (almost certainly) correct date
# dust_storm_warnings_df <- dust_storm_warnings_df %>%
#   mutate(
#     warning_start_datetime = with_tz(warning_start_datetime, "MST"),
#     warning_start_date = date(warning_start_datetime),
#     warning_end_datetime = with_tz(warning_end_datetime, "MST"),
#     warning_end_date = date(warning_end_datetime)
#   )
# 
# # Remove the datetime variables for now, just use the dates
# dust_storm_warnings_df <- dust_storm_warnings_df %>%
#   dplyr::select(-c(warning_start_datetime, warning_end_datetime))
# 
# # By county-start date-significance, choose the largest county overlap
# dust_storm_warnings_df <- dust_storm_warnings_df %>%
#   group_by(stabv, stfp, cntyfp, warning_start_date, warning_end_date, sig) %>%
#   dplyr::summarize(pct_county_overlap = max(pct_county_overlap)) %>%
#   ungroup() %>%
#   dplyr::mutate(id = paste0(stfp, cntyfp)) %>%
#   dplyr::select(id, warning_start_date, warning_end_date, sig)
# 
# # By county-start date-significance, choose the
# 
# # Initialize a full county-by-date panel from 2005 through 2022
# counties <- dust_storm_warnings_df %>%
#   dplyr::distinct(id)
# dates <- data.frame(
#   date = seq(as.Date("2005-01-01"), as.Date("2022-12-31"), by = "days")
# )
# warnings_panel_df <- crossing(counties, dates) %>%
#   mutate(W = 0, Y = 0)
# 
# # Loop through the actual warnings data, setting the warning (W) and advisory (Y)
# # dummies equal to 1 when a warning or advisory was in effect
# n <- nrow(dust_storm_warnings_df)
# for (i in 1:n) {
#   print(paste0("Creating county-date warning panel: ", i, "/", n))
#   id_i <- dust_storm_warnings_df[i, "id"][[1]]
#   start_i <- dust_storm_warnings_df[i, "warning_start_date"][[1]]
#   end_i <- dust_storm_warnings_df[i, "warning_end_date"][[1]]
#   alert_type_i <- dust_storm_warnings_df[i, "sig"][[1]]
#   warnings_panel_df[
#     warnings_panel_df$id == id_i &
#       warnings_panel_df$date >= start_i &
#       warnings_panel_df$date <= end_i,
#   ][[alert_type_i]] <- 1
# }
# 
# warnings_panel_df <- warnings_panel_df %>%
#   dplyr::rename(warning = W, advisory = Y)
# 
# # Need to identify a single warning/advisory that can occur over multiple days
# 
# 
# 

# # Process the storms data
# names(storm_details_df) <- tolower(names(storm_details_df))
# storm_details_df <- storm_details_df %>%
#   filter(event_type == "Dust Storm") %>%
#   mutate(
#     cz_state = get_state_abbreviation_from_name(state),
#     cz_fips = str_pad(as.character(cz_fips), width = 3, side = "left", pad = "0"),
#     cz_name = tidy_string(cz_name),
#     storm_start_date_local = dmy(str_sub(begin_date_time, 1, 9)),
#     storm_end_date_local = dmy(str_sub(end_date_time, 1, 9)),
#     storm_start_time_local = hms(str_sub(begin_date_time, 11, 18)),
#     storm_end_time_local = hms(str_sub(end_date_time, 11, 18)),
#   ) %>%
#   filter(!(cz_state %in% c("AK", "VI", "PR", "GU", "HI", "MP", "TT", "AS"))) %>%
#   dplyr::select(-c(state, event_type)) %>%
#   dplyr::select(episode_id, event_id, begin_date_time, end_date_time, cz_state, 
#                 cz_timezone, storm_start_date_local, storm_end_date_local,
#                 storm_start_time_local, storm_end_time_local)
# 
# adjust_timezone <- function(tz_str) {
#   case_when(
#     tz_str == "EST"                                          ~ "Etc/GMT+5",
#     tz_str == "CST" | tz_str == "CST-6"                      ~ "Etc/GMT+6",
#     tz_str == "MST" | tz_str == "MST-7" | tz_str == "PDT-7"  ~ "Etc/GMT+7",
#     tz_str == "PST" | tz_str == "PST-8"                      ~ "Etc/GMT+8",
#     TRUE                                                     ~ NA
#   )
# }
# 
# storm_details_df <- storm_details_df %>%
#   mutate(
#     storm_start_datetime_mst = as.POSIXct(NA, tz = "MST"), 
#     storm_end_datetime_mst = as.POSIXct(NA, tz = "MST"), 
#     cz_timezone = adjust_timezone(cz_timezone)
#   )
# 
# n <- nrow(storm_details_df)
# for (i in 1:n) {
#   
#   print(paste0(i, "/", n))
#   
#   # Grab values
#   begin_datetime_i <- storm_details_df[i, "begin_date_time"]
#   end_datetime_i <- storm_details_df[i, "end_date_time"]
#   timezone_i <- storm_details_df[i, "cz_timezone"]
#   
#   # Get the local times
#   begin_datetime_local_i <- force_tz(dmy_hms(begin_datetime_i, tz = "UTC"), timezone_i)
#   end_datetime_local_i <- force_tz(dmy_hms(end_datetime_i, tz = "UTC"), timezone_i)
#   
#   # Change the times to MST
#   begin_datetime_mst_i <- with_tz(begin_datetime_local_i, tz = "MST")
#   end_datetime_mst_i <- with_tz(end_datetime_local_i, tz = "MST")
#   
#   # Store results
#   storm_details_df[i, "storm_start_datetime_mst"] <- begin_datetime_mst_i
#   storm_details_df[i, "storm_end_datetime_mst"] <- end_datetime_mst_i
#   
# }
# 
# View(storm_details_df)
# 
# storm_details_df %>% 
#   mutate(
#     diff_start = date(storm_start_datetime_mst) - storm_start_date_local,
#     diff_end = date(storm_end_datetime_mst) - storm_end_date_local
#   ) %>%
#   dplyr::count(diff_start, diff_end)


# 
# 
# 
# 
# 
# 
# # ------------------------------------------------------------------------------
# # Merge the county-storm-date panel with the county-storm warning-date panel
# 
# county_day_panel_df <- full_join(county_storm_panel_df, warnings_panel_df,
#                                  by = c("id", "date")) %>%
#   dplyr::mutate_at(c("dust_storm", "warning", "advisory"), ~replace_na(., 0))
