setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")

# Prep data --------------------------------------------------------------------

# Load 2020 state shapefile
state_shp <- st_read(paste0(path_data_int, "/census/2020/state/US_state_2020.shp"))

# Load 2020 county shapefile
county_shp <- st_read(paste0(path_data_int, "/census/2020/county/US_county_2020.shp")) %>%
  dplyr::select(-c(cntyname, cntyarea))

# Prep the dust storm data
storm_df <- read_csv(paste0(path_data_int, "/Dust_storms/panel_county x storm_dust storms and warnings")) %>%
  filter(year(storm_start_date) >= 2008) %>%
  dplyr::distinct(stfp, cntyfp) %>%
  mutate(any_storm = 1)

# Merge crop yield data with county shapefile
county_shp <- left_join(county_shp, storm_df, by = c("stfp", "cntyfp")) %>%
  mutate(any_storm = as.factor(ifelse(is.na(any_storm), 0, any_storm)))

# Map if counties that ever experience a dust storm --------------------------

p <- ggplot() + 
  geom_sf(
    data = county_shp,
    lwd = 0.1,
    color = "grey20",
    aes(fill = any_storm)
  ) +
  scale_fill_manual(
    values = c("0" = "transparent", "1" = midblue1)
    ) +
  geom_sf(
    data = state_shp, 
    lwd = 0.4,
    color = "grey20",
    fill = "transparent"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "grey100", color = "grey100")
  )

file_out <- paste0(
  path_output, "/maps/map_counties_with_any_storm.png"
  )
ggsave(file_out)

img = image_read(file_out)
img_crop <- image_trim(img)
image_write(img_crop, path = file_out)

# Annual time series of number of storms ---------------------------------------
storm_df_annual <- storm_df %>%
  mutate(year = year(storm_start_date)) %>%
  dplyr::distinct(episode_id, event_id, year) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(num_storms = n()) %>%
  ungroup()

p <- ggplot() + 
  geom_point(data = storm_df_annual, aes(x = year, y = num_storms)) +
  geom_smooth(data = storm_df_annual, aes(x = year, y = num_storms), 
              method = "lm", se = FALSE, color = "blue", size = 1) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.25)
  ) +
  labs(x = "", y = "Number of storms")

file_out <- paste0(
  path_output, "/figures/timeseries_num_dust_storms.png"
)
ggsave(file_out)




