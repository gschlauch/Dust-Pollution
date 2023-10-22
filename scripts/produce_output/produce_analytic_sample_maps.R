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

# Prep crop yield data
crop_yield_df <- read_csv(paste0(path_data_int, "/crops/panel_crop_yields.csv")) %>%
  filter(yield > 0, !is.na(yield), year >= 2008, !(crop %in% c("rice", "lentils"))) %>%
  group_by(stfp, cntyfp, crop) %>%
  dplyr::summarize(nobs = n()) %>%
  ungroup() %>%
  filter(nobs >= 10)

# Merge crop yield data with county shapefile
county_shp <- left_join(county_shp, crop_yield_df, by = c("stfp", "cntyfp")) %>%
  mutate(has_data = as.factor(ifelse(!is.na(crop), 1, 0)))

# Maps for counties with crop yield data by crop -------------------------------

# Loop through crops
unique_crops <- unique(crop_yield_df$crop)
unique_crops <- unique_crops[!is.na(unique_crops)]
for (i in 1:length(unique_crops)) {
  
  crop_name <- unique_crops[i]
  
  county_shp_crop <- county_shp %>%
    filter(crop == crop_name)
  
  p <- ggplot() + 
    geom_sf(
      data = state_shp, 
      lwd = 0.4,
      color = "grey20",
      fill = "transparent"
    ) +
    geom_sf(
      data = county_shp_crop,
      lwd = 0,
      color = "grey20",
      aes(fill = has_data)
    ) +
    scale_fill_manual(
      values = c("0" = "transparent", "1" = midblue1)
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
    path_output, "/maps/map_counties_with_yields_", crop_name, ".png"
    )
  ggsave(file_out)
  
  img = image_read(file_out)
  img_crop <- image_trim(img)
  image_write(img_crop, path = file_out)
  
}

