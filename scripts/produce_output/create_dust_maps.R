#-------------------------------------------------------------------------------
# Create dust maps
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")

# Load the grid cell shapefile
shp_grid_full <- st_read(paste0(path_data_int, "/grid/grid_latlong_0p1_degree.shp"))

# Load the state shapefile
shp_state <- st_read(paste0(path_data_int, "/census/2010/state/US_state_2010.shp")) %>%
  st_make_valid()

# Intersect the grid cell shapefile with the state shapefile
intersections <- st_intersects(shp_grid_full, shp_state)
shp_grid <- shp_grid_full[lengths(intersections) != 0, ]

# Load the dust data
df_dust <- read_csv(paste0(path_data_int, "/viirs/panel_dustgrid_0p1deg_latlon.csv"))

# Compute the number of dust days by grid cell-year
df_dust_annual <- df_dust %>%
  mutate(year = year(date)) %>%
  group_by(grid_id_0p1latlon, year) %>%
  dplyr::summarize(num_dust_days = sum(dust, na.rm = T)) %>%
  ungroup() %>%
  mutate_all(as.integer)
rm(df_dust)



# Make a separate map for each year



year_nums <- 2012:2022
for (year_num in year_nums) {
  
  df <- df_dust_annual %>% filter(year == year_num) %>% dplyr::select(-year)
  shp_grid_dust <- left_join(shp_grid, df, by = c("ID" = "grid_id_0p1latlon")) %>%
    mutate_at(c("num_dust_days"), ~replace_na(., 0))
  n_dust_days <- shp_grid_dust %>%
    as.data.frame() %>%
    dplyr::summarize(count = sum(num_dust_days))
  
  map <- ggplot() + 
    geom_sf(
      data = shp_grid_dust, 
      aes(fill = num_dust_days),
      lwd = 0,
      color = NA
    ) +
    geom_sf(
      data = shp_state,
      lwd = 0.2,
      color = "grey10",
      fill = "transparent"
    ) +
    scale_fill_continuous(
      na.value = "white",
      limits = c(0, 366),
      breaks = c(0, 100, 200, 300),
      labels = c("0", "100", "200", "300"),
      low = "white",
      high = "red"
    ) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          rect = element_blank(),
          plot.title = element_blank(),
          legend.key = element_rect(linetype = "solid", color = "grey80"),
          legend.title = element_blank(),
          legend.text = element_text(size = 12, color = "black", hjust = 0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.94, 0.25),
          panel.background = element_rect(fill = "grey100", color = "grey100")
    )
  
  file_out <- paste0(
    path_output, "/maps/Map_dust_days_by_gridcell_", as.character(year_num), ".png"
    )
  ggsave(file_out)
  
}





year_nums <- 2012:2022
for (year_num in year_nums) {
  
  df <- df_dust_annual %>% filter(year == year_num) %>% dplyr::select(-year)
  shp_grid_dust <- left_join(shp_grid, df, by = c("ID" = "grid_id_0p1latlon")) %>%
    mutate_at(c("num_dust_days"), ~replace_na(., 0))
  n_dust_days <- shp_grid_dust %>%
    as.data.frame() %>%
    dplyr::summarize(count = sum(num_dust_days))
  
  print(
    paste0("Num dust days in ", as.character(year_num), ": ", as.character(n_dust_days))
  )
  
}
