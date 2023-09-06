#-------------------------------------------------------------------------------
# Create dust maps
# Written by: Gary Schlauch
#-------------------------------------------------------------------------------

# Yearly VIIRS dust map, 0.1 degree lat/lon grid -------------------------------

# Load the dust data
df_dust <- read_csv(paste0(path_int_viirs, "/panel_dustgrid_0p1deg_latlon.csv"))

# Compute the number of dust days by grid cell-year
df_dust_annual <- df_dust %>%
  mutate(year = year(date)) %>%
  group_by(grid_id_0p1latlon, year) %>%
  dplyr::summarize(num_dust_days = sum(dust, na.rm = T)) %>%
  ungroup() %>%
  mutate_all(as.integer)
rm(df_dust)

# Load the grid cell shapefile
shp_grid <- st_read(paste0(path_int_grid, "/grid_latlong_0p1_degree.shp"))

# Make a separate map for each year

df <- df_dust_annual %>% filter(year == 2012) %>% dplyr::select(-year)
shp_grid_dust <- left_join(shp_grid, df, by = c("ID" = "grid_id_0p1latlon")) %>%
  mutate_at(c("num_dust_days"), ~replace_na(., 0))



map <- ggplot() + 
  geom_sf(
    data = shp_grid_dust, 
    aes(fill = num_dust_days),
    lwd = 0
  ) +
  geom_sf(
    data = shp_state,
    lwd = 0.2,
    color = "grey10",
    fill = "transparent"
  ) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod4") +
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

file_out <- paste0(path_output, "/maps/Map_dust_days_by_gridcell.png")
ggsave(file_out)

