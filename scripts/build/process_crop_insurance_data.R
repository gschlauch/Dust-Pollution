setwd("/Users/garyschlauch/Documents/github/Dust-Pollution")
source("scripts/setup/00_load_settings.R")
source("scripts/setup/00_load_packages.R")
source("scripts/setup/00_load_functions.R")



df <- read_csv(paste0(path_data_raw, "/Crop_insurance/CI_space_corn.csv")) %>%
  filter(Commodity_Year >= 2005, Commodity_Year <= 2022) %>%
  dplyr::select(Commodity_Name, Insurance_Plan_Name_Abbreviation, GEOID, 
                Location_State_Abbreviation, Net_Reported_Quantity, `Net_Reported_Quantity_%`,
                Subsidy_Amount, `Subsidy_Amount_%`)