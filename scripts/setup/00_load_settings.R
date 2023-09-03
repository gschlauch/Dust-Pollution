# Set R settings
rm(list=ls())
options(stringsAsFactors = F)

# Set to location of Box folder
path_box <- "/Users/garyschlauch/Library/CloudStorage/Box-Box/second_year_paper"

# File paths based on root folder above
path_raw <- paste0(path_box, "/data/raw")
path_raw_census <- paste0(path_raw, "/census")
path_raw_epa <- paste0(path_raw, "/epa")
path_raw_improve <- paste0(path_raw, "/improve")
path_raw_viirs_adp <- paste0(path_raw, "/viirs_adp")
path_raw_prism <- paste0(path_raw, "/prism")
path_raw_usda <- paste0(path_raw, "/usda")
path_raw_narr <- paste0(path_raw, "/narr")

path_int <- paste0(path_box, "/data/generated/intermediate")
path_int_viirs <- paste0(path_int, "/viirs")
path_int_epa <- paste0(path_int, "/epa")
path_int_nasa <- paste0(path_int, "/nasa")
path_int_census <- paste0(path_int, "/census")
path_int_usda <- paste0(path_int, "/usda")
path_int_temp <- paste0(path_int, "/temp")
path_int_grid <- paste0(path_int, "/grid")

path_final <- paste0(path_box, "/data/generated/final")
path_final_reg_inputs <- paste0(path_final, "/regression_inputs")
path_final_results <- paste0(path_final, "/results")

path_output <- paste0(path_box, "/output")

