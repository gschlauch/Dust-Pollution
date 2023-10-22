# Set R settings
rm(list=ls())
options(stringsAsFactors = F)
options(scipen = 999)
set.seed(12345)

# Set to location of Box folder
path_box <- "/Users/garyschlauch/Library/CloudStorage/Box-Box/Dust-Pollution"

# File paths based on root folder above
path_data_raw <- paste0(path_box, "/data/raw")
path_data_int <- paste0(path_box, "/data/generated/intermediate")
path_temp <- paste0(path_data_int, "/temp")
path_data_final <- paste0(path_box, "/data/generated/final")
path_output <- paste0(path_box, "/output")

# Custom colors
midblue1 <- "#65b5ff"
midblue2 <- "#3675bd"