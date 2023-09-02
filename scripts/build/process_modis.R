
library(terra)

s <- sds("/Users/garyschlauch/Downloads/MOD08_D3.A2000055.061.2017276160246.hdf")

r <- rast("/Users/garyschlauch/Downloads/MOD08_D3.A2000055.061.2017276160246.hdf")

fileConn<-file("/users/garyschlauch/downloads/temp.txt")
writeLines(names(r), fileConn)
close(fileConn)


s[]
Deep_Blue_Angstrom_Exponent_Land_Mean