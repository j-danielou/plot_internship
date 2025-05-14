library(ncdf4)
library(lubridate)
library(dplyr)
library(maps)
library(ggplot2)


# === Chmaps# === Charger fichier NetCDF ===
nc_file =  nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/regrid/glorys-v1-southpacific-sst-monthly-199301-202112-regrid.nc")
nc_oisst = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")


# === Lire les dimensions ===
lon = ncvar_get(nc_file, "longitude")[1:684]  
lat = ncvar_get(nc_file, "latitude")[41:320] 
time = ncvar_get(nc_oisst, "time")[137:484]

# Convertir le temps en date
time_units = ncatt_get(nc_oisst, "time", "units")$value
origin_str = sub(".*since ", "", time_units)
dates = as.Date(time, origin = origin_str)

# === Lire la variable SST ===

sst = ncvar_get(nc_file, "sst")[1:684,41:320,]

# === Fermer le fichier ===
nc_close(nc_file)

# === Aplatir les données ===
grid = expand.grid(
  lat = lat,
  lon = lon,
  time = dates
)
sst2 = aperm(sst, c(2, 1, 3))
grid$sst = as.vector(sst2)

saveRDS(grid, file = "C:/Users/jdanielou/Desktop/rds/sst_glorys.rds")


