library(ncdf4)
library(lubridate)
library(dplyr)
library(maps)
library(ggplot2)


# === Chmaps# === Charger fichier NetCDF ===
nc_file =  nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/esacci-v5.5/esacci-v5.5-southpacific-sss-monthly-201001-202312.nc")
nc_oisst = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/esacci-v5.5/esacci-v5.5-southpacific-sss-monthly-201001-202312.nc")


# === Lire les dimensions ===
lon = ncvar_get(nc_file, "lon")[]  
lat = ncvar_get(nc_file, "lat")[] 
time = ncvar_get(nc_oisst, "time")[]

# Convertir le temps en date
time_units = ncatt_get(nc_oisst, "time", "units")$value
origin_str = sub(".*since ", "", time_units)
dates = as.Date(time, origin = origin_str)

# === Lire la variable SST ===

sss = ncvar_get(nc_file, "sss")[]

# === Fermer le fichier ===
nc_close(nc_file)

# === Aplatir les données ===
grid = expand.grid(
  lat = lat,
  lon = lon,
  time = dates
)
sss2 = aperm(sss, c(2, 1, 3))
grid$sss = as.vector(sss2)

saveRDS(grid, file = "C:/Users/jdanielou/Desktop/rds/sss_esa.rds")


