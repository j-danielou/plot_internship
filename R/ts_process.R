library(gts)
library(ncdf4)

# === Chargement des données ===
datasets <- list(
  oisst = read_gts("Y:/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc"),
  glorys = read_gts("Y:/reanalysis/regional/southpacific/glorys-v1/glorys-v1-southpacific-sst-monthly-199301-202312.nc"),
  bran = read_gts("Y:/reanalysis/regional/southpacific/bran-2020/bran-2020-southpacific-sst-monthly-199301-202312.nc"),
  hycom = read_gts("Y:/reanalysis/regional/southpacific/hycom-3.1/gofs-3.1-southpacific-sst-monthly-199401-201512.nc")
)

# === Masque uniquement pour OISST ===
datasets$oisst$grid$mask <- mask(datasets$oisst$x)

# === Restriction temporelle ===
for (name in names(datasets)) {
  message("Start window subsetting for : ", name)
  datasets[[name]] <- window(datasets[[name]], start = c(1993, 1), end = c(2021, 12))
  message("Window subsetting performed succesfully for : ", name)
}

# === Définition des régions ===
regions <- list(
  nw = list(lon = c(120, 180), lat = c(-27.5, 5)),
  nc = list(lon = c(180, 240), lat = c(-27.5, 5)),
  ne = list(lon = c(240, 300), lat = c(-27.5, 5)),
  sw = list(lon = c(120, 180), lat = c(-60, -27.5)),
  sc = list(lon = c(180, 240), lat = c(-60, -27.5)),
  se = list(lon = c(240, 300), lat = c(-60, -27.5))
)

# === Répertoire de sortie ===
output_base <- "C:/Users/jdanielou/Desktop/plots_internship/ts_csv"

# === Fonction de traitement et d’exportation ===
process_and_export <- function(data, name) {
  # Full region (moyenne Sud Pacifique)
  message("Mean South Pacific start for : ", name)
  full_mean <- mean(data, by = "time")
  
  write.table(full_mean, file = file.path(output_base, name, paste0(name, "_sp.csv")),
              row.names = FALSE, col.names = FALSE)
  message("Mean South Pacific perfomed succesfully for : ", name)
  
  
  # MHW region
  mhw_data <- subset(data, lat = regions$mhw$lat, lon = regions$mhw$lon)
  
  message("Mean El Niño Area start for : ", name)
  mhw_mean <- mean(mhw_data, by = "time")
  
  write.table(mhw_mean, file = file.path(output_base, name, paste0(name, "_mhw.csv")),
              row.names = FALSE, col.names = FALSE)
  message("Mean El Niño Area perfomed succesfully for : ", name)
  # Subregions
  for (region in names(regions)) {
    r <- regions[[region]]
    sub_data <- subset(data, lat = r$lat, lon = r$lon)
    
    message("Mean subregions (", r, ") start for : ", name)
    sub_mean <- mean(sub_data, by = "time")
    
    write.table(sub_mean, file = file.path(output_base, name, paste0(name, "_", region, ".csv")),
                row.names = FALSE, col.names = FALSE)
    message("Mean subregions (", r, ") perfomed succesfully for : ", name)
  }
}

# === Boucle principale ===
for (name in names(datasets)) {
  process_and_export(datasets[[name]], name)  # ex: "oisst" → "OISST"
}


#------------------Process for El Niño zones (1+2, 3, 3.4, 4)-------------------
elnino = list(
  zone_1.2 = list(lon = c(270, 280), lat = c(-10,0)),
  zone_3 = list(lon = c(210, 270), lat = c(-5,5)),
  zone_3.4 = list(lon = c(190, 240), lat = c(-5,5)),
  zone_4= list(lon = c(160, 210), lat = c(-5,5))
)


process_elnino_area <- function(data, name) {
  # Subregions El Niño
  for (zone in names(elnino)) {
    r <- elnino[[zone]]
    sub_data <- subset(data, lat = r$lat, lon = r$lon)
    
    message("Mean subregions (", r, ") start for : ", name)
    sub_mean <- mean(sub_data, by = "time")
    
    write.table(sub_mean, file = file.path(output_base, name, paste0(name, "_", zone, ".csv")),
                row.names = FALSE, col.names = FALSE)
    message("Mean subregions (", r, ") perfomed succesfully for : ", name)
  }
}

# === Boucle principale ===
for (name in names(datasets)) {
  process_elnino_area(datasets[[name]], name)  # ex: "oisst" → "OISST"
}
