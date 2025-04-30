library(devtools)
library(gts)
library(ncdf4)

rea_products = c("glorys-v1", "bran-2020", "hycom-3.1")
path = "C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific"
file_obs = "C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc"

for (product in rea_products) {
  # file
  file_rea = list.files(path = file.path(path,product), pattern =  ".*sst", recursive = TRUE, include.dirs = FALSE, full.names = TRUE)
  
  # path setting
  if (product != "hycom") {
    file_out = file.path(path, "regrid", paste0(product, "-southpacific-sst-monthly-199301-202112-regrid.nc"))
  } else {
    file_out = file.path(path, "regrid", paste0(product, "-southpacific-sst-monthly-199401-201512-regrid.nc"))
  }
  file_tmp = paste0(file_out, ".tmp")  # Temp file
  
  # Checkpoint
  if (file.exists(file_out)) {
    message(product, " : fichier final déjà existant, passage au suivant.")
    next
  }
  if (file.exists(file_tmp)) {
    message(product, " : fichier temporaire détecté, traitement en cours ou avorté, passage au suivant.")
    next
  }
  
  # Creation temp file
  file.create(file_tmp)
  message(product, " : traitement en cours...")
  
  # Read data
  data = read_gts(filename = file_rea)
  oisst = read_gts(filename = file_obs)
  
  if (product != "hycom") {
    data = window(data, start = c(1993, 01), end = c(2021, 12))
    oisst = window(oisst, start = c(1993, 01), end = c(2021, 12))
  } else {
    data = window(data, start = c(1994, 01), end = c(2015, 12))
    oisst = window(oisst, start = c(1994, 01), end = c(2015, 12))
  }
  
  # Regrid
  regrid_product = try(regrid(object = data, grid = oisst, method = "nearest"))
  if(inherits(regrid_product, "try-error")) {
    message(sprintf("Error processing %s", file_out))
    file.remove(file_tmp) # we remove it, so at the end we rerun the errors only
    next
  }
  
  # Save
  write_ncdf(regrid_product, filename = file_out)
  
  # Suppression temp file
  file.remove(file_tmp)
  
  message(product, " : traitement terminé et sauvegardé.")
  
  # Clean RAM
  rm(data, oisst, regrid_product, file_rea)
  gc()

}

