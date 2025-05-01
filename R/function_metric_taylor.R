library(ncdf4)

# ------------------------------
# Charger la fonction Taylor
source("R./function_taylor.R")

# ------------------------------
# Charger vos données

glo = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/regrid/glorys-v1-southpacific-sst-monthly-199301-202112-regrid.nc")
GLORYS_data = ncvar_get(glo, "sst")

oisst = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")
OISST_data = ncvar_get(oisst, "sst")
OISST_data = OISST_data[,,137:484]

lon = ncvar_get(oisst, 'lon')
lat = ncvar_get(oisst, 'lat')

# ------------------------------
# Préparation
nlon = length(lon)
nlat = length(lat)
npoints = nlon * nlat

# Pré-allocation
lon_list = numeric(npoints)
lat_list = numeric(npoints)
sd_list = numeric(npoints)
R_list = numeric(npoints)
crmsd_list = numeric(npoints)

row_id = 1
diagram_started = FALSE

# Progression
pb = txtProgressBar(min = 0, max = nlon * nlat, style = 3)
step = 0

# ------------------------------
# Boucle principale

png("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_pixel_glorys.png", width = 1200, height = 1200)
par(mar = c(6, 6, 4, 4))

for (i in 1:nlon) {
  for (j in 1:nlat) {
    
    
    model_ts = GLORYS_data[i, j, ]
    obs_ts = OISST_data[i, j, ]
    
    
    if (sum(!is.na(model_ts) & !is.na(obs_ts)) > 3) {
      
      if (!diagram_started) {
        # Initialisation du diagramme
        metrics = taylor.diagram(ref = obs_ts, model = model_ts, label = "", add = FALSE)
        diagram_started = TRUE
      } else {
        metrics = taylor.diagram(ref = obs_ts, model = model_ts, label = "", add = TRUE)
      }
      
      
      lon_list[row_id] = lon[i]
      lat_list[row_id] = lat[j]
      sd_list[row_id] = metrics$sd.f
      R_list[row_id] = metrics$R
      crmsd_list[row_id] = metrics$crmsd
      
      row_id = row_id + 1
    }
    
    
    step = step + 1
    setTxtProgressBar(pb, step)
  }
}

dev.off()
close(pb)

# ------------------------------
# Créer le data.frame final

valid_index = 1:(row_id - 1)

results_df = data.frame(
  lon = lon_list[valid_index],
  lat = lat_list[valid_index],
  sd = sd_list[valid_index],
  R = R_list[valid_index],
  crmsd = crmsd_list[valid_index]
)


head(results_df)

# Export CSV
write.csv(results_df, "C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/taylor_metrics_pixel_glorys.csv", row.names = FALSE)
