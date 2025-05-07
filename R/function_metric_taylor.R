library(ncdf4)

# ------------------------------
# Charger la fonction Taylor
source("R./function_taylor.R")

# ------------------------------
# Charger vos données

model = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/regrid/hycom-3.1-southpacific-sst-monthly-199301-202112-regrid.nc")
model_data = ncvar_get(model, "sst")
model_data = model_data[,41:320,]

oisst = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")
OISST_data = ncvar_get(oisst, "sst")
OISST_data = OISST_data[,41:320,149:412]

lon = ncvar_get(oisst, 'lon')
lat = ncvar_get(oisst, 'lat')
lat = lat[41:320]

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
rmse_list = numeric(npoints)

row_id = 1
diagram_started = FALSE

# Progression
pb = txtProgressBar(min = 0, max = nlon * nlat, style = 3)
step = 0

# ------------------------------
# Boucle principale

png("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_pixel_hycom_2.png", width = 1200, height = 1200)
par(mar = c(6, 6, 4, 4))

for (i in 1:nlon) {
  for (j in 1:nlat) {
    
    model_ts = model_data[i, j, ]
    obs_ts = OISST_data[i, j, ]
    
    # Initialiser les valeurs par défaut (NA)
    sd_value = NA
    R_value = NA
    crmsd_value = NA
    rmse_value = NA
    
    # Vérifier qu'il y a suffisamment de données valides
    if (sum(!is.na(model_ts) & !is.na(obs_ts)) > 3) {
      
      # Calcul des métriques via Taylor
      if (!diagram_started) {
        metrics = taylor.diagram(ref = obs_ts, model = model_ts, label = "", add = FALSE)
        diagram_started = TRUE
      } else {
        metrics = taylor.diagram(ref = obs_ts, model = model_ts, label = "", add = TRUE)
      }
      
      # Calcul du RMSE
      rmse_value = sqrt(mean((obs_ts - model_ts)^2, na.rm = TRUE))
      
      # Extraire les métriques
      sd_value = metrics$sd.f
      R_value = metrics$R
      crmsd_value = metrics$crmsd
    }
    
    # Stocker les résultats (même si NA)
    lon_list[row_id] = lon[i]
    lat_list[row_id] = lat[j]
    sd_list[row_id] = sd_value
    R_list[row_id] = R_value
    crmsd_list[row_id] = crmsd_value
    rmse_list[row_id] = rmse_value
    
    row_id = row_id + 1
    
    step = step + 1
    setTxtProgressBar(pb, step)
  }
}

dev.off()
close(pb)

# ------------------------------
# Créer le data.frame final

results_df = data.frame(
  lon = lon_list,
  lat = lat_list,
  sd = sd_list,
  R = R_list,
  crmsd = crmsd_list,
  rmse = rmse_list
)

head(results_df)

# Export CSV
write.csv(results_df, "C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv", row.names = FALSE)
