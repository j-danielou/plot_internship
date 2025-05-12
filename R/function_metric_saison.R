library(ncdf4)
# ----------------------------------
# Fonction rapide de calcul Taylor
fast_taylor_metrics = function(ref, model, normalize = TRUE) {
  ref = as.vector(ref)
  model = as.vector(model)
  
  valid = which(!is.na(ref) & !is.na(model))
  if (length(valid) < 4) return(list(R = NA, sd.f = NA, crmsd = NA))
  
  ref = ref[valid]
  model = model[valid]
  
  R = cor(ref, model)
  sd.r = sd(ref)
  sd.f = sd(model)
  
  if (normalize) {
    sd.f = sd.f / sd.r
    sd.r = 1
  }
  
  crmsd = sqrt(sd.r^2 + sd.f^2 - 2 * sd.r * sd.f * R)
  
  return(list(sd.f = sd.f, R = R, crmsd = crmsd))
}

# ------------------------------
# Charger les données
model = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/regrid/hycom-3.1-southpacific-sst-monthly-199301-202112-regrid.nc")
model_data = ncvar_get(model, "sst")[1:684, 41:320, ]


oisst = nc_open("C:/Users/jdanielou/Desktop/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")
OISST_data = ncvar_get(oisst, "sst")[1:684, 41:320, 149:412]

lon = ncvar_get(oisst, 'lon')[1:684]
lat = ncvar_get(oisst, 'lat')[41:320]

nlon = length(lon)
nlat = length(lat)
npoints = nlon * nlat
nmonths = dim(model_data)[3]

# ------------------------------
# Indices par saison
get_season_indices = function(months, time_length) {
  months_seq = rep(1:12, times = ceiling(time_length / 12))[1:time_length]
  which(months_seq %in% months)
}

seasons = list(
  summer = get_season_indices(c(1,2,3), nmonths),
  autumn = get_season_indices(c(4,5,6), nmonths),
  winter = get_season_indices(c(7,8,9), nmonths),
  spring = get_season_indices(c(10,11,12), nmonths)
)

# ------------------------------
# Préparer structure de sortie
results = data.frame(
  lon = rep(NA, npoints),
  lat = rep(NA, npoints)
)

for (s in names(seasons)) {
  results[[paste0("sd_", s)]] = NA
  results[[paste0("R_", s)]] = NA
  results[[paste0("crmsd_", s)]] = NA
  results[[paste0("rmse_", s)]] = NA
  results[[paste0("bias_", s)]] = NA
}

# ------------------------------
# Initialisation boucle
row_id = 1
pb = txtProgressBar(min = 0, max = nlon * nlat, style = 3)


for (i in 1:nlon) {
  for (j in 1:nlat) {
    
    results$lon[row_id] = lon[i]
    results$lat[row_id] = lat[j]
    
    model_ts_full = model_data[i, j, ]
    obs_ts_full = OISST_data[i, j, ]
    
    for (s in names(seasons)) {
      idx = seasons[[s]]
      model_ts = model_ts_full[idx]
      obs_ts = obs_ts_full[idx]
      
      if (sum(!is.na(model_ts) & !is.na(obs_ts)) > 3) {
        
        metrics = fast_taylor_metrics(obs_ts, model_ts)
        
        bias = model_ts - obs_ts
        bias_mean = mean(bias, na.rm=TRUE)
        
        results[[paste0("sd_", s)]][row_id] = metrics$sd.f
        results[[paste0("R_", s)]][row_id] = metrics$R
        results[[paste0("crmsd_", s)]][row_id] = metrics$crmsd
        results[[paste0("rmse_", s)]][row_id] = sqrt(mean((obs_ts - model_ts)^2, na.rm = TRUE))
        results[[paste0("bias_", s)]][row_id] = bias_mean
      }
    }
    
    row_id = row_id + 1
    setTxtProgressBar(pb, row_id)
  }
}

close(pb)

# ------------------------------
# Export final
write.csv(results, "C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom_saisons.csv", row.names = FALSE)
