# --- Packages ---
library(dplyr)
library(purrr)
library(lubridate)
source("R./function_taylor.R")

# --- Chemins des fichiers RDS ---
models = c("glorys", "bran", "hycom")
model_files = paste0("C:/Users/jdanielou/Desktop/rds/", models, "_by_cluster_test.rds")
names(model_files) = models
model_clusters = lapply(model_files, readRDS)

# --- Renommer la colonne 'sss' pour chaque modèle ---
model_clusters = purrr::imap(model_clusters, function(model_data, model_name) {
  lapply(model_data, function(df) {
    dplyr::rename(df, !!paste0("sst_", model_name) := sst)
  })
})

# --- Charger CMEMS et filtrer pour HYCOM (période 1994-01-16 à 2015-12-16) ---
cmems_cluster = readRDS("C:/Users/jdanielou/Desktop/rds/oisst_by_cluster_test.rds")

# conversion format Date si besoin
for (i in 1:6) {
  cmems_cluster[[i]]$time = as.Date(cmems_cluster[[i]]$time)
}

start_hycom = as.Date("1994-01-16")
end_hycom = as.Date("2015-12-16")

# --- 1. Données complètes pour CMEMS, BRAN, GLORYS uniquement ---
df_all = map(names(cmems_cluster), function(cluster_name) {
  df = cmems_cluster[[cluster_name]]
  
  for (model in c("glorys", "bran")) {
    df = left_join(df, model_clusters[[model]][[cluster_name]], by = c("lon", "lat", "time"))
  }
  
  df = df[complete.cases(df), ]
  return(df)
})
names(df_all) = names(cmems_cluster)

# --- 2. Données tronquées pour comparaison avec HYCOM (CMEMS + HYCOM) ---
df_all_hycom = map(names(cmems_cluster), function(cluster_name) {
  df = cmems_cluster[[cluster_name]] %>%
    filter(time >= start_hycom & time <= end_hycom)
  
  df = left_join(df, model_clusters$hycom[[cluster_name]], by = c("lon", "lat", "time"))
  df = df[complete.cases(df), ]
  return(df)
})
names(df_all_hycom) = names(cmems_cluster)

# --- Affichage Taylor Diagram global par cluster avec HYCOM géré à part ---
colors = c("red", "blue", "forestgreen", "darkorange", "purple", "black")
clusters = names(df_all)
model_colors = c(GLORYS = "black", BRAN = "red", HYCOM = "darkorange")
model_pch = c(GLORYS = 16, BRAN = 17, HYCOM = 15)

diagram_start = FALSE

x11(width = 12, height = 12)

for (i in seq_along(clusters)) {
  # CMEMS complet pour BRAN/GLORYS
  cluster_df = df_all[[i]]
  ref = cluster_df$sst
  cluster_col = colors[i]
  
  # GLORYS & BRAN
  for (model in c("glorys", "bran")) {
    mod_data = cluster_df[[paste0("sst_", model)]]
    model_name = toupper(model)
    
    taylor.diagram(ref, mod_data,
                   label = "",
                   add = diagram_start,
                   col = cluster_col,
                   pch = model_pch[[model_name]])
    diagram_start = TRUE
  }
  
  # HYCOM : CMEMS tronqué à sa période
  cluster_df_hycom = df_all_hycom[[i]]
  ref_hycom = cluster_df_hycom$sst
  mod_data_hycom = cluster_df_hycom$sst_hycom
  
  taylor.diagram(ref_hycom, mod_data_hycom,
                 label = "",
                 add = TRUE,
                 col = cluster_col,
                 pch = model_pch[["HYCOM"]])
}

dev.copy(png, "C:/Users/jdanielou/Desktop/taylor-diagram-sst-v2.png", width = 10, height = 10, units = "in", res = 150)
dev.off()
