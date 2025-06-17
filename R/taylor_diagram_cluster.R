
# --- Paramètre à modifier : soit "sst" soit "sss"
var_type = "sss"  # ou "sss"

# --- Packages ---
library(dplyr)
library(purrr)
library(ggplot2)
library(maps)
library(lubridate)
library(patchwork)
source("R./function_taylor.R")

# --- Configuration selon variable ---
obs_file     = ifelse(var_type == "sst",
                      "oisst_by_cluster_sst.rds",
                      "cmems_by_cluster_sss.rds")

models       = c("glorys", "bran", "hycom")
rds_suffix   = paste0("_by_cluster_", var_type, ".rds")
model_files  = paste0("C:/Users/jdanielou/Desktop/rds/", models, rds_suffix)
obs_path     = paste0("C:/Users/jdanielou/Desktop/rds/", obs_file)
save_path    = paste0("C:/Users/jdanielou/Desktop/rds/distance_", var_type, ".rds")
plot_path    = paste0("C:/Users/jdanielou/Desktop/taylor-diagram-", var_type, "-test.png")

# --- Chargement des fichiers ---
names(model_files) = models
model_clusters = lapply(model_files, readRDS)
obs_cluster = readRDS(obs_path)

# --- Harmonisation du nom de la colonne ("sst" ou "sss") ---
model_clusters = purrr::imap(model_clusters, function(model_data, model_name) {
  lapply(model_data, function(df) {
    dplyr::rename(df, !!paste0(var_type, "_", model_name) := .data[[var_type]])
  })
})

# --- Conversion des dates ---
for (i in 1:6) {
  obs_cluster[[i]]$time = as.Date(obs_cluster[[i]]$time)
}

start_hycom = as.Date("1994-01-16")
end_hycom   = as.Date("2015-12-16")

# --- 1. Jointures complètes (GLORYS, BRAN) ---
df_all = list()
for (i in seq_along(obs_cluster)) {
  cluster_name = names(obs_cluster)[i]
  df = obs_cluster[[cluster_name]]
  
  for (model in c("glorys", "bran")) {
    df = dplyr::left_join(df, model_clusters[[model]][[cluster_name]], by = c("lon", "lat", "time"))
  }
  
  df = df[complete.cases(df), ]
  df_all[[cluster_name]] = df
}

names(df_all) = names(obs_cluster)

# --- 2. Jointure restreinte HYCOM ---
df_all_hycom = list()
for (i in seq_along(obs_cluster)) {
  cluster_name = names(obs_cluster)[i]
  
  # Filtrer la période HYCOM
  df = obs_cluster[[cluster_name]] %>%
    filter(time >= start_hycom & time <= end_hycom)
  
  df = dplyr::left_join(df, model_clusters$hycom[[cluster_name]], by = c("lon", "lat", "time"))
  
  df = df[complete.cases(df), ]
  df_all_hycom[[cluster_name]] = df
}

names(df_all_hycom) = names(obs_cluster)

# --- Taylor Diagram par cluster ---
colors = colorful::divergencePalette(n = 8, col = c("dodgerblue3", "firebrick4"), intensity = 1)
length(colors) = 6
model_pch = c(GLORYS = 16, BRAN = 17, HYCOM = 15)
clusters = names(df_all)
results = list()
diagram_start = FALSE

x11(width = 12, height = 12)

for (i in seq_along(clusters)) {
  cluster_df = df_all[[i]]
  ref = cluster_df[[var_type]]
  cluster_col = colors[i]
  results[[paste0("cluster", i)]] = list()
  
  for (model in c("glorys", "bran")) {
    mod_data = cluster_df[[paste0(var_type, "_", model)]]
    model_name = toupper(model)
    
    result = taylor.diagram(ref, mod_data,
                            label = "",
                            add = diagram_start,
                            col = cluster_col,
                            pch = model_pch[[model_name]],
                            pcex = 1)
    results[[paste0("cluster", i)]][[model]] = result
    diagram_start = TRUE
  }
  
  cluster_df_hycom = df_all_hycom[[i]]
  ref_hycom = cluster_df_hycom[[var_type]]
  mod_data_hycom = cluster_df_hycom[[paste0(var_type, "_hycom")]]
  
  result = taylor.diagram(ref_hycom, mod_data_hycom,
                          label = "",
                          add = TRUE,
                          col = cluster_col,
                          pch = model_pch[["HYCOM"]],
                          pcex = 1)
  results[[paste0("cluster", i)]][["hycom"]] = result
}

# --- Légendes ---
legend("topright", 
       legend = c("GLORYS", "BRAN", "HYCOM"),
       col = "black", pch = model_pch,
       pt.cex = 1.2, cex = 1,
       title = "Modèles",
       inset = c(-0.06, -0.17), xpd = TRUE)

legend("topright", 
       legend = paste0("Cluster ", 1:6),
       col = colors, pch = 15,
       pt.cex = 1.2, cex = 1,
       title = "Clusters",
       inset = c(-0.06, 0.01), xpd = TRUE)

dev.copy(png, plot_path, width = 12, height = 12, units = "in", res = 150)
dev.off()

# --- Calcul des distances au point idéal ---
distance_df = data.frame()
for (clust in names(results)) {
  for (mod in names(results[[clust]])) {
    res = results[[clust]][[mod]]
    sd = res$sd
    crmsd = res$crmsd
    D = sqrt((sd - 1)^2 + crmsd^2)
    
    distance_df = rbind(distance_df, data.frame(
      cluster = clust,
      model = mod,
      sd = sd,
      crmsd = crmsd,
      D = D
    ))
  }
}

# --- Association distances → points spatiaux
distance_maps = list(
  bran = data.frame(), 
  glorys = data.frame(), 
  hycom = data.frame()
)

for (model_name in c("bran", "glorys", "hycom")) {
  for (i in 1:6) {
    cluster_name = paste0("cluster", i)
    df_points = unique(df_all[[i]][, c("lon", "lat")])
    
    D_val = distance_df %>%
      filter(cluster == cluster_name, model == model_name) %>%
      pull(D)
    
    df_points$cluster = i
    df_points$model = model_name
    df_points$D = D_val
    
    distance_maps[[model_name]] = rbind(distance_maps[[model_name]], df_points)
  }
}

# --- Sauvegarde
saveRDS(object = distance_maps, file = save_path)
