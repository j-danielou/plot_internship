# -----------------------------------------
# package
library(dplyr)
library(ggplot2)
library(ClusterR)
library(viridis)
library(maps)
library(RColorBrewer)
# --------------------------------------
# Lecture des fichiers CSV
glorys_sss_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_sss_glorys_cmems_025.csv")
bran_sss_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_sss_bran_cmems_025.csv")
hycom_sss_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_sss_hycom_cmems_025.csv")

glorys_sst_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_sst_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_sst_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

# Renommage des colonnes
glorys_sss_df = glorys_sss_df %>%
  rename(glorys_crmsd_sss = crmsd, glorys_R_sss = R, glorys_sd_sss = sd, glorys_rmse_sss = rmse, glorys_biais_sss = biais)

bran_sss_df = bran_sss_df %>%
  rename(bran_crmsd_sss = crmsd, bran_R_sss = R, bran_sd_sss = sd, bran_rmse_sss = rmse, bran_biais_sss = biais)

hycom_sss_df = hycom_sss_df %>%
  rename(hycom_crmsd_sss = crmsd, hycom_R_sss = R, hycom_sd_sss = sd, hycom_rmse_sss = rmse, hycom_biais_sss = biais)

glorys_sst_df = glorys_sst_df %>%
  rename(glorys_crmsd_sst = crmsd, glorys_R_sst = R, glorys_sd_sst = sd, glorys_rmse_sst = rmse, glorys_biais_sst = biais)

bran_sst_df = bran_sst_df %>%
  rename(bran_crmsd_sst = crmsd, bran_R_sst = R, bran_sd_sst = sd, bran_rmse_sst = rmse, bran_biais_sst = biais)

hycom_sst_df = hycom_sst_df %>%
  rename(hycom_crmsd_sst = crmsd, hycom_R_sst = R, hycom_sd_sst = sd, hycom_rmse_sst = rmse, hycom_biais_sst = biais)

# Merge des données
glorys_df = glorys_sss_df %>%
  left_join(glorys_sst_df, by = c("lon", "lat"))

bran_df = bran_sss_df %>%
  left_join(bran_sst_df, by = c("lon", "lat"))

hycom_df = hycom_sss_df %>%
  left_join(hycom_sst_df, by = c("lon", "lat"))

df_all = glorys_df %>%
  left_join(bran_df, by = c("lon", "lat")) %>%
  left_join(hycom_df, by = c("lon", "lat")) %>%
  filter(complete.cases(.)) %>%
  mutate(
    lon_scaled = (lon - min(lon)) / (max(lon) - min(lon)),
    lat_scaled = (lat - min(lat)) / (max(lat) - min(lat))) %>%
  select(lon, lat, lon_scaled, lat_scaled)

rm(list = setdiff(ls(), "df_all"))

models = c("glorys", "bran", "hycom")
variables = c("sst", "sss")

for (model in models){
  for (var in variables){
    cluster_result = readRDS(paste0("C:/Users/jdanielou/Desktop/clust_centroid_combined_", var, "_", model, ".rds"))
    clust_mod_var = paste0("cluster_", model, "_", var)
    df_all[[clust_mod_var]] = cluster_result$clusters
  }
}
rm(list = c("var", "model"))

# -----------------------------
# Paramètres
save_path = "C:/Users/jdanielou/Desktop/"
common_columns = c("crmsd", "R", "sd", "lon_scaled", "lat_scaled")

# -----------------------------
# Liste pour stocker les résultats
centroids_list = list()

# -----------------------------
# Boucle
for (model in models) {
  for (var in variables) {
    
    # Générer le nom du fichier
    file_path = paste0(save_path, "clust_centroid_combined_", var, "_", model, ".rds")
    
    # Lire le fichier RDS
    clust_result = readRDS(file_path)
    
    # Extraire les centroïdes et les transformer en data frame
    centroids_df = as.data.frame(clust_result$centroids)
    
    # Renommer les colonnes
    colnames(centroids_df) = common_columns
    
    # Stocker dans la liste avec un nom explicite
    list_name = paste0(model, "_", var)
    centroids_list[[list_name]] = centroids_df
    
    cat("Centroides chargés pour :", list_name, "\n")
  }
}
rm(list = setdiff(ls(), c("df_all", "centroids_list", "variables", "models")))


# -----------------------------
# Fonction d'attribution des centroïdes
# -----------------------------
attribuer_centroides = function(df, cluster_col, centroids_df, prefix) {
  # Vérifie que les indices des clusters sont valides
  stopifnot(all(df[[cluster_col]] >= 1 & df[[cluster_col]] <= nrow(centroids_df)))
  
  # Pour chaque métrique, attribuer la valeur du centroïde correspondant
  df[[paste0(prefix, "_crmsd")]] = centroids_df$crmsd[df[[cluster_col]]]
  df[[paste0(prefix, "_R")]]     = centroids_df$R[df[[cluster_col]]]
  df[[paste0(prefix, "_sd")]]    = centroids_df$sd[df[[cluster_col]]]
  df[[paste0(prefix, "_lon_scaled")]]    = centroids_df$lon_scaled[df[[cluster_col]]]
  df[[paste0(prefix, "_lat_scaled")]]    = centroids_df$lat_scaled[df[[cluster_col]]]
  
  return(df)
}


# Appliquer à df_all
df_all = attribuer_centroides(df_all, "cluster_glorys_sst", centroids_list$glorys_sst, "sst_glorys")
df_all = attribuer_centroides(df_all, "cluster_glorys_sss", centroids_list$glorys_sss, "sss_glorys")

# Répéter pour les autres modèles
df_all = attribuer_centroides(df_all, "cluster_bran_sst", centroids_list$bran_sst, "sst_bran")
df_all = attribuer_centroides(df_all, "cluster_bran_sss", centroids_list$bran_sss, "sss_bran")

df_all = attribuer_centroides(df_all, "cluster_hycom_sst", centroids_list$hycom_sst, "sst_hycom")
df_all = attribuer_centroides(df_all, "cluster_hycom_sss", centroids_list$hycom_sss, "sss_hycom")


data_list_combined = list(
  glorys = df_all %>% select(sst_glorys_crmsd, sst_glorys_R, sst_glorys_sd, sss_glorys_crmsd, sss_glorys_R, sss_glorys_sd, lon_scaled, lat_scaled) %>% as.matrix(),
  bran = df_all %>% select(sst_bran_crmsd, sst_bran_R, sst_bran_sd, sss_bran_crmsd, sss_bran_R, sss_bran_sd, lon_scaled, lat_scaled) %>% as.matrix(),
  hycom = df_all %>% select(sst_hycom_crmsd, sst_hycom_R, sst_hycom_sd, sss_hycom_crmsd, sss_hycom_R, sss_hycom_sd, lon_scaled, lat_scaled) %>% as.matrix()
)


# --------------------------------------
# Fonction de clustering et sauvegarde
cluster_and_save = function(data_list, name_out, k = 1000, num_init = 5, max_iters = 300, save_path = "C:/Users/jdanielou/Desktop/") {
  
  set.seed(123)
  
  for (name in names(data_list)) {
    
    cat("Clustering en cours pour:", name, "\n")
    
    # S'assurer que les données sont une matrice
    data_mat = data_list[[name]]
    n_samples = nrow(data_mat)
    
    cat(" -> Utilisation de KMeans_rcpp (", n_samples, "échantillons)\n")
    
    # Clustering
    result = KMeans_rcpp(
      data_mat,
      clusters = k,
      num_init = num_init,
      max_iters = max_iters
    )
    
    # Nom du fichier de sortie
    file_name = paste0(save_path, name_out, name, ".rds")
    
    # Sauvegarde
    saveRDS(result, file = file_name)
    
    cat(" -> Sauvegardé dans :", file_name, "\n\n")
  }
}

cluster_and_save(data_list_combined, name_out = "clust_centroid_combined_2_v2_", k=8)


#---------------------
#Function
reorder_and_plot_clusters = function(df_all, clust1_path, prefix, colors = NULL) {
  library(dplyr)
  library(ggplot2)
  source("R./function_taylor.R")
  # Charger les clusters
  clust1 = readRDS(clust1_path)

  # Appliquer le 1er clustering
  df_all$cluster = clust1$clusters
  
  # Charger les centroïdes
  centroids_df = as.data.frame(clust1$centroids)[, 1:6]
  colnames(centroids_df) = paste0(prefix, c("_crmsd_sst", "_R_sst", "_sd_sst", "_crmsd_sss", "_R_sss", "_sd_sss"))
  centroids_df$cluster = 1:nrow(centroids_df)
  
  # Distance à l'idéal
  target = c(crmsd = 0, R = 1, sd = 1)
  centroids_df$distance_to_target = sqrt(
    (centroids_df[[1]] - target["crmsd"])^2 +
      (centroids_df[[2]] - target["R"])^2 +
      (centroids_df[[3]] - target["sd"])^2+
      (centroids_df[[4]] - target["crmsd"])^2 +
      (centroids_df[[5]] - target["R"])^2 +
      (centroids_df[[6]] - target["sd"])^2
  )

  # Réordonner
  centroids_df = centroids_df |>
    arrange(distance_to_target) |>
    mutate(new_cluster = row_number())
  
  # Recode
  recode_vector = centroids_df$new_cluster
  names(recode_vector) = centroids_df$cluster
  df_all$cluster_final_reordered = recode_vector[as.character(df_all$cluster)]
  
  # Couleurs
  n_clust = nrow(centroids_df)
  names(colors) = as.character(1:n_clust)

  # Carte
  c2t = gts:::coord2text
  cl = gts:::checkLongitude
  
  x11(width = 16, height = 8)
  p = ggplot() +
    geom_polygon(data = map_data("world2"), aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "grey70") +
    geom_point(data = df_all, aes(x = lon, y = lat, color = as.factor(cluster_final_reordered)),
               size = 1) +
    coord_fixed(xlim = range(df_all$lon), ylim = range(df_all$lat), expand = FALSE) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = pretty(df_all$lon, n=6), labels = c2t(cl(pretty(df_all$lon, n=6)), "lon"), expand = c(0, 0))+
    scale_y_continuous(breaks = pretty(df_all$lat, n=4), labels = c2t(pretty(df_all$lat, n=4), "lat"), expand = c(0,0))+
    labs(color = "Cluster", title = "") +
    guides(color = guide_legend(override.aes = list(size = 10), label.theme = element_text(size = 15),
                                title.theme = element_text(size = 15))) + 
    theme_minimal() +
    theme(plot.title = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15))
  
  print(p)
  dev.copy(png, file = paste0("C:/Users/jdanielou/Desktop/map-cluster-combined-v2-k8-",prefix,".png"), width = 16, height = 7, units = "in", res = 200)
  dev.off() 
  #Sys.sleep(1)
  
  # Retourne les résultats utiles si besoin
  return(list(df = df_all, centroids = centroids_df, colors = colors, clust1 = clust1))
}

col = c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2", "#7f7f7f", "#17becf")
length(col) = 8

result = reorder_and_plot_clusters(
  df_all = df_all,
  clust1_path = "C:/Users/jdanielou/Desktop/clust_centroid_combined_2_v2_glorys.rds",
  prefix = "glorys",
  colors =  col
)




