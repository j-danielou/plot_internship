# -----------------------------------------
# package
library(dplyr)
library(ggplot2)
library(ClusterR)
library(viridis)
library(maps)
library(RColorBrewer)

# Charger la fonction Taylor
source("R./function_taylor.R")
# -----------------------------------------
# Data loading
world = map_data("world2")

# --------------------------------------
# Lecture des fichiers CSV
glorys_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

# Renommage des colonnes
glorys_df = glorys_df |>
  rename(glorys_crmsd = crmsd, glorys_R = R, glorys_sd = sd, glorys_rmse = rmse, glorys_biais = biais)

bran_df = bran_df |>
  rename(bran_crmsd = crmsd, bran_R = R, bran_sd = sd, bran_rmse = rmse, bran_biais = biais)

hycom_df = hycom_df |>
  rename(hycom_crmsd = crmsd, hycom_R = R, hycom_sd = sd, hycom_rmse = rmse, hycom_biais = biais)

# Merge des données
df_all = glorys_df |>
  left_join(bran_df, by = c("lon", "lat")) |>
  left_join(hycom_df, by = c("lon", "lat")) |>
  filter(complete.cases(.)) |>
  mutate(
    lon_scaled = (lon - min(lon)) / (max(lon) - min(lon)),
    lat_scaled = (lat - min(lat)) / (max(lat) - min(lat))
  )

# --------------------------------------
# Préparation des datasets pour clustering

data_list = list(
  full = df_all |> select(glorys_crmsd, glorys_R, glorys_sd, bran_crmsd, bran_R, bran_sd, hycom_crmsd, hycom_R, hycom_sd, lon_scaled, lat_scaled) |> as.matrix(),
  glorys = df_all |> select(glorys_crmsd, glorys_R, glorys_sd, lon_scaled, lat_scaled) |> as.matrix(),
  bran = df_all |> select(bran_crmsd, bran_R, bran_sd, lon_scaled, lat_scaled) |> as.matrix(),
  hycom = df_all |> select(hycom_crmsd, hycom_R, hycom_sd, lon_scaled, lat_scaled) |> as.matrix()
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

# Lancer le clustering et sauvegarde
cluster_and_save(data_list, name_out = "clust_centroid_")


# -----------------------------
# Paramètres
save_path = "C:/Users/jdanielou/Desktop/"
cluster_files = list(
  full = "clust_centroid_full.rds",
  glorys = "clust_centroid_glorys.rds",
  bran = "clust_centroid_bran.rds",
  hycom = "clust_centroid_hycom.rds"
)

# -----------------------------
# Colonnes selon le dataset
columns_list = list(
  full = c("glorys_crmsd", "glorys_R", "glorys_sd",
           "bran_crmsd", "bran_R", "bran_sd",
           "hycom_crmsd", "hycom_R", "hycom_sd",
           "lon_scaled", "lat_scaled"),
  
  glorys = c("glorys_crmsd", "glorys_R", "glorys_sd",
             "lon_scaled", "lat_scaled"),
  
  bran = c("bran_crmsd", "bran_R", "bran_sd",
           "lon_scaled", "lat_scaled"),
  
  hycom = c("hycom_crmsd", "hycom_R", "hycom_sd",
            "lon_scaled", "lat_scaled")
)

# -----------------------------
# Liste pour stocker les résultats
centroids_list = list()

# -----------------------------
# Boucle
for (name in names(cluster_files)) {
  
  # Lire le fichier RDS
  clust_result = readRDS(paste0(save_path, cluster_files[[name]]))
  
  # Extraire les centroïdes
  centroids_df = as.data.frame(clust_result$centroids)
  
  # Renommer les colonnes
  colnames(centroids_df) = columns_list[[name]]
  
  # Stocker dans la liste
  centroids_list[[name]] = centroids_df
  
  cat("Centroids récupérés pour:", name, "\n")
}

# --------------------------------------
# Préparation des datasets pour clustering 2 

data_list_centroid = list(
  full = centroids_list$full |> select(glorys_crmsd, glorys_R, glorys_sd, bran_crmsd, bran_R, bran_sd, hycom_crmsd, hycom_R, hycom_sd, lon_scaled, lat_scaled) |> as.matrix(),
  glorys = centroids_list$glorys |> select(glorys_crmsd, glorys_R, glorys_sd, lon_scaled, lat_scaled) |> as.matrix(),
  bran = centroids_list$bran |> select(bran_crmsd, bran_R, bran_sd, lon_scaled, lat_scaled) |> as.matrix(),
  hycom = centroids_list$hycom |> select(hycom_crmsd, hycom_R, hycom_sd, lon_scaled, lat_scaled) |> as.matrix()
)

# Lancer le clustering et sauvegarde
cluster_and_save(data_list_centroid, name_out = "clust_centroid_2_", k=7)


#######################################
##############  FULL  #################
#######################################
clust_centroid_2_full = readRDS("C:/Users/jdanielou/Desktop/clust_centroid_2_full.rds")

#Dataframe des centroide 
centroids_df_2 = as.data.frame(clust_centroid_2_full$centroids)
centroids_df_2 = centroids_df_2[, 1:9]
colnames(centroids_df_2) = c("glorys_crmsd", "glorys_R", "glorys_sd",
                           "bran_crmsd", "bran_R", "bran_sd",
                           "hycom_crmsd", "hycom_R", "hycom_sd")
centroids_df_2$cluster = 1:nrow(centroids_df_2)
centroids_df_2 = centroids_df_2 |>
  select(cluster, everything())




x11(width = 10, height = 10)
taylor.diagram(ref = c(1, 1.1), model = c(1, 0.9), label = "Test", add = FALSE)

n_clust = nrow(clust_centroid_2_full$centroids)
cluster_colors = brewer.pal(n = n_clust, name = "Set3")
names(cluster_colors) = as.character(1:n_clust)
pch_set = c(16, 17, 18)             

# Boucle sur les 3 produits
for (i in 1:3) {
  
  # Sélectionner les colonnes appropriées
  crmsd_col = centroids_df_2[[ (i-1)*3 + 2 ]]
  R_col = centroids_df_2[[ (i-1)*3 + 3 ]]
  sd_col = centroids_df_2[[ (i-1)*3 + 4 ]]
  
  # Ajouter les points pour chaque cluster
  for (j in 1:n_clust) {
    
    # Ajouter point
    points(sd_col[j] * R_col[j], sd_col[j] * sin(acos(R_col[j])), col = cluster_colors[as.character(j)], pch = pch_set[i], cex = 1)
    
  }
}

legend("topright", legend = c("GLORYS", "BRAN", "HYCOM"),
       col = "black", pch = pch_set, pt.cex = 2)

legend("bottomright", legend = paste("Cluster", 1:n_clust),
       col = cluster_colors, pch = 15, pt.cex = 2, title = "Cluster")



clust_result_full = readRDS("C:/Users/jdanielou/Desktop/clust_centroid_full.rds")

df_all$cluster = clust_result_full$clusters

df_all$cluster_final = clust_centroid_2_full$clusters[df_all$cluster]

x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "grey70") +
  geom_point(data = df_all, aes(x = lon, y = lat, color = as.factor(cluster_final)),
             size = 1) +
  coord_fixed(xlim = c(min(df_all$lon), max(df_all$lon)), ylim = c(min(df_all$lat), max(df_all$lat)), expand = FALSE) +
  scale_color_manual(values = cluster_colors) +
  labs(color = "Cluster", title = "Clusters des pixels basés sur les performances du modèle") +
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_blank()
  )
