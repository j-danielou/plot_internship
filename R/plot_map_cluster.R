# -----------------------------------------
# package
library(dplyr)
library(ggplot2)
library(ClusterR)
library(viridis)
library(maps)
# -----------------------------------------
# Data loading
world = map_data("world2")

glorys_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

glorys_df = glorys_df %>%
  rename(
    glorys_crmsd = crmsd,
    glorys_R = R,
    glorys_sd = sd,
    glorys_rmse = rmse
  )

bran_df = bran_df %>%
  rename(
    bran_crmsd = crmsd,
    bran_R = R,
    bran_sd = sd,
    bran_rmse = rmse
  )

hycom_df = hycom_df %>%
  rename(
    hycom_crmsd = crmsd,
    hycom_R = R,
    hycom_sd = sd,
    hycom_rmse = rmse
  )

df_all = glorys_df %>%
  left_join(bran_df, by = c("lon", "lat")) %>%
  left_join(hycom_df, by = c("lon", "lat"))

df_all = df_all[complete.cases(df_all), ]
# Préparation pour le clustering
df_all = df_all %>%
  mutate(
    lon_scaled = (lon - min(lon)) / (max(lon) - min(lon)),
    lat_scaled = (lat - min(lat)) / (max(lat) - min(lat))
  )


clust_data = df_all %>%
  select(glorys_crmsd, glorys_R, glorys_sd, 
         bran_crmsd, bran_R, bran_sd, 
         hycom_crmsd, hycom_R, hycom_sd,
         lon_scaled,lat_scaled) %>%
  as.matrix()

clust_data_centroide = df_all %>%
  select(glorys_crmsd, glorys_R, glorys_sd, 
         bran_crmsd, bran_R, bran_sd, 
         hycom_crmsd, hycom_R, hycom_sd) %>%
  as.matrix()


# Clustering rapide avec ClusterR

set.seed(123)  
k = 11       # nombre de clusters
clust = KMeans_rcpp(clust_data, clusters = k, num_init = 5, max_iters = 1000)

# Ajouter les clusters au dataframe
df_all$cluster = clust$clusters

# -----------------------------------------
# Cluster Map
x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "grey70") +
  geom_point(data = df_all, aes(x = lon, y = lat, color = as.factor(cluster)),
             size = 1) +
  coord_fixed(xlim = c(128, 292),
              ylim = c(-57, 7)) +
  scale_color_brewer(palette = "Set3") +
  labs(color = "Cluster", title = "Clusters des pixels basés sur les performances du modèle") +
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_blank()
  )
dev.copy(png,"C:/Users/jdanielou/Desktop/plots_internship/plot/maps/cluster/hycom/clusters_map_hycom_full_k5.png", width = 1600, height = 1200, res = 150)
dev.off()


# Metric Map
x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "grey70") +
  geom_point(data = results_df, aes(x = lon, y = lat, color = R),
             size = 1) +
  coord_fixed(xlim = c(128, 292),
              ylim = c(-57, 7)) +
  scale_color_viridis(option = "viridis", name = "Corrélation R", limits = c(0.3, 1)) +
  labs(title = "Corrélation (R) entre modèle et observations") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_blank()
  )

dev.copy(png,"C:/Users/jdanielou/Desktop/plots_internship/plot/maps/cluster/hycom/correlation_map_hycom.png", width = 1600, height = 1200, res = 150)
dev.off()

# -----------------------------------------
# Save dataframe

write.csv(results_df, "C:/Users/jdanielou/Desktop/plots_internship/csv/metric_csv/glorys/clusters_results_norm_k4.csv", row.names = FALSE)


