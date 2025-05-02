# -----------------------------------------
# package
library(dplyr)
library(ggplot2)
library(ClusterR)
library(viridis)
library(maps)
# -----------------------------------------
# Data loading
results_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

world <- map_data("world2")

# Préparation pour le clustering

clust_data = results_df %>%
  select(crmsd) %>%
  as.matrix() 

# Clustering rapide avec ClusterR

set.seed(123)  
k = 4       # nombre de clusters
clust = KMeans_rcpp(clust_data, clusters = k, num_init = 5, max_iters = 1000)

# Ajouter les clusters au dataframe
results_df$cluster = clust$clusters

# -----------------------------------------
# Cluster Map
x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "grey70") +
  geom_point(data = results_df, aes(x = lon, y = lat, color = as.factor(cluster)),
             size = 1) +
  coord_fixed(xlim = c(128, 292),
              ylim = c(-67, 7)) +
  scale_color_brewer(palette = "Set3") +
  labs(color = "Cluster", title = "Clusters des pixels basés sur les performances du modèle") +
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_blank()
  )
dev.copy(png,"C:/Users/jdanielou/Desktop/plots_internship/plot/maps/cluster/hycom/clusters_map_hycom_crmsd_k4.png", width = 1600, height = 1200, res = 150)
dev.off()


# Metric Map
x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "grey70") +
  geom_point(data = results_df, aes(x = lon, y = lat, color = R),
             size = 1) +
  coord_fixed(xlim = c(128, 292),
              ylim = c(-67, 7)) +
  scale_color_viridis(option = "viridis", name = "Corrélation R", limits = c(-0.4, 1), breaks = seq(-0.4, 1, by = 0.35)) +
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


