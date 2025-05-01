# -----------------------------------------
# package
library(dplyr)
library(ggplot2)
library(ClusterR)

# -----------------------------------------
# Data loading
results_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/")

results_scaled = results_df %>%
  mutate(
    sd_scaled = scale(sd),
    R_scaled = scale(R),
    crmsd_scaled = scale(crmsd)
  )

# Préparation pour le clustering

clust_data = results_scaled %>%
  select(sd_scaled, R_scaled, crmsd_scaled) %>%
  as.matrix()   

# Clustering rapide avec ClusterR

set.seed(123)  
k = 4          # nombre de clusters
clust = KMeans_rcpp(clust_data, clusters = k, num_init = 5, max_iters = 1000)

# Ajouter les clusters au dataframe
results_df$cluster = clust$clusters

# -----------------------------------------
# Save map

png("C:/Users/jdanielou/Desktop/plots_internship/plot/maps/clusters_map_glorys.png", width = 1600, height = 1200, res = 150)

ggplot(results_df, aes(x = lon, y = lat, color = as.factor(cluster))) +
  geom_point(size = 1) +
  coord_fixed() +
  scale_color_brewer(palette = "Set3") +
  labs(color = "Cluster", title = "Clusters des pixels basés sur les performances du modèle") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_blank()
  )

dev.off()

# -----------------------------------------
# Save dataframe

write.csv(results_df, "C:/Users/jdanielou/Desktop/plots_internship/csv/metric_csv/glorys/clusters_results.csv", row.names = FALSE)


