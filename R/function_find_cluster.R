library(cluster)
library(ClusterR)
library(dplyr)


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


clust_data = df_all %>%
  mutate(
    lon_scaled = scale(lon)
  ) %>%
  select(glorys_crmsd, glorys_R, glorys_sd,bran_crmsd, bran_R, bran_sd,hycom_crmsd, hycom_R, hycom_sd,lon_scaled) %>%
  as.matrix()

# Calculer l'inertie pour k = 1 à 10
wss = numeric(10)

for (k in 1:10) {
  set.seed(123)
  clust = KMeans_rcpp(clust_data, clusters = k, num_init = 5, max_iters = 1000)
  
  # Récupérer les centres
  centers = clust$centroids[clust$clusters, ]
  
  # Calculer les distances au centre
  distances = rowSums((clust_data - centers)^2)
  
  # Stocker la somme → WSS (Within Sum of Squares)
  wss[k] = sum(distances)
}

# Plot Elbow
x11(width = 12, height = 8)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Nombre de clusters (k)",
     ylab = "Inertie intra-cluster (tot.withinss)",
     main = "Méthode du coude pour choisir k")



# Echantillonner 
set.seed(123)
sample_index = sample(1:nrow(clust_data), 10000)

clust_sample = clust_data[sample_index, ]
clusters_sample = clust$clusters[sample_index]

# Calcul de la silhouette sur l'échantillon
sil = silhouette(clusters_sample, dist(clust_sample))

summary(sil)

