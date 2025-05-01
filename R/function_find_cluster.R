library(cluster)
library(ClusterR)
library(ggplot2)

# Préparer les données
results_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/")

clust_data = results_df %>%
  select(crmsd) %>%
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

