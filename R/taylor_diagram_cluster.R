# -----------------------------------------
# Packages
library(dplyr)
library(ggplot2)
library(viridis)
library(maps)
library(RColorBrewer)
source("R./function_taylor.R")
# -----------------------------------------
# Données
masks = readRDS("C:/Users/jdanielou/Desktop/rds/masks_cluster.rds")
df_all = readRDS("C:/Users/jdanielou/Desktop/rds/df_all_season.rds") %>%
  filter(complete.cases(.))


clusters = list(
  df_all[masks$mask_cluster1, ],
  df_all[masks$mask_cluster2, ],
  df_all[masks$mask_cluster3, ],
  df_all[masks$mask_cluster4, ],
  df_all[masks$mask_cluster5, ],
  df_all[masks$mask_cluster6, ]
)

# Paramètres
n_clust = 6
cluster_colors = colorful::divergencePalette(n = 8, col = c("dodgerblue3", "firebrick4"), intensity = 1)
length(cluster_colors) = n_clust
names(cluster_colors) = as.character(1:n_clust)

models = c("glorys", "bran", "hycom")
pch_set = c(16, 17, 18)
seasons = c("summer", "autumn", "winter", "spring")
# -----------------------------------------
# Taylor Diagram avec légende intégrée dans le subplot "autumn"
x11(width = 12, height = 12)
par(mfrow = c(2,2), oma = c(0,0,0,0), mar = c(5,5,3,3))  # Ajuste les marges si besoin

for (season in seasons) {
  taylor.diagram(ref = c(1, 1.1), model = c(1, 0.9), label = "Test", add = FALSE,
                 main = paste0("Taylor Diagram - ", season))
  
  for (j in seq_along(clusters)) {
    cluster = clusters[[j]]
    for (i in seq_along(models)) {
      model = models[i]
      
      R_col = cluster[[paste0(model, "_", season, "_R")]]
      sd_col = cluster[[paste0(model, "_", season, "_sd")]]
      
      R_mean = mean(R_col, na.rm = TRUE)
      sd_mean = mean(sd_col, na.rm = TRUE)
      
      x = sd_mean * R_mean
      y = sd_mean * sin(acos(R_mean))
      
      points(x, y, col = cluster_colors[as.character(j)], pch = pch_set[i], cex = 1.4)
    }
  }
  
  # Afficher les légendes seulement sur le plot "autumn"
  if (season == "autumn") {
    legend("topright", legend = c("GLORYS", "BRAN", "HYCOM"),
           col = "black", pch = pch_set, pt.cex = 2, cex = 1.2, inset = c(-0.19, -0.1), 
           x.intersp = 0.6, y.intersp = 0.6, bty = "n")
    
    legend("topright", legend = paste("Cluster", 1:n_clust),
           col = cluster_colors, pch = 15, pt.cex = 2, cex = 1.2, x.intersp = 0.6, y.intersp = 0.6, 
           inset = c(-0.19, 0.06), bty = "n")
  }
}

# Export
dev.copy(png, "C:/Users/jdanielou/Desktop/plots_internship/plot/article_plots/taylor/taylor_seasons.png", 
         units = "in", width = 12, height = 12, res = 150)
dev.off()




# -----------------------------------------
# Données
masks = readRDS("C:/Users/jdanielou/Desktop/masks_cluster.rds")
df_all = readRDS("C:/Users/jdanielou/Desktop/df_all.rds") %>%
  filter(complete.cases(.))


clusters = list(
  df_all[masks$mask_cluster1, ],
  df_all[masks$mask_cluster2, ],
  df_all[masks$mask_cluster3, ],
  df_all[masks$mask_cluster4, ],
  df_all[masks$mask_cluster5, ],
  df_all[masks$mask_cluster6, ]
)

# Paramètres
n_clust = 6
cluster_colors = colorful::divergencePalette(n = 8, col = c("dodgerblue3", "firebrick4"), intensity = 1)
length(cluster_colors) = n_clust
names(cluster_colors) = as.character(1:n_clust)

models = c("glorys", "bran", "hycom")
pch_set = c(16, 17, 18)

# -----------------------------------------
# Taylor Diagram
x11(width = 10, height = 10)
taylor.diagram(ref = c(1, 1.1), model = c(1, 0.9), label = "Test", add = FALSE)

for (j in seq_along(clusters)) {
  cluster = clusters[[j]]
  for (i in seq_along(models)) {
    model = models[i]
    
    R_col = cluster[[paste0(model, "_R")]]
    sd_col = cluster[[paste0(model, "_sd")]]
    
    R_mean = mean(R_col, na.rm = TRUE)
    sd_mean = mean(sd_col, na.rm = TRUE)
    
    x = sd_mean * R_mean
    y = sd_mean * sin(acos(R_mean))
    
    points(x, y, col = cluster_colors[as.character(j)], pch = pch_set[i], cex = 1.2)
  }
}

legend("topright", legend = c("GLORYS", "BRAN", "HYCOM"),
       col = "black", pch = pch_set, pt.cex = 2)

legend("topright", legend = paste("Cluster", 1:n_clust),
       col = cluster_colors, pch = 15, pt.cex = 2, title = "Cluster", inset = c(0, 0.12))








