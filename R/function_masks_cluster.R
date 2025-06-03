df_all = readRDS(file = "C:/Users/jdanielou/Desktop/rds/df_all.rds")

df_all$cluster_final_reordered_corrected = df_all$cluster_final_reordered

# Réassignation conditionnelle pour les points du cluster 7
df_all$cluster_final_reordered_corrected[
  df_all$cluster_final_reordered == 7 &
    df_all$lat <= 10 & df_all$lat > -10 &
    df_all$lon >= 120 & df_all$lon <= 150
] = 6

df_all$cluster_final_reordered_corrected[
  df_all$cluster_final_reordered == 7 &
    df_all$lat <= -10 & df_all$lat > -20 &
    df_all$lon >= 120 & df_all$lon <= 150
] = 2

df_all$cluster_final_reordered_corrected[
  df_all$cluster_final_reordered == 7 &
    df_all$lat <= -30 & df_all$lat > -40 &
    df_all$lon >= 120 & df_all$lon <= 150
] = 4

df_all$cluster_final_reordered_corrected[
  df_all$cluster_final_reordered == 7 &
    df_all$lat <= 10 & df_all$lat > 0 &
    df_all$lon >= 250 & df_all$lon <= 300
] = 3


clusters = sort(unique(df_all$cluster_final_reordered_corrected))
saveRDS(df_all, file = "C:/Users/jdanielou/Desktop/df_all.rds")


masks = lapply(clusters, function(cl) df_all$cluster_final_reordered_corrected == cl)
names(masks) = paste0("mask_cluster-", clusters)
saveRDS(masks, file = "C:/Users/jdanielou/Desktop/rds/masks_cluster.rds")



