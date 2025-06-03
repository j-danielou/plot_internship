library(dplyr)
library(ggplot2)
library(viridis)
library(maps)
library(patchwork)
library(rlang)
library(gridExtra)
library(cowplot)
source("R./function_taylor.R")
# -----------------------------------------
# Data
world = map_data("world2")

glorys_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

glorys_df = glorys_df %>%
  rename(
    glorys_crmsd = crmsd,
    glorys_R = R,
    glorys_sd = sd,
    glorys_rmse = rmse,
    glorys_bias = biais
  )

bran_df = bran_df %>%
  rename(
    bran_crmsd = crmsd,
    bran_R = R,
    bran_sd = sd,
    bran_rmse = rmse,
    bran_bias = biais
  )

hycom_df = hycom_df %>%
  rename(
    hycom_crmsd = crmsd,
    hycom_R = R,
    hycom_sd = sd,
    hycom_rmse = rmse,
    hycom_bias = biais
  )

df_all = glorys_df %>%
  left_join(bran_df, by = c("lon", "lat")) %>%
  left_join(hycom_df, by = c("lon", "lat"))

df_all_clean = df_all %>%
  filter(complete.cases(.))
# -----------------------------------------
# function
reorder_and_plot_clusters = function(df_all, clust1_path, clust2_path, prefix,
                                     show_x = FALSE, show_y = FALSE, show_legend = FALSE) {
  
  # Fond de carte
  world = map_data("world2")
  
  # Charger les clusters
  clust1 = readRDS(clust1_path)
  clust2 = readRDS(clust2_path)
  
  # Appliquer les clusters aux points
  df_all$cluster = clust1$clusters
  df_all$cluster_final = clust2$clusters[df_all$cluster]
  
  # Charger les centroïdes et calculer leur distance à l'idéal
  centroids_df = as.data.frame(clust2$centroids)[, 1:3]
  colnames(centroids_df) = paste0(prefix, c("_crmsd", "_R", "_sd"))
  centroids_df$cluster = 1:nrow(centroids_df)
  
  target = c(crmsd = 0, R = 1, sd = 1)
  centroids_df$distance_to_target = sqrt(
    (centroids_df[[1]] - target["crmsd"])^2 +
      (centroids_df[[2]] - target["R"])^2 +
      (centroids_df[[3]] - target["sd"])^2
  )
  
  # Créer un vecteur nommé pour associer chaque cluster à sa distance
  map_cluster_to_distance = setNames(centroids_df$distance_to_target, centroids_df$cluster)
  df_all$distance_to_target = map_cluster_to_distance[as.character(df_all$cluster_final)]
  
  # Vérification
  if (all(is.na(df_all$distance_to_target))) {
    stop("Aucune correspondance entre clusters et distances : vérifiez les index.")
  }
  
  # Centrer la palette sur la médiane
  dist_mean = mean(df_all$distance_to_target, na.rm = TRUE)
  message(dist_mean)
  
  # Fonctions de formatage des axes
  c2t = gts:::coord2text
  cl = gts:::checkLongitude
  
  # Générer le plot
  p = ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "grey80", color = "black", linewidth = 0.1, inherit.aes = FALSE) +
    geom_point(data = df_all, aes(x = lon, y = lat, color = distance_to_target), size = 0.4) +
    coord_fixed(xlim = range(df_all$lon, na.rm = TRUE),
                ylim = range(df_all$lat, na.rm = TRUE),
                expand = FALSE) +
    scale_color_gradientn(
      colours = colorRampPalette(c("dodgerblue3", "white", "firebrick4"))(1000),
      limits = c(0, 10),
      values = scales::rescale(c(0, 0.95, 10)),
      name = "Distance"
    )+
    scale_x_continuous(
      breaks = pretty(df_all$lon, n = 6),
      labels = c2t(cl(pretty(df_all$lon, n = 6)), "lon"),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = pretty(df_all$lat, n = 4),
      labels = c2t(pretty(df_all$lat, n = 4), "lat"),
      expand = c(0, 0)
    ) +
    theme_minimal(base_size = 12) +
    labs(title = "") +
    guides(color = guide_colorbar(
      barwidth = 0.8, barheight = 9,
      title.position = "right",
      title.theme = element_text(angle = 90, hjust = 0.5, size = 12),
      label.theme = element_text(size = 17)
    )) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.title = element_blank(),
      axis.text.x = if (show_x) element_text(size = 15) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 15) else element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      legend.position = if (show_legend) "right" else "none",
      plot.margin = margin(5, 5, 5, 5)
    )
  
  return(p)
}

# -----------------------------------------
# function
c2t = gts:::coord2text
cl = gts:::checkLongitude

# -----------------------------------------
# function
quantil_troncate = function(data, quantil, sign = ">", na.rm=TRUE){
  dat_glo = data
  if (sign ==">"){
    dat_glo[dat_glo>quantile(data, quantil, na.rm = na.rm)] = quantile(data, quantil, na.rm = na.rm)
  }else{
    dat_glo[dat_glo<quantile(data, quantil, na.rm = na.rm)] = quantile(data, quantil, na.rm = na.rm)
  }
  return(dat_glo)
}

df_all$glorys_rmse_quantil = quantil_troncate(data = df_all$glorys_rmse, quantil = 0.99)
df_all$bran_rmse_quantil = quantil_troncate(data = df_all$bran_rmse, quantil = 0.99)
df_all$hycom_rmse_quantil = quantil_troncate(data = df_all$hycom_rmse, quantil = 0.99)

df_all$glorys_bias_quantil = quantil_troncate(data = df_all$glorys_bias, quantil = 0.002, sign = "<")
df_all$bran_bias_quantil = quantil_troncate(data = df_all$bran_bias, quantil = 0.002, sign = "<")
df_all$hycom_bias_quantil = quantil_troncate(data = df_all$hycom_bias, quantil = 0.002, sign = "<")

df_all$glorys_bias_quantil = quantil_troncate(data = df_all$glorys_bias_quantil, quantil = 0.999)
df_all$bran_bias_quantil = quantil_troncate(data = df_all$bran_bias_quantil, quantil = 0.999)
df_all$hycom_bias_quantil = quantil_troncate(data = df_all$hycom_bias_quantil, quantil = 0.999)

# -----------------------------------------
# function
plot_metric = function(df, variable, title = "", legend_title = "", limits, palette_option = "viridis", show_x = FALSE, show_y = FALSE, show_legend = FALSE) {
  p = ggplot(df, aes(x = lon, y = lat, fill = !!sym(variable))) +
    geom_tile() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "grey80", color = "black", linewidth = 0.1, inherit.aes = FALSE) +
    coord_fixed(xlim = c(min(df$lon), max(df$lon)), ylim = c(min(df$lat), max(df$lat)), expand = FALSE) +
    scale_x_continuous(breaks = pretty(df_all$lon, n=6), labels = c2t(cl(pretty(df_all$lon, n=6)), "lon"), expand = c(0, 0))+
    scale_y_continuous(breaks = pretty(df_all$lat, n=4), labels = c2t(pretty(df_all$lat, n=4), "lat"), expand = c(0,0))+
    theme_minimal(base_size = 12) +
    theme(plot.margin = margin(5, 5, 5, 5))+
    ggtitle(title) +
    scale_fill_gradientn(colors = if (palette_option == "viridis") viridis::viridis(100) else if (palette_option == "magma") viridis::magma(5) else rev(RColorBrewer::brewer.pal(11, "RdBu")),
                         limits = limits,
                         name = legend_title,
                         guide = guide_colorbar(barwidth = 0.8, barheight = 9, title.position = "right",
                                                title.theme = element_text(angle = 90, hjust = 0.5, size = 12),
                                                label.theme = element_text(size = 16))) +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = -0.2, size = 18),
      axis.title = element_blank(),
      axis.text.x = if (show_x) element_text(size = 15) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 15) else element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      legend.position = if (show_legend) "right" else "none",
      plot.margin = margin(0, -3, 0, -3)
    )
  return(p)
}

# ---------------------------------------
# Biais
max_abs = max(abs(c(df_all$glorys_bias_quantil, df_all$bran_bias_quantil, df_all$hycom_bias_quantil)), na.rm = TRUE)
p1 = plot_metric(df_all, "glorys_bias_quantil", limits =  c(-max_abs, max_abs), palette_option = "RdBu", title = "GLORYS", show_y = TRUE)
p2 = plot_metric(df_all, "bran_bias_quantil", limits =  c(-max_abs, max_abs), palette_option = "RdBu", title = "BRAN")
p3 = plot_metric(df_all, "hycom_bias_quantil", legend_title = "bias (°C)", limits = c(-max_abs, max_abs), palette_option = "RdBu", title = "HYCOM", show_legend = TRUE)

# Corrélation
range = range(c(df_all$glorys_R, df_all$bran_R, df_all$hycom_R), na.rm = TRUE)
p4 = plot_metric(df_all, "glorys_R", limits = range, palette_option = "viridis", show_y = TRUE)
p5 = plot_metric(df_all, "bran_R", limits = range, palette_option = "viridis")
p6 = plot_metric(df_all, "hycom_R", legend_title = "Correlation", limits = range, palette_option = "viridis", show_legend = TRUE)

# RMSE
range = range(c(df_all$glorys_rmse_quantil, df_all$bran_rmse_quantil, df_all$hycom_rmse_quantil), na.rm = TRUE)
p7 = plot_metric(df_all, "glorys_rmse_quantil", limits = range, palette_option = "magma", show_y = TRUE)
p8 = plot_metric(df_all, "bran_rmse_quantil", limits = range, palette_option = "magma")
p9 = plot_metric(df_all, "hycom_rmse_quantil", legend_title = "RMSE", limits = range, palette_option = "magma", show_legend = TRUE)

#Cluster

p10 = reorder_and_plot_clusters(df_all = df_all_clean, clust1_path = "C:/Users/jdanielou/Desktop/clust_centroid_glorys.rds",
                                clust2_path = "C:/Users/jdanielou/Desktop/clust_centroid_2_glorys.rds", prefix = "glorys", show_y = TRUE, show_x = TRUE)
p11 = reorder_and_plot_clusters(df_all = df_all_clean, clust1_path = "C:/Users/jdanielou/Desktop/clust_centroid_bran.rds",
                                clust2_path = "C:/Users/jdanielou/Desktop/clust_centroid_2_bran.rds", prefix = "bran", show_x = TRUE)
p12 = reorder_and_plot_clusters(df_all = df_all_clean, clust1_path = "C:/Users/jdanielou/Desktop/clust_centroid_hycom.rds",
                                clust2_path = "C:/Users/jdanielou/Desktop/clust_centroid_2_hycom.rds", prefix = "hycom", show_legend = TRUE, show_x = TRUE)

# ---------------------------------------
# Extraire les légendes
legend_biais = get_legend(p3)
legend_corr = get_legend(p6)
legend_rmse = get_legend(p9)
legend_cluster = get_legend(p12)

# Supprimer les légendes pour les plots
p3 = p3 + theme(legend.position = "none")
p6 = p6 + theme(legend.position = "none")
p9 = p9 + theme(legend.position = "none")
p12 = p12 + theme(legend.position = "none")

# Même proportions pour toutes les lignes
standard_widths = c(1.3, 1.25, 1.25, 0.20)

# Lignes
row1 = plot_grid(p1, p2, p3, legend_biais, ncol = 4, rel_widths = standard_widths, align = "h", axis = "tb")
row2 = plot_grid(p4, p5, p6, legend_corr, ncol = 4, rel_widths = standard_widths, align = "h", axis = "tb")
row3 = plot_grid(p7, p8, p9, legend_rmse, ncol = 4, rel_widths = standard_widths, align = "h", axis = "tb")
row4 = plot_grid(p10, p11, p12, legend_cluster, ncol = 4, rel_widths = standard_widths, align = "h", axis = "tb")

# Plot final
final_plot = plot_grid(row1, row2, row3, row4, ncol = 1, align = "v", axis = "lr", rel_heights = c(1.5, 1.5, 1.5, 1.7))
save_plot("C:/Users/jdanielou/Desktop/comparaison_maps_sst_cluster-test-v2.png", final_plot, base_width = 20, base_height = 11, dpi = 150)

