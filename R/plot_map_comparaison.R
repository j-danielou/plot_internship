# -----------------------------------------
# Packages
library(dplyr)
library(ggplot2)
library(ClusterR)
library(viridis)
library(maps)
library(patchwork)
library(rlang)
library(gridExtra)
library(cowplot)
# -----------------------------------------
# Data
world = map_data("world2")

glorys_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

glorys_df = glorys_df |>
  rename(
    glorys_crmsd = crmsd,
    glorys_R = R,
    glorys_sd = sd,
    glorys_rmse = rmse,
    glorys_biais = biais
  )

bran_df = bran_df |>
  rename(
    bran_crmsd = crmsd,
    bran_R = R,
    bran_sd = sd,
    bran_rmse = rmse,
    bran_biais = biais
  )

hycom_df = hycom_df |>
  rename(
    hycom_crmsd = crmsd,
    hycom_R = R,
    hycom_sd = sd,
    hycom_rmse = rmse,
    hycom_biais = biais
  )

df_all = glorys_df |>
  left_join(bran_df, by = c("lon", "lat")) |>
  left_join(hycom_df, by = c("lon", "lat"))

# -----------------------------------------
# function
c2t = gts:::coord2text
cl = gts:::checkLongitude
# -----------------------------------------
# function
quantil_troncate = function(data, quantil, na.rm=TRUE){
  dat_glo = data
  dat_glo[dat_glo>quantile(data, quantil, na.rm = na.rm)] = quantile(data, quantil, na.rm = na.rm)
  return(dat_glo)
}
df_all$glorys_rmse_quantil = quantil_troncate(data = df_all$glorys_rmse, quantil = 0.99)
df_all$bran_rmse_quantil = quantil_troncate(data = df_all$bran_rmse, quantil = 0.99)
df_all$hycom_rmse_quantil = quantil_troncate(data = df_all$hycom_rmse, quantil = 0.99)
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
    
    ggtitle(title) +
    
    scale_fill_gradientn(colors = if (palette_option == "viridis") viridis::viridis(100) else if (palette_option == "magma") viridis::magma(5) else rev(RColorBrewer::brewer.pal(11, "RdBu")),
                         limits = limits,
                         name = legend_title,
                         guide = guide_colorbar(barwidth = 0.8, barheight = 12, title.position = "right", 
                                                  title.theme = element_text(angle = 90, hjust = 0.5, size = 12),
                                                label.theme = element_text(size = 17))) + 
    
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_blank(),
      axis.text.x = if (show_x) element_text(size = 15) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 15) else element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      legend.position = if (show_legend) "right" else "none"
    )
  
  return(p)
}

# ---------------------------------------
# Génération des plots (exemple avec df_all)
# Biais
p1 = plot_metric(df_all, "glorys_biais", limits = c(-2.2, 2.2), palette_option = "RdBu", title = "GLORYS", show_y = TRUE)
p2 = plot_metric(df_all, "bran_biais", limits = c(-2.2, 2.2), palette_option = "RdBu", title = "BRAN")
p3 = plot_metric(df_all, "hycom_biais", legend_title = "°C", limits = c(-2.2, 2.2), palette_option = "RdBu", title = "HYCOM", show_legend = TRUE)

# Corrélation
p4 = plot_metric(df_all, "glorys_R", limits = c(0.3, 1), palette_option = "viridis", show_y = TRUE)
p5 = plot_metric(df_all, "bran_R", limits = c(0.3, 1), palette_option = "viridis")
p6 = plot_metric(df_all, "hycom_R", legend_title = "Correlation (r)", limits = c(0.3, 1), palette_option = "viridis", show_legend = TRUE)

# RMSE
range = range(c(df_all$glorys_rmse_quantil, df_all$bran_rmse_quantil, df_all$hycom_rmse_quantil), na.rm = TRUE)
p7 = plot_metric(df_all, "glorys_rmse_quantil", limits = range, palette_option = "magma", show_y = TRUE, show_x = TRUE)
p8 = plot_metric(df_all, "bran_rmse_quantil", limits = range, palette_option = "magma", show_x = TRUE)
p9 = plot_metric(df_all, "hycom_rmse_quantil", legend_title = "RMSE", limits = range, palette_option = "magma", show_legend = TRUE, show_x = TRUE)

# ---------------------------------------
# Extraire les légendes
legend_biais = get_legend(p3)
legend_corr = get_legend(p6)
legend_rmse = get_legend(p9)

# Supprimer les légendes pour les plots
p3 = p3 + theme(legend.position = "none")
p6 = p6 + theme(legend.position = "none")
p9 = p9 + theme(legend.position = "none")

# Créer les lignes de graphiques
row1 = plot_grid(p1, p2, p3, legend_biais, ncol = 4, rel_widths = c(1.07, 1, 1, 0.13))
row2 = plot_grid(p4, p5, p6, legend_corr, ncol = 4, rel_widths = c(1.07, 1, 1, 0.13))
row3 = plot_grid(p7, p8, p9, legend_rmse, ncol = 4, rel_widths = c(1.07, 1, 1, 0.13))


final_plot = plot_grid(row1, row2, row3, ncol = 1, scale = 1)

# Affichage
x11(width = 20, height = 10)
print(final_plot)

save_plot("C:/Users/jdanielou/Desktop/comparaison_maps.png", final_plot, base_width = 21, base_height = 10, dpi = 150)




