library(dplyr)
library(ggplot2)
library(maps)

world = map_data("world2")

glorys_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/glorys/taylor_metrics_pixel_glorys.csv")
bran_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/bran/taylor_metrics_pixel_bran.csv")
hycom_df = read.csv("C:/Users/jdanielou/Desktop/plot_internship/csv/metric_csv/hycom/taylor_metrics_pixel_hycom.csv")

df_all = glorys_df %>%
  select(lon, lat, R) %>%
  rename(R_GLORYS = R) %>%
  left_join(bran_df %>% select(lon, lat, R) %>% rename(R_BRAN = R),
            by = c("lon", "lat")) %>%
  left_join(hycom_df %>% select(lon, lat, R) %>% rename(R_HYCOM = R),
            by = c("lon", "lat"))


df_all_status = df_all %>%
  mutate(
    GLORYS = ifelse(is.na(R_GLORYS), "NA", "OK"),
    BRAN   = ifelse(is.na(R_BRAN), "NA", "OK"),
    HYCOM  = ifelse(is.na(R_HYCOM), "NA", "OK"),
    

    combinaison = paste(GLORYS, BRAN, HYCOM, sep = "-")
  ) %>%
  select(lon, lat, GLORYS, BRAN, HYCOM, combinaison)


head(df_all_status)
write.csv(df_all_status, "C:/Users/jdanielou/Desktop/comparaison_mask.csv", row.names = FALSE)


palette_comb = c(
  "OK-OK-OK" = "#1F77B4",
  "OK-OK-NA" = "#2CA02C",
  "OK-NA-OK" = "#FF7F0E",
  "OK-NA-NA" = "#9467BD",
  "NA-OK-OK" = "#E377C2",
  "NA-OK-NA" = "#17BECF",
  "NA-NA-OK" = "#FFD700",
  "NA-NA-NA" = "#4D4D4D"    
)

# Forcer l'ordre
df_all_status$combinaison = factor(df_all_status$combinaison, levels = names(palette_comb))

# Plot final
x11(width = 16, height = 12)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70") +
  
  geom_point(data = df_all_status, aes(x = lon, y = lat, color = combinaison),
             size = 1.5) +
  
  coord_fixed(xlim = c(128, 292), ylim = c(-57, 7)) +
  
  scale_color_manual(values = palette_comb, name = "G-B-H") +
  
  labs(title = "Disponibilité des données par combinaison de produits (Corrélation R)") +
  
  guides(color = guide_legend(override.aes = list(size = 5))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
