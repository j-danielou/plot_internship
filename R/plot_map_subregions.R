library(ncdf4)
library(ggplot2)
library(viridis)
library(dplyr)
library(maps)


# Lecture des données netCDF
obs = nc_open("Y:/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")
sst = ncvar_get(obs, "sst")
lon = ncvar_get(obs, "lon")
lat = ncvar_get(obs, "lat")
nc_close(obs)

map_data = map_data("world2")

# Mise en forme pour ggplot
grid = expand.grid(lon = lon, lat = lat)
grid$sst = as.vector(sst[,,1])

# Data pour les segments (borders noirs)
segments_data = data.frame(
  x = c(120, 120, 120, 120, 180, 240, 300),
  xend = c(120, 300, 300, 300, 180, 240, 300),
  y = c(-60, -27.5, 5, -60, 5, 5, 5),
  yend = c(5, -27.5, 5, -60, -60, -60, -60)
)

# Data pour les labels
labels_data = data.frame(
  lon = c(165, 210, 255, 161, 210, 263),
  lat = c(0, 0, 0, -35, -35, -35),
  label = c("N-W", "N-C", "N-E", "S-W", "S-C", "S-E")
)
x11(width = 14, height = 12)
ggplot(grid, aes(x = lon, y = lat, fill = sst)) +
  geom_raster(na.rm = FALSE) +
  scale_fill_viridis(option = "inferno", na.value = "grey80", guide = "none", begin = 0.15) +
  geom_segment(data = segments_data, aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE, color = "black", size = 1.2) +
  geom_text(data = labels_data, aes(x = lon, y = lat, label = label),
            inherit.aes = FALSE, size = 6, fontface = "bold") +
  geom_path(data = map_data, aes(x = long, y = lat, group = group),
            color = "grey30", size = 0.3, inherit.aes = FALSE)+
  coord_fixed(xlim = c(min(lon), max(lon)), ylim = c(min(lat), max(lat)), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),         
    axis.text = element_text(size = 13, colour = "grey10"),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black", colour = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave("C:/Users/jdanielou/Desktop/plots_internship/plot/maps/map_regions_ggplot.png",
       width = 12, height = 8, dpi = 300)
