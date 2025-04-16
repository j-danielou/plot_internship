library(ncdf4)
library(gts)
library(zoo)

# data = read_gts("C:/Users/jdanielou/Desktop/ERSST v5/sst.mnmean.nc")
# data = subset(data, lat = c(-5,5), lon = c(190,240))
# data_mean = mean(data, by="time")
# write.table(data_mean, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/ersst/ersst_zone_3.4.csv",
#             row.names = FALSE, col.names = FALSE)
# print(data)

# --- Charger les séries ---
bran = read.table("C:/Users/jdanielou/Desktop/plot_internship/ts_csv/bran/bran_zone_3.4.csv")$V1
ersst  = read.table("C:/Users/jdanielou/Desktop/plots_internship/ts_csv/ersst/ersst_zone_3.4.csv")$V1
ersst_sub = ersst[1669:2016]
dates_bran = seq(from = as.Date("1993-01-01"), by = "month", length.out = length(bran))
dates_ersst  = seq(from = as.Date("1854-01-01"), by = "month", length.out = length(ersst))

# --- Calcul moyenne glissante 3 mois pour BRAN ---
bran_3mois = rollmean(bran, k = 3, align = "center")
ersst_3mois = rollmean(ersst_sub, k = 3, align = "center")
dates_3mois = dates_bran[2:(length(dates_bran) - 1)]

# --- Fonction pour extraire la climatologie glissante pour une date ---
get_climatology = function(date) {
  year = as.numeric(format(date, "%Y"))
  
  # Déterminer la période de 5 ans
  year_group_start = floor((year - 1) / 5) * 5 + 1  # ex: 1993 → 1991
  clim_start = year_group_start - 30
  clim_end   = year_group_start - 1
  
  # Extraire les données ERSST de cette période
  date_start = as.Date(paste0(clim_start, "-01-01"))
  date_end   = as.Date(paste0(clim_end, "-12-31"))
  idx = which(dates_ersst >= date_start & dates_ersst <= date_end)
  
  if (length(idx) < 12 * 20) {
    warning(paste("Climatologie insuffisante pour", year, ":", clim_start, "-", clim_end))
    return(rep(NA, 12))
  }
  
  clim_data = ersst[idx]
  mois = rep(1:12, length.out = length(clim_data))
  climatologie = tapply(clim_data, mois, mean)
  return(climatologie)
}

# --- Calculer l’ONI avec climatologie glissante tous les 5 ans ---
oni_bran = numeric(length(bran_3mois))

for (i in seq_along(dates_3mois)) {
  date_i = dates_3mois[i]
  mois_i = as.numeric(format(date_i, "%m"))
  clim = get_climatology(date_i)
  oni_bran[i] = bran_3mois[i] - ((clim[ifelse(mois_i-1==0,12,mois_i-1)] + clim[mois_i] + clim[ifelse(mois_i + 1 == 13, 1, mois_i+1)])/3)
}

oni_ersst = numeric(length(ersst_3mois))

for (i in seq_along(dates_3mois)) {
  date_i = dates_3mois[i]
  mois_i = as.numeric(format(date_i, "%m"))
  clim = get_climatology(date_i)
  oni_ersst[i] = ersst_3mois[i] - ((clim[ifelse(mois_i-1==0,12,mois_i-1)] + clim[mois_i] + clim[ifelse(mois_i + 1 == 13, 1, mois_i+1)])/3)
}

#------------ PLOT COMPARAISON BRAN vs ERSST --------------------------
x11(width = 15, height = 9)
par(oma = c(0,2,0,0))
# Couleurs NOAA stylisées
el_nino_colors = c("#FFCCCC", "#FF9999", "#FF6666", "#FF3333", "#FF0000")
la_nina_colors = c("#CCCCFF", "#9999FF", "#6666FF", "#3333FF", "#0000FF")

# Seuils d’anomalie pour les phases (modéré, fort, etc.)
el_nino_thresholds = c(0.5, 1.0, 1.5, 2.0, 3.0)
la_nina_thresholds = c(-0.5, -1.0, -1.5, -2.0, -3.0)

# Initialiser le plot
plot(dates_3mois, oni_bran, type = "n", ylim = c(-2.79, 2.79), xlim = c(8811, 18557),
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n", bty = "n",
     main = paste("Comparison of ONI indexes - BRAN vs ERSST", "( RMSE = ",round(sqrt(mean((oni_bran - oni_ersst)^2, na.rm = TRUE)),3),")"),
     cex.main = 1.8, cex.lab = 1.5)

# Bandes El Niño (zones rouges)
for (i in seq_along(el_nino_thresholds)) {
  ybottom = if (i == 1) 0 else el_nino_thresholds[i - 1]
  ytop = el_nino_thresholds[i]
  rect(min(dates_3mois), ybottom, max(dates_3mois), ytop,
       col = el_nino_colors[i], border = NA)
}

# Bandes La Niña (zones bleues)
for (i in seq_along(la_nina_thresholds)) {
  ytop = if (i == 1) 0 else la_nina_thresholds[i - 1]
  ybottom = la_nina_thresholds[i]
  rect(min(dates_3mois), ybottom, max(dates_3mois), ytop,
       col = la_nina_colors[i], border = NA)
}

# Ajouter les annotations de phase
text(10500, c(0.75, 1.25, 1.75, 2.25),
     labels = c("Weak", "Moderate", "Strong", "Very Strong"),
     col = rev(c("white", "#FF9999", "#FF2222", "#FF0000")), pos = 4, cex = 1.5)

text(11700, c(-0.75, -1.25, -1.75, -2.25),
     labels = c("Weak", "Moderate", "Strong", "Very Strong"),
     col = rev(c("white", "#9999FF", "#2222FF", "#0000FF")), pos = 4, cex = 1.5)

# Grille horizontale
abline(h = seq(-2.5, 2.5, 0.5), col = "gray85", lty = 2)
abline(h = 0, col = "black", lwd = 2)

# Tracer les deux séries ONI
lines(dates_3mois, oni_bran, col = "gold", lwd = 3)
lines(dates_3mois, oni_ersst, col = "purple", lwd = 3, lty = 2)

# Axe Y
axis(2, at = seq(-3, 3, 0.5), las = 1, cex.axis = 1.3)

# Axe X : années
years = seq(from = as.Date(format(min(dates_3mois), "%Y-01-01")),
             to   = as.Date(format(max(dates_3mois)+280, "%Y-01-01")),
             by = "year")

axis(1, at = years, labels = FALSE)
mtext("ONI Index (°C)", side = 2, line = 4, cex = 1.5)
text(x = years, y = par("usr")[3] - 0.2,
     labels = format(years, "%Y"), srt = 45, adj = 1,
     xpd = TRUE, cex = 1.0)

# Légende
legend("topright", legend = c("ONI BRAN", "ONI ERSST"), text.col = "gray99",
       col = c("gold", "purple"),
       lty = c(1, 2), lwd = 2.5,
       bty = "0", bg= "gray55" ,cex = 1.3)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/oni_index/bran_oni_index.png", width = 15, height = 9, units = "in", res = 150 )
dev.off()


