# Charger les fonctions
source("R./function_taylor.R")

# Définir les chemins de base
base_path <- "C:/Users/jdanielou/Desktop/plot_internship/ts_csv"
products <- c("oisst", "hycom", "bran", "glorys")
zones <- c("", "west", "center", "east")

# Fonction pour charger les fichiers et extraire la colonne V1
load_data <- function(product, zone = "") {
  suffix <- if (zone == "") product else paste0(product, "_", zone)
  file_path <- file.path(base_path, product, paste0(suffix, ".csv"))
  read.table(file_path)$V1
}

# Charger toutes les données dans une liste nommée
data_list <- list()
for (prod in products) {
  for (zone in zones) {
    name <- if (zone == "") prod else paste0(prod, "_", zone)
    data_list[[name]] <- load_data(prod, zone)
  }
}

# Création des années/mois
dates <- seq(as.Date("1993-01-01"), by = "month", length.out = length(data_list$oisst))
years <- format(dates, "%Y")
months <- as.numeric(format(dates, "%m"))

# Fonction de découpe par année
split_by_year <- function(ts, offset = 0) {
  split(ts, as.numeric(years)[(1 + offset):(length(ts) + offset)])
}

# Fonction d'attribution de saison (hémisphère sud)
get_south_season <- function(month) {
  switch(as.character(month),
         "12" = "Été", "1" = "Été", "2" = "Été",
         "3" = "Automne", "4" = "Automne", "5" = "Automne",
         "6" = "Hiver", "7" = "Hiver", "8" = "Hiver",
         "9" = "Printemps", "10" = "Printemps", "11" = "Printemps")
}

# Regrouper les données par saison
seasons_vec <- sapply(months, get_south_season)
split_by_season <- function(ts, offset = 0) {
  split(ts, seasons_vec[(1 + offset):(length(ts) + offset)])
}

# Split des séries temporelles par années et saisons
data_years <- list(
  oisst = split_by_year(data_list$oisst),
  glorys = split_by_year(data_list$glorys),
  bran = split_by_year(data_list$bran),
  hycom = split_by_year(data_list$hycom, offset = 12)
)

data_seasons <- list(
  oisst = split_by_season(data_list$oisst),
  glorys = split_by_season(data_list$glorys),
  bran = split_by_season(data_list$bran),
  hycom = split_by_season(data_list$hycom, offset = 12)
)

# Paramètres de tracé
seasons <- c("Hiver", "Printemps", "Été", "Automne")
season_colors <- c("Hiver" = "navy", "Printemps" = "forestgreen", "Été" = "darkorange", "Automne" = "firebrick")
season_pch <- c("Hiver" = 15, "Printemps" = 16, "Été" = 17, "Automne" = 18)
spatial_colors <- c("west" = "peru", "center" = "magenta3", "east" = "turquoise3")
spatial_pch <- c("west" = 15, "center" = 16, "east" = 17)

# Fonction de tracé Taylor pour années
plot_taylor_years <- function(obs_years, mod_years, mod_label) {
  years_range <- names(obs_years)
  taylor.diagram(obs_years[[years_range[1]]], mod_years[[years_range[1]]],
                 "", col="grey40", pcex=1, tcex=1.2, pos.cor=TRUE)
  for (yr in years_range[-1]) {
    taylor.diagram(obs_years[[yr]], mod_years[[yr]], "", col="grey40", pcex=1,
                   tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
  }
  taylor.diagram(unlist(obs_years), unlist(mod_years), "", col="red", pcex=2, tcex=1.2,
                 pos.cor=TRUE, labpos=1, add=TRUE)
  if (mod_label=="GLORYS") legend(1, 1.8, legend = c("Year (1993 - 2021)", "All Years Combined"),col = c("grey", "red"), bty = "n", pch = c(19, 19), pt.cex = 1.5, cex = 1.2)
}

# Fonction de tracé Taylor pour saisons
plot_taylor_seasons <- function(obs_seasons, mod_seasons, mod_label) {
  if (identical(mod_seasons, data_seasons$hycom)){
    first <- seasons[1]
    taylor.diagram(obs_seasons[[first]][4:69], mod_seasons[[first]], "",
                   col = season_colors[first], pch = season_pch[first], pcex=2,
                   tcex=1.2, pos.cor=TRUE)
    for (s in seasons[-1]) {
      taylor.diagram(obs_seasons[[s]][4:69], mod_seasons[[s]], "",
                     col = season_colors[s], pch = season_pch[s],
                     pcex=2, tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
    }
  }else {
    first <- seasons[1]
    taylor.diagram(obs_seasons[[first]], mod_seasons[[first]], "",
                   col = season_colors[first], pch = season_pch[first], pcex=2,
                   tcex=1.2, pos.cor=TRUE)
    for (s in seasons[-1]) {
      taylor.diagram(obs_seasons[[s]], mod_seasons[[s]], "",
                     col = season_colors[s], pch = season_pch[s],
                     pcex=2, tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
    }
  }
  if (mod_label=="GLORYS") legend(1.2, 1.7, legend = seasons, col = season_colors, pch = season_pch,
                                  pt.cex = 1.5, cex = 1.2, bty = "n")
}

# Fonction de tracé Taylor pour spatial (west/center/east)
plot_taylor_spatial <- function(obs_list, mod_list, mod_label) {
  i=1
  if (identical(mod_list, data_list[c("hycom_west", "hycom_center", "hycom_east")])){
    
    for (zone in c("west", "center", "east")) {
      taylor.diagram(obs_list[[i]][13:276], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "west")
      i=i+1
    }
    
  }else {
    for (zone in c("west", "center", "east")) {
      taylor.diagram(obs_list[[i]], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "west")
      i=i+1
    }
  }
  if (mod_label=="GLORYS") legend(1.2, 1.8, legend = c("West", "Center", "East"),
                                  col = spatial_colors, pch = spatial_pch, pt.cex = 1.5, cex = 1.2, bty = "n")
}

#---------------------PLOTS--------------------------
x11(width = 22, height = 20)
par(mfrow = c(3, 3), oma = c(0, 6, 0, 0))

# GLORYS
plot_taylor_years(data_years$oisst, data_years$glorys, "GLORYS")
plot_taylor_seasons(data_seasons$oisst, data_seasons$glorys, "GLORYS")
plot_taylor_spatial(data_list[c("oisst_west", "oisst_center", "oisst_east")],
                    data_list[c("glorys_west", "glorys_center", "glorys_east")],
                    "GLORYS")

# BRAN
plot_taylor_years(data_years$oisst, data_years$bran, "BRAN")
plot_taylor_seasons(data_seasons$oisst, data_seasons$bran, "BRAN")
plot_taylor_spatial(data_list[c("oisst_west", "oisst_center", "oisst_east")],
                    data_list[c("bran_west", "bran_center", "bran_east")],
                    "BRAN")

# HYCOM
plot_taylor_years(data_years$oisst[2:23], data_years$hycom, "HYCOM")
plot_taylor_seasons(data_seasons$oisst, data_seasons$hycom, "HYCOM")
plot_taylor_spatial(data_list[c("oisst_west", "oisst_center", "oisst_east")],
                    data_list[c("hycom_west", "hycom_center", "hycom_east")],
                    "HYCOM")

mtext("GLORYS12v1", side = 2, outer = TRUE, line = 1, at = 0.85, cex = 1.2)
mtext("BRAN2020", side = 2, outer = TRUE, line = 1, at = 0.52, cex = 1.2)
mtext("HYCOM 3.1", side = 2, outer = TRUE, line = 1, at = 0.18, cex = 1.2)

# Texte penché sur la gauche (rotation à 90°)


dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor.png", width = 15.5, height = 13, units = "in", res = 300)
dev.off()

