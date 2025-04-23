# Charger les fonctions
source("R./function_taylor.R")

# Définir les chemins de base
base_path = "C:/Users/jdanielou/Desktop/plot_internship/ts_csv"
products = c("oisst", "hycom", "bran", "glorys")
zones = c("zone_1.2", "zone_3", "zone_3.4", "zone_4")

# Fonction pour charger les fichiers et extraire la colonne V1
load_data = function(product, zone = "") {
  suffix = if (zone == "") product else paste0(product, "_", zone)
  file_path = file.path(base_path, product, paste0(suffix, ".csv"))
  read.table(file_path)$V1
}

# Charger toutes les données dans une liste nommée
data_list = list()
for (prod in products) {
  for (zone in zones) {
    name = if (zone == "") prod else paste0(prod, "_", zone)
    data_list[[name]] = load_data(prod, zone)
  }
}

# Création des années/mois
dates = seq(as.Date("1993-01-01"), by = "month", length.out = length(data_list$oisst_zone_1.2))
years = format(dates, "%Y")
months = as.numeric(format(dates, "%m"))

# Fonction de découpe par année
split_by_year = function(ts, offset = 0) {
  split(ts, as.numeric(years)[(1 + offset):(length(ts) + offset)])
}

# Fonction d'attribution de saison (hémisphère sud)
get_south_season = function(month) {
  switch(as.character(month),
         "1" = "Summer", "2" = "Summer", "3" = "Summer",
         "4" = "Autumn", "5" = "Autumn", "6" = "Autumn",
         "7" = "Winter", "8" = "Winter", "9" = "Winter",
         "10" = "Spring", "11" = "Spring", "12" = "Spring")
}

# Regrouper les données par saison
seasons_vec = sapply(months, get_south_season)
split_by_season = function(ts, offset = 0) {
  split(ts, seasons_vec[(1 + offset):(length(ts) + offset)])
}

# Split des séries temporelles par années et saisons
data_years = list(
  oisst_zone_1.2 = split_by_year(data_list$oisst_zone_1.2),
  oisst_zone_3 = split_by_year(data_list$oisst_zone_3),
  oisst_zone_3.4 = split_by_year(data_list$oisst_zone_3.4),
  oisst_zone_4 = split_by_year(data_list$oisst_zone_4),
  glorys_zone_1.2 = split_by_year(data_list$glorys_zone_1.2),
  glorys_zone_3 = split_by_year(data_list$glorys_zone_3),
  glorys_zone_3.4 = split_by_year(data_list$glorys_zone_3.4),
  glorys_zone_4 = split_by_year(data_list$glorys_zone_4),
  bran_zone_1.2 = split_by_year(data_list$bran_zone_1.2),
  bran_zone_3 = split_by_year(data_list$bran_zone_3),
  bran_zone_3.4 = split_by_year(data_list$bran_zone_3.4),
  bran_zone_4 = split_by_year(data_list$bran_zone_4),
  hycom_zone_1.2 = split_by_year(data_list$hycom_zone_1.2, offset = 12),
  hycom_zone_3 = split_by_year(data_list$hycom_zone_3, offset = 12),
  hycom_zone_3.4 = split_by_year(data_list$hycom_zone_3.4, offset = 12), 
  hycom_zone_4 = split_by_year(data_list$hycom_zone_4, offset = 12)
)


data_seasons = list(
  oisst_zone_1.2 = split_by_season(data_list$oisst_zone_1.2),
  oisst_zone_3 = split_by_season(data_list$oisst_zone_3),
  oisst_zone_3.4 = split_by_season(data_list$oisst_zone_3.4),
  oisst_zone_4 = split_by_season(data_list$oisst_zone_4),
  glorys_zone_1.2 = split_by_season(data_list$glorys_zone_1.2),
  glorys_zone_3 = split_by_season(data_list$glorys_zone_3),
  glorys_zone_3.4 = split_by_season(data_list$glorys_zone_3.4),
  glorys_zone_4 = split_by_season(data_list$glorys_zone_4),
  bran_zone_1.2 = split_by_season(data_list$bran_zone_1.2),
  bran_zone_3 = split_by_season(data_list$bran_zone_3),
  bran_zone_3.4 = split_by_season(data_list$bran_zone_3.4),
  bran_zone_4 = split_by_season(data_list$bran_zone_4),
  hycom_zone_1.2 = split_by_season(data_list$hycom_zone_1.2, offset = 12),
  hycom_zone_3 = split_by_season(data_list$hycom_zone_3, offset = 12),
  hycom_zone_3.4 = split_by_season(data_list$hycom_zone_3.4, offset = 12), 
  hycom_zone_4 = split_by_season(data_list$hycom_zone_4, offset = 12)
)

# Paramètres de tracé
seasons = c("Winter", "Spring", "Summer", "Autumn")
season_colors = c("Winter" = "navy", "Spring" = "forestgreen", "Summer" = "darkorange", "Autumn" = "firebrick")
season_pch = c("Winter" = 15, "Spring" = 16, "Summer" = 17, "Autumn" = 18)
#spatial_colors = c("nw" = "sienna3", "nc" = "orchid4", "ne" = "turquoise4", "sw" = "peru", "sc" = "magenta3", "se" = "turquoise3")
spatial_colors = c("zone_1.2" = "#E41A1C", "zone_3" = "#377EB8", "zone_3.4" = "#4DAF4A", "zone_4" = "#984EA3")
spatial_pch = c("zone_1.2" = 15, "zone_3" = 16, "zone_3.4" = 17, "zone_4" = 18)

# Fonction de tracé Taylor pour années
plot_taylor_years = function(obs_years, mod_years, mod_label) {
  years_range = names(obs_years)
  taylor.diagram(obs_years[[years_range[1]]], mod_years[[years_range[1]]],
                 "", col="grey40", pcex=0.8, tcex=1.2, pos.cor=TRUE)
  for (yr in years_range[-1]) {
    taylor.diagram(obs_years[[yr]], mod_years[[yr]], "", col="grey40", pcex=0.8,
                   tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
  }
  taylor.diagram(unlist(obs_years), unlist(mod_years), "", col="red", pcex=1.5, tcex=1.2,
                 pos.cor=TRUE, labpos=1, add=TRUE)
  if (mod_label=="GLORYS") legend(1.1, 1.65, legend = c("Year (1993 - 2021)", "All Years Combined"),col = c("grey", "red"), bty = "0", pch = c(19, 19), 
                                  pt.cex = 1.2, cex = 1, y.intersp = 1.1)
}

# Fonction de tracé Taylor pour saisons
plot_taylor_seasons = function(obs_seasons, mod_seasons, mod_label, hycom = FALSE) {
  if (hycom == TRUE){
    first = seasons[1]
    taylor.diagram(obs_seasons[[first]][4:69], mod_seasons[[first]], "",
                   col = season_colors[first], pch = season_pch[first], pcex=1.3,
                   tcex=1.2, pos.cor=TRUE)
    for (s in seasons[-1]) {
      taylor.diagram(obs_seasons[[s]][4:69], mod_seasons[[s]], "",
                     col = season_colors[s], pch = season_pch[s],
                     pcex=1.3, tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
    }
  }else {
    first = seasons[1]
    taylor.diagram(obs_seasons[[first]], mod_seasons[[first]], "",
                   col = season_colors[first], pch = season_pch[first], pcex=1.3,
                   tcex=1.2, pos.cor=TRUE)
    for (s in seasons[-1]) {
      taylor.diagram(obs_seasons[[s]], mod_seasons[[s]], "",
                     col = season_colors[s], pch = season_pch[s],
                     pcex=1.3, tcex=1.2, pos.cor=TRUE, labpos=1, add=TRUE)
    }
  }
  if (mod_label=="GLORYS") legend(1.25, 1.75, legend = seasons, col = season_colors, pch = season_pch,
                                  pt.cex = 1.3, cex = 1.1, bty = "n", y.intersp = 1.1)
}

# Fonction de tracé Taylor pour spatial (west/center/east)
plot_taylor_spatial = function(obs_list, mod_list, mod_label, legend) {
  i=1
  if (identical(mod_list, data_list[c("hycom_zone_1.2", "hycom_zone_3", "hycom_zone_3.4", "hycom_zone_4")])){
    
    for (zone in c("zone_1.2", "zone_3", "zone_3.4", "zone_4")) {
      taylor.diagram(obs_list[[i]][13:276], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "zone_1.2")
      i=i+1
    }
    
  }else {
    for (zone in c("zone_1.2", "zone_3", "zone_3.4", "zone_4")) {
      taylor.diagram(obs_list[[i]], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "zone_1.2")
      i=i+1
    }
  }
  if (mod_label=="GLORYS") {legend(1.1, 1.71, legend = legend[1:2], ncol = 1,
                                   col = spatial_colors[1:2], pch = spatial_pch[1:2], pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 2.5)
    legend(1.45, 1.71, legend = legend[3:4], ncol = 1,
           col = spatial_colors[3:4], pch = spatial_pch[3:4], pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 2.5)
  }
}

# Fonction de tracé Taylor pour spatial subregions
plot_taylor_subregion = function(obs, mod_list, legend){
  
  for (i in 1:length(mod_list)){
    if (i==3){
      taylor.diagram(obs[13:276], mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      
    } else{
      taylor.diagram(obs, mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      if (i == 1 && length(legend) > 1) legend(1.12, 1.77, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], 
                                               pch = spatial_pch[1:length(mod_list)], pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 1.3)
    }
  }
}


# Fonction de tracé Taylor pour spatial subregions en fonction des saisons
plot_taylor_subregion_season = function(obs, mod_list, legend){
  
  for (i in 1:length(mod_list)){
    if (i==3){
      taylor.diagram(obs[4:69], mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      
    } else{
      taylor.diagram(obs, mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      if (i == 1 && length(legend) > 1) legend(1.3, 1.75, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], pch = spatial_pch[1:length(mod_list)], 
                                               pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 1.1)
    }
  }
}

#---------------------PLOTS--------------------------
x11(width = 45, height = 20)
par(mfrow = c(1, 3), oma = c(3, 1, 0, 1))

# GLORYS
plot_taylor_spatial(data_list[c("oisst_zone_1.2", "oisst_zone_3", "oisst_zone_3.4", "oisst_zone_4")],
                    data_list[c("glorys_zone_1.2", "glorys_zone_3", "glorys_zone_3.4", "glorys_zone_4")],
                    "GLORYS",legend = c("zone 1+2", "zone 3", "zone 3.4", "zone 4"))

# BRAN
plot_taylor_spatial(data_list[c("oisst_zone_1.2", "oisst_zone_3", "oisst_zone_3.4", "oisst_zone_4")],
                    data_list[c("bran_zone_1.2", "bran_zone_3", "bran_zone_3.4", "bran_zone_4")],
                    "BRAN",legend = c("zone 1+2", "zone 3", "zone 3.4", "zone 4"))

# HYCOM
plot_taylor_spatial(data_list[c("oisst_zone_1.2", "oisst_zone_3", "oisst_zone_3.4", "oisst_zone_4")],
                    data_list[c("hycom_zone_1.2", "hycom_zone_3", "hycom_zone_3.4", "hycom_zone_4")],
                    "HYCOM",legend = c("zone 1+2", "zone 3", "zone 3.4", "zone 4"))

mtext("GLORYS12v1", side = 1, outer = TRUE, line = -2, at = 0.175, cex = 1.2)
mtext("BRAN2020", side = 1, outer = TRUE, line = -2, at = 0.51, cex = 1.2)
mtext("HYCOM 3.1", side = 1, outer = TRUE, line = -2, at = 0.84, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_elnino.png", width = 12, height = 4, units = "in", res = 150)
dev.off()


#-------------------Plots Subregions-----------------------
x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

#Zone 1+2
plot_taylor_subregion(data_list$oisst_zone_1.2, mod_list = data_list[c("glorys_zone_1.2","bran_zone_1.2","hycom_zone_1.2")], legend = "")

#Zone 3
plot_taylor_subregion(data_list$oisst_zone_3, mod_list = data_list[c("glorys_zone_3","bran_zone_3","hycom_zone_3")], legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Zone 3.4
plot_taylor_subregion(data_list$oisst_zone_3.4, mod_list = data_list[c("glorys_zone_3.4","bran_zone_3.4","hycom_zone_3.4")], legend = "")

#Zone 4 
plot_taylor_subregion(data_list$oisst_zone_4, mod_list = data_list[c("glorys_zone_4","bran_zone_4","hycom_zone_4")], legend = "")


mtext("Zone 1+2", side = 3, outer = TRUE, line = -2, at = 0.27, cex = 1.2, font=2)
mtext("Zone 3", side = 3, outer = TRUE, line = -2, at = 0.76, cex = 1.2, font=2)
mtext("Zone 3.4", side = 3, outer = TRUE, line = -31, at = 0.30, cex = 1.2, font=2)
mtext("Zone 4", side = 3, outer = TRUE, line = -31, at = 0.79, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_elnino_models.png", width = 12, height = 10, units = "in", res = 150)
dev.off()


################################################
#############                      #############
############# Subregions - Seasons #############
#############                      #############
################################################

x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
#-------------GLORYS12v1---------------
#Zone 1+2
plot_taylor_seasons(data_seasons$oisst_zone_1.2, data_seasons$glorys_zone_1.2, "")

#Zone 3
plot_taylor_seasons(data_seasons$oisst_zone_3, data_seasons$glorys_zone_3, "GLORYS")

#Zone 3.4
plot_taylor_seasons(data_seasons$oisst_zone_3.4, data_seasons$glorys_zone_3.4, "")

#Zone 4
plot_taylor_seasons(data_seasons$oisst_zone_4, data_seasons$glorys_zone_4, "")

mtext("Zone 1+2", side = 3, outer = TRUE, line = -2, at = 0.27, cex = 1.2, font=2)
mtext("Zone 3", side = 3, outer = TRUE, line = -2, at = 0.76, cex = 1.2, font=2)
mtext("Zone 3.4", side = 3, outer = TRUE, line = -31, at = 0.30, cex = 1.2, font=2)
mtext("Zone 4", side = 3, outer = TRUE, line = -31, at = 0.79, cex = 1.2, font=2)


dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_elnino_season_glorys.png", width = 12, height = 10, units = "in", res = 150)
dev.off()
