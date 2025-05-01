# Charger les fonctions
source("R./function_taylor.R")

# Définir les chemins de base
base_path = "C:/Users/jdanielou/Desktop/plot_internship/csv/ts_csv"
products = c("oisst", "hycom", "bran", "glorys")
zones = c("full", "nw", "nc", "ne", "sw", "sc", "se")

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
dates = seq(as.Date("1993-01-01"), by = "month", length.out = length(data_list$oisst_full))
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
  oisst_nw = split_by_year(data_list$oisst_nw),
  oisst_nc = split_by_year(data_list$oisst_nc),
  oisst_ne = split_by_year(data_list$oisst_ne),
  oisst_sw = split_by_year(data_list$oisst_sw),
  oisst_sc = split_by_year(data_list$oisst_sc),
  oisst_se = split_by_year(data_list$oisst_se),
  glorys_nw = split_by_year(data_list$glorys_nw),
  glorys_nc = split_by_year(data_list$glorys_nc),
  glorys_ne = split_by_year(data_list$glorys_ne),
  glorys_sw = split_by_year(data_list$glorys_sw),
  glorys_sc = split_by_year(data_list$glorys_sc),
  glorys_se = split_by_year(data_list$glorys_se),
  bran_nw = split_by_year(data_list$bran_nw),
  bran_nc = split_by_year(data_list$bran_nc),
  bran_ne = split_by_year(data_list$bran_ne),
  bran_sw = split_by_year(data_list$bran_sw),
  bran_sc = split_by_year(data_list$bran_sc),
  bran_se = split_by_year(data_list$bran_se),
  hycom_nw = split_by_year(data_list$hycom_nw, offset = 12),
  hycom_nc = split_by_year(data_list$hycom_nc, offset = 12),
  hycom_ne = split_by_year(data_list$hycom_ne, offset = 12),
  hycom_sw = split_by_year(data_list$hycom_sw, offset = 12),
  hycom_sc = split_by_year(data_list$hycom_sc, offset = 12),
  hycom_se = split_by_year(data_list$hycom_se, offset = 12) 
)

data_seasons = list(
  oisst_nw = split_by_season(data_list$oisst_nw),
  oisst_nc = split_by_season(data_list$oisst_nc),
  oisst_ne = split_by_season(data_list$oisst_ne),
  oisst_sw = split_by_season(data_list$oisst_sw),
  oisst_sc = split_by_season(data_list$oisst_sc),
  oisst_se = split_by_season(data_list$oisst_se),
  glorys_nw = split_by_season(data_list$glorys_nw),
  glorys_nc = split_by_season(data_list$glorys_nc),
  glorys_ne = split_by_season(data_list$glorys_ne),
  glorys_sw = split_by_season(data_list$glorys_sw),
  glorys_sc = split_by_season(data_list$glorys_sc),
  glorys_se = split_by_season(data_list$glorys_se),
  bran_nw = split_by_season(data_list$bran_nw),
  bran_nc = split_by_season(data_list$bran_nc),
  bran_ne = split_by_season(data_list$bran_ne),
  bran_sw = split_by_season(data_list$bran_sw),
  bran_sc = split_by_season(data_list$bran_sc),
  bran_se = split_by_season(data_list$bran_se),
  hycom_nw = split_by_season(data_list$hycom_nw, offset = 12),
  hycom_nc = split_by_season(data_list$hycom_nc, offset = 12),
  hycom_ne = split_by_season(data_list$hycom_ne, offset = 12),
  hycom_sw = split_by_season(data_list$hycom_sw, offset = 12),
  hycom_sc = split_by_season(data_list$hycom_sc, offset = 12),
  hycom_se = split_by_season(data_list$hycom_se, offset = 12) 
)

# Paramètres de tracé
seasons = c("Winter", "Spring", "Summer", "Autumn")
season_colors = c("Winter" = "navy", "Spring" = "forestgreen", "Summer" = "darkorange", "Autumn" = "firebrick")
season_pch = c("Winter" = 15, "Spring" = 16, "Summer" = 17, "Autumn" = 18)
#spatial_colors = c("nw" = "sienna3", "nc" = "orchid4", "ne" = "turquoise4", "sw" = "peru", "sc" = "magenta3", "se" = "turquoise3")
spatial_colors = c("nw" = "#E41A1C", "nc" = "#377EB8", "ne" = "#4DAF4A", "sw" = "#984EA3", "sc" = "#FF7F00", "se" = "#FFFF33")  
spatial_pch = c("nw" = 15, "nc" = 16, "ne" = 17, "sw" = 18, "sc" = 8, "se" = 4)

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
  if (mod_label=="GLORYS") legend(1.3, 1.65, legend = seasons, col = season_colors, pch = season_pch,
                                  pt.cex = 1.5, cex = 1.2, bty = "o", y.intersp = 1.2)
}

# Fonction de tracé Taylor pour spatial (west/center/east)
plot_taylor_spatial = function(obs_list, mod_list, mod_label, legend) {
  i=1
  if (identical(mod_list, data_list[c("hycom_nw", "hycom_nc", "hycom_ne", "hycom_sw", "hycom_sc", "hycom_se")])){
    
    for (zone in c("nw", "nc", "ne", "sw", "sc", "se")) {
      taylor.diagram(obs_list[[i]][13:276], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "nw")
      i=i+1
    }
    
  }else {
    for (zone in c("nw", "nc", "ne", "sw", "sc", "se")) {
      taylor.diagram(obs_list[[i]], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 1.5, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "nw")
      i=i+1
    }
  }
  if (mod_label=="GLORYS") {legend(1.1, 1.7, legend = legend[1:3], ncol = 1,
                                  col = spatial_colors[1:3], pch = spatial_pch[1:3], pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 2.5)
    legend(1.35, 1.7, legend = legend[4:6], ncol = 1,
           col = spatial_colors[4:6], pch = spatial_pch[4:6], pt.cex = 1.2, cex = 1, bty = "n", y.intersp = 2.5)
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
        if (i == 1 && length(legend) > 1) legend(1.1, 1.65, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], 
                                                 pch = spatial_pch[1:length(mod_list)], pt.cex = 1.2, cex = 1, bty = "o", y.intersp = 1.3)
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
      if (i == 1 && length(legend) > 1) legend(1.3, 1.6, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], pch = spatial_pch[1:length(mod_list)], 
                                               pt.cex = 1.2, cex = 1, bty = "o", y.intersp = 1.1)
    }
  }
}




#---------------------PLOTS--------------------------
x11(width = 45, height = 20)
par(mfrow = c(1, 3), oma = c(3, 1, 0, 1))

# GLORYS
plot_taylor_spatial(data_list[c("oisst_nw", "oisst_nc", "oisst_ne", "oisst_sw", "oisst_sc", "oisst_se")],
                    data_list[c("glorys_nw", "glorys_nc", "glorys_ne", "glorys_sw", "glorys_sc", "glorys_se")],
                    "GLORYS",legend = c("N-W", "N-C", "N-E", "S-W", "S-C", "S-E"))

# BRAN
plot_taylor_spatial(data_list[c("oisst_nw", "oisst_nc", "oisst_ne", "oisst_sw", "oisst_sc", "oisst_se")],
                    data_list[c("bran_nw", "bran_nc", "bran_ne", "bran_sw", "bran_sc", "bran_se")],
                    "BRAN",legend = c("N-W", "N-C", "N-E", "S-W", "S-C", "S-E"))

# HYCOM
plot_taylor_spatial(data_list[c("oisst_nw", "oisst_nc", "oisst_ne", "oisst_sw", "oisst_sc", "oisst_se")],
                    data_list[c("hycom_nw", "hycom_nc", "hycom_ne", "hycom_sw", "hycom_sc", "hycom_se")],
                    "HYCOM",legend = c("N-W", "N-C", "N-E", "S-W", "S-C", "S-E"))

mtext("GLORYS12v1", side = 1, outer = TRUE, line = -2, at = 0.175, cex = 1.2)
mtext("BRAN2020", side = 1, outer = TRUE, line = -2, at = 0.51, cex = 1.2)
mtext("HYCOM 3.1", side = 1, outer = TRUE, line = -2, at = 0.84, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions.png", width = 12, height = 4, units = "in", res = 150)
dev.off()


#-------------------Plots Subregions-----------------------
x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 3, 3, 0))

#N-W
plot_taylor_subregion(data_list$oisst_nw, mod_list = data_list[c("glorys_nw","bran_nw","hycom_nw")], legend = "")

#N-C
plot_taylor_subregion(data_list$oisst_nc, mod_list = data_list[c("glorys_nc","bran_nc","hycom_nc")], legend = "")

#N-E
plot_taylor_subregion(data_list$oisst_ne, mod_list = data_list[c("glorys_ne","bran_ne","hycom_ne")], legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#S-W
plot_taylor_subregion(data_list$oisst_sw, mod_list = data_list[c("glorys_sw","bran_sw","hycom_sw")], legend = "")

#S-C
plot_taylor_subregion(data_list$oisst_sc, mod_list = data_list[c("glorys_sc","bran_sc","hycom_sc")], legend = "")

#S-E
plot_taylor_subregion(data_list$oisst_se, mod_list = data_list[c("glorys_se","bran_se","hycom_se")], legend = "")

mtext("North Side", side = 2, outer = TRUE, line = 0, at = 0.76, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 0, at = 0.26, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 0, at = 0.16, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 0, at = 0.50, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 0, at = 0.83, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_models.png", width = 15, height = 9, units = "in", res = 150)
dev.off()

################################################
#############                      #############
############# Subregions - Seasons #############
#############                      #############
################################################

x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------GLORYS12v1---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$glorys_nw, "")

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$glorys_nc, "")

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$glorys_ne, "GLORYS")

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$glorys_sw, "")

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$glorys_sc, "")

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$glorys_se, "")

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_glorys.png", width = 15, height = 9, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------BRAN2020---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$bran_nw, "")

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$bran_nc, "")

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$bran_ne, "GLORYS")

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$bran_sw, "")

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$bran_sc, "")

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$bran_se, "")

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_bran.png", width = 15, height = 9, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------HYCOM3.1---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$hycom_nw, "", hycom=TRUE)

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$hycom_nc, "",hycom=TRUE)

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$hycom_ne, "GLORYS", hycom=TRUE)

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$hycom_sw, "", hycom=TRUE)

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$hycom_sc, "", hycom=TRUE)

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$hycom_se, "", hycom=TRUE)

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_hycom.png", width = 15, height = 9, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#--------------N-W / all seasons / All Models----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_nw$Summer, mod_list = list(
  glorys_nw = data_seasons[["glorys_nw"]][["Summer"]],
  bran_nw   = data_seasons[["bran_nw"]][["Summer"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Summer"]] ), legend = c(""))

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_nw$Autumn, mod_list = list(
  glorys_nw = data_seasons[["glorys_nw"]][["Autumn"]],
  bran_nw   = data_seasons[["bran_nw"]][["Autumn"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_nw$Winter, mod_list = list(
  glorys_nw = data_seasons[["glorys_nw"]][["Winter"]],
  bran_nw   = data_seasons[["bran_nw"]][["Winter"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_nw$Spring, mod_list = list(
  glorys_nw = data_seasons[["glorys_nw"]][["Spring"]],
  bran_nw   = data_seasons[["bran_nw"]][["Spring"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_nw.png", width = 15, height = 14, units = "in", res = 150 )
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#-------------- N-C / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_nc$Summer, mod_list = list(
  glorys_nc = data_seasons[["glorys_nc"]][["Summer"]],
  bran_nc   = data_seasons[["bran_nc"]][["Summer"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Summer"]] ), legend = c(""))

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_nc$Autumn, mod_list = list(
  glorys_nc = data_seasons[["glorys_nc"]][["Autumn"]],
  bran_nc   = data_seasons[["bran_nc"]][["Autumn"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_nc$Winter, mod_list = list(
  glorys_nc = data_seasons[["glorys_nc"]][["Winter"]],
  bran_nc   = data_seasons[["bran_nc"]][["Winter"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_nc$Spring, mod_list = list(
  glorys_nc = data_seasons[["glorys_nc"]][["Spring"]],
  bran_nc   = data_seasons[["bran_nc"]][["Spring"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_nc.png", width = 15, height = 14, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#-------------- N-E / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_ne$Summer, mod_list = list(
  glorys_ne = data_seasons[["glorys_ne"]][["Summer"]],
  bran_ne   = data_seasons[["bran_ne"]][["Summer"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Summer"]] ), legend = c(""))

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_ne$Autumn, mod_list = list(
  glorys_ne = data_seasons[["glorys_ne"]][["Autumn"]],
  bran_ne   = data_seasons[["bran_ne"]][["Autumn"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_ne$Winter, mod_list = list(
  glorys_ne = data_seasons[["glorys_ne"]][["Winter"]],
  bran_ne   = data_seasons[["bran_ne"]][["Winter"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_ne$Spring, mod_list = list(
  glorys_ne = data_seasons[["glorys_ne"]][["Spring"]],
  bran_ne   = data_seasons[["bran_ne"]][["Spring"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_ne.png", width = 15, height = 14, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#-------------- S-W / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_sw$Summer, mod_list = list(
  glorys_sw = data_seasons[["glorys_sw"]][["Summer"]],
  bran_sw   = data_seasons[["bran_sw"]][["Summer"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Summer"]] ), legend = "")

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_sw$Autumn, mod_list = list(
  glorys_sw = data_seasons[["glorys_sw"]][["Autumn"]],
  bran_sw   = data_seasons[["bran_sw"]][["Autumn"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_sw$Winter, mod_list = list(
  glorys_sw = data_seasons[["glorys_sw"]][["Winter"]],
  bran_sw   = data_seasons[["bran_sw"]][["Winter"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_sw$Spring, mod_list = list(
  glorys_sw = data_seasons[["glorys_sw"]][["Spring"]],
  bran_sw   = data_seasons[["bran_sw"]][["Spring"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_sw.png", width = 15, height = 14, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#-------------- S-C / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_sc$Summer, mod_list = list(
  glorys_sc = data_seasons[["glorys_sc"]][["Summer"]],
  bran_sc   = data_seasons[["bran_sc"]][["Summer"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Summer"]] ), legend = "")

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_sc$Autumn, mod_list = list(
  glorys_sc = data_seasons[["glorys_sc"]][["Autumn"]],
  bran_sc   = data_seasons[["bran_sc"]][["Autumn"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_sc$Winter, mod_list = list(
  glorys_sc = data_seasons[["glorys_sc"]][["Winter"]],
  bran_sc   = data_seasons[["bran_sc"]][["Winter"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_sc$Spring, mod_list = list(
  glorys_sc = data_seasons[["glorys_sc"]][["Spring"]],
  bran_sc   = data_seasons[["bran_sc"]][["Spring"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_sc.png", width = 15, height = 14, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(2, 0, 2, 0))
#-------------- S-E / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_se$Summer, mod_list = list(
  glorys_se = data_seasons[["glorys_se"]][["Summer"]],
  bran_se   = data_seasons[["bran_se"]][["Summer"]],
  hycom_se  = data_seasons[["hycom_se"]][["Summer"]] ), legend = "")

#Autumn
plot_taylor_subregion_season(data_seasons$oisst_se$Autumn, mod_list = list(
  glorys_se = data_seasons[["glorys_se"]][["Autumn"]],
  bran_se   = data_seasons[["bran_se"]][["Autumn"]],
  hycom_se  = data_seasons[["hycom_se"]][["Autumn"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Winter
plot_taylor_subregion_season(data_seasons$oisst_se$Winter, mod_list = list(
  glorys_se = data_seasons[["glorys_se"]][["Winter"]],
  bran_se   = data_seasons[["bran_se"]][["Winter"]],
  hycom_se  = data_seasons[["hycom_se"]][["Winter"]] ), legend = "")

#Spring
plot_taylor_subregion_season(data_seasons$oisst_se$Spring, mod_list = list(
  glorys_se = data_seasons[["glorys_se"]][["Spring"]],
  bran_se   = data_seasons[["bran_se"]][["Spring"]],
  hycom_se  = data_seasons[["hycom_se"]][["Spring"]] ), legend = "")

mtext("Summer", side = 3, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Autumn", side = 3, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)
mtext("Winter", side = 1, outer = TRUE, line = -1.5, at = 0.25, cex = 1.2, font = 2)
mtext("Spring", side = 1, outer = TRUE, line = -1.5, at = 0.77, cex = 1.2, font = 2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_se.png", width = 15, height = 14, units = "in", res = 150)
dev.off()


##############################################
#############                    #############
############# Subregions - Years #############
#############                    #############
##############################################

x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------GLORYS12v1---------------
#N-W
plot_taylor_years(data_years$oisst_nw, data_years$glorys_nw, "")

#N-C
plot_taylor_years(data_years$oisst_nc, data_years$glorys_nc, "")

#N-E
plot_taylor_years(data_years$oisst_ne, data_years$glorys_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw, data_years$glorys_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc, data_years$glorys_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se, data_years$glorys_se, "")

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_glorys.png", width = 15, height = 9, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------BRAN2020---------------
#N-W
plot_taylor_years(data_years$oisst_nw, data_years$bran_nw, "")

#N-C
plot_taylor_years(data_years$oisst_nc, data_years$bran_nc, "")

#N-E
plot_taylor_years(data_years$oisst_ne, data_years$bran_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw, data_years$bran_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc, data_years$bran_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se, data_years$bran_se, "")

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_bran.png", width = 15, height = 9, units = "in", res = 150)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
#-------------HYCOM3.1---------------
#N-W
plot_taylor_years(data_years$oisst_nw[2:23], data_years$hycom_nw, "")

#N-C
plot_taylor_years(data_years$oisst_nc[2:23], data_years$hycom_nc, "")

#N-E
plot_taylor_years(data_years$oisst_ne[2:23], data_years$hycom_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw[2:23], data_years$hycom_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc[2:23], data_years$hycom_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se[2:23], data_years$hycom_se, "")

mtext("N-W", side = 3, outer = TRUE, line = -2, at = 0.20, cex = 1.2, font=2)
mtext("N-C", side = 3, outer = TRUE, line = -2, at = 0.54, cex = 1.2, font=2)
mtext("N-E", side = 3, outer = TRUE, line = -2, at = 0.86, cex = 1.2, font=2)
mtext("S-W", side = 3, outer = TRUE, line = -38, at = 0.27, cex = 1.2, font=2)
mtext("S-C", side = 3, outer = TRUE, line = -38, at = 0.60, cex = 1.2, font=2)
mtext("S-E", side = 3, outer = TRUE, line = -38, at = 0.94, cex = 1.2, font=2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_hycom.png", width = 15, height = 9, units = "in", res = 150)
dev.off()

