# Charger les fonctions
source("R./function_taylor.R")

# Définir les chemins de base
base_path <- "C:/Users/jdanielou/Desktop/plot_internship/ts_csv"
products <- c("oisst", "hycom", "bran", "glorys")
zones <- c("", "nw", "nc", "ne", "sw", "sc", "se")

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

data_seasons <- list(
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
seasons <- c("Hiver", "Printemps", "Été", "Automne")
season_colors <- c("Hiver" = "navy", "Printemps" = "forestgreen", "Été" = "darkorange", "Automne" = "firebrick")
season_pch <- c("Hiver" = 15, "Printemps" = 16, "Été" = 17, "Automne" = 18)
#spatial_colors <- c("nw" = "sienna3", "nc" = "orchid4", "ne" = "turquoise4", "sw" = "peru", "sc" = "magenta3", "se" = "turquoise3")
spatial_colors <- c("nw" = "#E41A1C", "nc" = "#377EB8", "ne" = "#4DAF4A", "sw" = "#984EA3", "sc" = "#FF7F00", "se" = "#FFFF33")  
spatial_pch <- c("nw" = 15, "nc" = 16, "ne" = 17, "sw" = 18, "sc" = 8, "se" = 4)

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
  if (mod_label=="GLORYS") legend(1, 1.6, legend = c("Year (1993 - 2021)", "All Years Combined"),col = c("grey", "red"), bty = "n", pch = c(19, 19), pt.cex = 1.5, cex = 1.2)
}

# Fonction de tracé Taylor pour saisons
plot_taylor_seasons <- function(obs_seasons, mod_seasons, mod_label, hycom = FALSE) {
  if (hycom == TRUE){
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
  if (mod_label=="GLORYS") legend(1.2, 1.6, legend = seasons, col = season_colors, pch = season_pch,
                                  pt.cex = 1.5, cex = 1.2, bty = "n")
}

# Fonction de tracé Taylor pour spatial (west/center/east)
plot_taylor_spatial <- function(obs_list, mod_list, mod_label, legend) {
  i=1
  if (identical(mod_list, data_list[c("hycom_nw", "hycom_nc", "hycom_ne", "hycom_sw", "hycom_sc", "hycom_se")])){
    
    for (zone in c("nw", "nc", "ne", "sw", "sc", "se")) {
      taylor.diagram(obs_list[[i]][13:276], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "nw")
      i=i+1
    }
    
  }else {
    for (zone in c("nw", "nc", "ne", "sw", "sc", "se")) {
      taylor.diagram(obs_list[[i]], mod_list[[i]], "", col = spatial_colors[zone],
                     pch = spatial_pch[zone], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = zone != "nw")
      i=i+1
    }
  }
  if (mod_label=="GLORYS") legend(1.1, 1.5, legend = legend, ncol = 2,
                                  col = spatial_colors, pch = spatial_pch, pt.cex = 1.5, cex = 1.2, bty = "n")
}

# Fonction de tracé Taylor pour spatial subregions
plot_taylor_subregion = function(obs, mod_list, legend){
  
  for (i in 1:length(mod_list)){
    if (i==3){
      taylor.diagram(obs[13:276], mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      
      } else{
        taylor.diagram(obs, mod_list[[i]], "", col = spatial_colors[i],
                   pch = spatial_pch[i], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                   labpos = 1, add = i!=1)
        if (i == 1 && length(legend) > 1) legend(1.1, 1.6, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], pch = spatial_pch[1:length(mod_list)], pt.cex = 1.5, cex = 1.2, bty = "n")
        }
  }
}


# Fonction de tracé Taylor pour spatial subregions en fonction des saisons
plot_taylor_subregion_season = function(obs, mod_list, legend){
  
  for (i in 1:length(mod_list)){
    if (i==3){
      taylor.diagram(obs[4:69], mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      
    } else{
      taylor.diagram(obs, mod_list[[i]], "", col = spatial_colors[i],
                     pch = spatial_pch[i], pcex = 2, tcex = 1.2, pos.cor = TRUE,
                     labpos = 1, add = i!=1)
      if (i == 1 && length(legend) > 1) legend(1.1, 1.6, legend = legend, ncol = 1, col = spatial_colors[1:length(mod_list)], pch = spatial_pch[1:length(mod_list)], pt.cex = 1.5, cex = 1.2, bty = "n")
    }
  }
}




#---------------------PLOTS--------------------------
x11(width = 45, height = 20)
par(mfrow = c(1, 3), oma = c(6, 1, 0, 1))

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

mtext("GLORYS12v1", side = 1, outer = TRUE, line = 1, at = 0.17, cex = 1.2)
mtext("BRAN2020", side = 1, outer = TRUE, line = 1, at = 0.52, cex = 1.2)
mtext("HYCOM 3.1", side = 1, outer = TRUE, line = 1, at = 0.85, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions.png", width = 30, height = 10, units = "in", res = 300)
dev.off()


#-------------------Plots Subregions-----------------------
x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))

#N-W
plot_taylor_subregion(data_list$oisst_nw, mod_list = data_list[c("glorys_nw","bran_nw","hycom_nw")], legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#N-C
plot_taylor_subregion(data_list$oisst_nc, mod_list = data_list[c("glorys_nc","bran_nc","hycom_nc")], legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#N-E
plot_taylor_subregion(data_list$oisst_ne, mod_list = data_list[c("glorys_ne","bran_ne","hycom_ne")], legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#S-W
plot_taylor_subregion(data_list$oisst_sw, mod_list = data_list[c("glorys_sw","bran_sw","hycom_sw")], legend = "")

#S-C
plot_taylor_subregion(data_list$oisst_sc, mod_list = data_list[c("glorys_sc","bran_sc","hycom_sc")], legend = "")

#S-E
plot_taylor_subregion(data_list$oisst_se, mod_list = data_list[c("glorys_se","bran_se","hycom_se")], legend = "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions.png", width = 20, height = 13, units = "in", res = 300)
dev.off()

################################################
#############                      #############
############# Subregions - Seasons #############
#############                      #############
################################################

x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------GLORYS12v1---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$glorys_nw, "GLORYS")

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$glorys_nc, "GLORYS")

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$glorys_ne, "GLORYS")

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$glorys_sw, "")

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$glorys_sc, "")

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$glorys_se, "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_glorys.png", width = 20, height = 13, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------BRAN2020---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$bran_nw, "GLORYS")

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$bran_nc, "GLORYS")

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$bran_ne, "GLORYS")

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$bran_sw, "")

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$bran_sc, "")

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$bran_se, "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_bran.png", width = 20, height = 13, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------HYCOM3.1---------------
#N-W
plot_taylor_seasons(data_seasons$oisst_nw, data_seasons$hycom_nw, "GLORYS", hycom=TRUE)

#N-C
plot_taylor_seasons(data_seasons$oisst_nc, data_seasons$hycom_nc, "GLORYS",hycom=TRUE)

#N-E
plot_taylor_seasons(data_seasons$oisst_ne, data_seasons$hycom_ne, "GLORYS", hycom=TRUE)

#S-W
plot_taylor_seasons(data_seasons$oisst_sw, data_seasons$hycom_sw, "", hycom=TRUE)

#S-C
plot_taylor_seasons(data_seasons$oisst_sc, data_seasons$hycom_sc, "", hycom=TRUE)

#S-E
plot_taylor_seasons(data_seasons$oisst_se, data_seasons$hycom_se, "", hycom=TRUE)

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_season_hycom.png", width = 20, height = 13, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#--------------N-W / all seasons / All Models----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_nw$Été, mod_list <- list(
  glorys_nw = data_seasons[["glorys_nw"]][["Été"]],
  bran_nw   = data_seasons[["bran_nw"]][["Été"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_nw$Automne, mod_list <- list(
  glorys_nw = data_seasons[["glorys_nw"]][["Automne"]],
  bran_nw   = data_seasons[["bran_nw"]][["Automne"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_nw$Hiver, mod_list <- list(
  glorys_nw = data_seasons[["glorys_nw"]][["Hiver"]],
  bran_nw   = data_seasons[["bran_nw"]][["Hiver"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_nw$Printemps, mod_list <- list(
  glorys_nw = data_seasons[["glorys_nw"]][["Printemps"]],
  bran_nw   = data_seasons[["bran_nw"]][["Printemps"]],
  hycom_nw  = data_seasons[["hycom_nw"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_nw.png", width = 20, height = 19, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#-------------- N-C / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_nc$Été, mod_list <- list(
  glorys_nc = data_seasons[["glorys_nc"]][["Été"]],
  bran_nc   = data_seasons[["bran_nc"]][["Été"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_nc$Automne, mod_list <- list(
  glorys_nc = data_seasons[["glorys_nc"]][["Automne"]],
  bran_nc   = data_seasons[["bran_nc"]][["Automne"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_nc$Hiver, mod_list <- list(
  glorys_nc = data_seasons[["glorys_nc"]][["Hiver"]],
  bran_nc   = data_seasons[["bran_nc"]][["Hiver"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_nc$Printemps, mod_list <- list(
  glorys_nc = data_seasons[["glorys_nc"]][["Printemps"]],
  bran_nc   = data_seasons[["bran_nc"]][["Printemps"]],
  hycom_nc  = data_seasons[["hycom_nc"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_nc.png", width = 20, height = 19, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#-------------- N-E / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_ne$Été, mod_list <- list(
  glorys_ne = data_seasons[["glorys_ne"]][["Été"]],
  bran_ne   = data_seasons[["bran_ne"]][["Été"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_ne$Automne, mod_list <- list(
  glorys_ne = data_seasons[["glorys_ne"]][["Automne"]],
  bran_ne   = data_seasons[["bran_ne"]][["Automne"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_ne$Hiver, mod_list <- list(
  glorys_ne = data_seasons[["glorys_ne"]][["Hiver"]],
  bran_ne   = data_seasons[["bran_ne"]][["Hiver"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_ne$Printemps, mod_list <- list(
  glorys_ne = data_seasons[["glorys_ne"]][["Printemps"]],
  bran_ne   = data_seasons[["bran_ne"]][["Printemps"]],
  hycom_ne  = data_seasons[["hycom_ne"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_ne.png", width = 20, height = 19, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#-------------- S-W / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_sw$Été, mod_list <- list(
  glorys_sw = data_seasons[["glorys_sw"]][["Été"]],
  bran_sw   = data_seasons[["bran_sw"]][["Été"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_sw$Automne, mod_list <- list(
  glorys_sw = data_seasons[["glorys_sw"]][["Automne"]],
  bran_sw   = data_seasons[["bran_sw"]][["Automne"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_sw$Hiver, mod_list <- list(
  glorys_sw = data_seasons[["glorys_sw"]][["Hiver"]],
  bran_sw   = data_seasons[["bran_sw"]][["Hiver"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_sw$Printemps, mod_list <- list(
  glorys_sw = data_seasons[["glorys_sw"]][["Printemps"]],
  bran_sw   = data_seasons[["bran_sw"]][["Printemps"]],
  hycom_sw  = data_seasons[["hycom_sw"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_sw.png", width = 20, height = 19, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#-------------- S-C / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_sc$Été, mod_list <- list(
  glorys_sc = data_seasons[["glorys_sc"]][["Été"]],
  bran_sc   = data_seasons[["bran_sc"]][["Été"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_sc$Automne, mod_list <- list(
  glorys_sc = data_seasons[["glorys_sc"]][["Automne"]],
  bran_sc   = data_seasons[["bran_sc"]][["Automne"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_sc$Hiver, mod_list <- list(
  glorys_sc = data_seasons[["glorys_sc"]][["Hiver"]],
  bran_sc   = data_seasons[["bran_sc"]][["Hiver"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_sc$Printemps, mod_list <- list(
  glorys_sc = data_seasons[["glorys_sc"]][["Printemps"]],
  bran_sc   = data_seasons[["bran_sc"]][["Printemps"]],
  hycom_sc  = data_seasons[["hycom_sc"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_sc.png", width = 20, height = 19, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 2), oma = c(3, 0, 3, 0))
#-------------- S-E / all seasons / All Models ----------------
#Ete
plot_taylor_subregion_season(data_seasons$oisst_se$Été, mod_list <- list(
  glorys_se = data_seasons[["glorys_se"]][["Été"]],
  bran_se   = data_seasons[["bran_se"]][["Été"]],
  hycom_se  = data_seasons[["hycom_se"]][["Été"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Automne
plot_taylor_subregion_season(data_seasons$oisst_se$Automne, mod_list <- list(
  glorys_se = data_seasons[["glorys_se"]][["Automne"]],
  bran_se   = data_seasons[["bran_se"]][["Automne"]],
  hycom_se  = data_seasons[["hycom_se"]][["Automne"]] ), legend = c("GLORYS12v1", "BRAN2020", "HYCOM3.1"))

#Hiver
plot_taylor_subregion_season(data_seasons$oisst_se$Hiver, mod_list <- list(
  glorys_se = data_seasons[["glorys_se"]][["Hiver"]],
  bran_se   = data_seasons[["bran_se"]][["Hiver"]],
  hycom_se  = data_seasons[["hycom_se"]][["Hiver"]] ), legend = "")

#Printemps
plot_taylor_subregion_season(data_seasons$oisst_se$Printemps, mod_list <- list(
  glorys_se = data_seasons[["glorys_se"]][["Printemps"]],
  bran_se   = data_seasons[["bran_se"]][["Printemps"]],
  hycom_se  = data_seasons[["hycom_se"]][["Printemps"]] ), legend = "")

mtext("Eté", side = 3, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Automne", side = 3, outer = TRUE, line = 0, at = 0.77, cex = 1.2)
mtext("Hiver", side = 1, outer = TRUE, line = 0, at = 0.25, cex = 1.2)
mtext("Printemps", side = 1, outer = TRUE, line = 0, at = 0.77, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_seasons_se.png", width = 20, height = 19, units = "in", res = 300)
dev.off()


##############################################
#############                    #############
############# Subregions - Years #############
#############                    #############
##############################################

x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------GLORYS12v1---------------
#N-W
plot_taylor_years(data_years$oisst_nw, data_years$glorys_nw, "GLORYS")

#N-C
plot_taylor_years(data_years$oisst_nc, data_years$glorys_nc, "GLORYS")

#N-E
plot_taylor_years(data_years$oisst_ne, data_years$glorys_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw, data_years$glorys_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc, data_years$glorys_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se, data_years$glorys_se, "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_glorys.png", width = 20, height = 13, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------BRAN2020---------------
#N-W
plot_taylor_years(data_years$oisst_nw, data_years$bran_nw, "GLORYS")

#N-C
plot_taylor_years(data_years$oisst_nc, data_years$bran_nc, "GLORYS")

#N-E
plot_taylor_years(data_years$oisst_ne, data_years$bran_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw, data_years$bran_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc, data_years$bran_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se, data_years$bran_se, "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_bran.png", width = 20, height = 13, units = "in", res = 300)
dev.off()



x11(width = 29, height = 20)
par(mfrow = c(2, 3), oma = c(0, 6, 5, 0))
#-------------HYCOM3.1---------------
#N-W
plot_taylor_years(data_years$oisst_nw[2:23], data_years$hycom_nw, "GLORYS")

#N-C
plot_taylor_years(data_years$oisst_nc[2:23], data_years$hycom_nc, "GLORYS")

#N-E
plot_taylor_years(data_years$oisst_ne[2:23], data_years$hycom_ne, "GLORYS")

#S-W
plot_taylor_years(data_years$oisst_sw[2:23], data_years$hycom_sw, "")

#S-C
plot_taylor_years(data_years$oisst_sc[2:23], data_years$hycom_sc, "")

#S-E
plot_taylor_years(data_years$oisst_se[2:23], data_years$hycom_se, "")

mtext("North Side", side = 2, outer = TRUE, line = 1, at = 0.77, cex = 1.2)
mtext("South Side", side = 2, outer = TRUE, line = 1, at = 0.25, cex = 1.2)
mtext("West Side", side = 3, outer = TRUE, line = 1, at = 0.15, cex = 1.2)
mtext("Center Side", side = 3, outer = TRUE, line = 1, at = 0.49, cex = 1.2)
mtext("East Side", side = 3, outer = TRUE, line = 1, at = 0.81, cex = 1.2)

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_taylor/taylor_subregions_years_hycom.png", width = 20, height = 13, units = "in", res = 300)
dev.off()

