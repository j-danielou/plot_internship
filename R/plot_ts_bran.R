source("R/function_plot_ts.R")


#------------------ 11 zones (subregions & El Niño zones) -----------------
# Définir les régions
regions = c("mhw", "nw", "nc", "ne", "sw", "sc", "se", "zone_1.2", "zone_3", "zone_3.4", "zone_4")

# Fonctions de lecture
read_ts = function(dataset, region) {
  path = paste0("C:/Users/jdanielou/Desktop/plot_internship/ts_csv/", dataset, "/", dataset, "_", region, ".csv")
  ts = read.table(file = path)$V1
  return(ts)
}

# Lire tous les fichiers en liste nommée
oisst_list = lapply(regions, function(r) read_ts("oisst", r))
names(oisst_list) = regions

bran_list = lapply(regions, function(r) read_ts("bran", r))
names(bran_list) = regions

# Définir le temps (en supposant que toutes les séries ont la même longueur)
time = 1:length(bran_list[[1]])

# Plot
for (r in regions) {
  x11(width = 18, height = 12)
  plot_diff(time = time,
            x = bran_list[[r]],
            y = oisst_list[[r]],
            Title = paste0("Time-Series of OISST and BRAN20 (1993-01 / 2021-12), (", toupper(r), ")"),
            Legend = c("SST BRAN2020", "SST OISST", "Difference (BRAN - OISST)"),
            pos.leg = "topright",
            lwd = 1.6)
  
  dev.copy(png,file=paste0("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_bran/ts_oisst_bran_",r,".png"), width = 15.5, height = 9, units = "in", res = 150)
  dev.off()
}

