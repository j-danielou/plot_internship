source("R/function_plot_ts.R")

#------------------ El Niño Area -----------------
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plot_internship/ts_csv/oisst/oisst_mhw.csv")
glorys = read.table(file = "C:/Users/jdanielou/Desktop/plot_internship/ts_csv/glorys/glorys_mhw.csv")

oisst=oisst$V1
glorys = glorys$V1
time = 1:length(glorys)

x11(width = 18, height = 12)
plot_diff(time = time, x = glorys, y = oisst, Title ="Time-Series of OISST and GLORYS12v1 (1993-01 / 2021-12),", 
          Legend = c("SST GLORYS12v1", "SST OISST", "Difference (GLORYS - OISST)"))

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_glorys/ts_oisst_glorys_mhw.png", width = 15, height = 9, units = "in", res = 150)
dev.off()


#------------------ 6 zones -----------------
# Définir les régions
regions <- c("nw", "nc", "ne", "sw", "sc", "se")

# Fonctions de lecture
read_ts <- function(dataset, region) {
  path <- paste0("C:/Users/jdanielou/Desktop/plot_internship/ts_csv/", dataset, "/", dataset, "_", region, ".csv")
  ts <- read.table(file = path)$V1
  return(ts)
}

# Lire tous les fichiers en liste nommée
oisst_list <- lapply(regions, function(r) read_ts("oisst", r))
names(oisst_list) <- regions

glorys_list <- lapply(regions, function(r) read_ts("glorys", r))
names(glorys_list) <- regions

# Définir le temps (en supposant que toutes les séries ont la même longueur)
time <- 1:length(glorys_list[[1]])

# Plot
for (r in regions) {
  x11(width = 18, height = 12)
  plot_diff(time = time,
            x = glorys_list[[r]],
            y = oisst_list[[r]],
            Title = paste0("Time-Series of OISST and GLORYS12v1 (1993-01 / 2021-12), (", toupper(r), ")"),
            Legend = c("SST GLORYS12v1", "SST OISST", "Difference (GLORYS - OISST)"),
            pos.leg = "topright")
  
  dev.copy(png,file=paste0("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_glorys/ts_oisst_glorys_",r,".png"), width = 15, height = 9, units = "in", res = 150)
  
  dev.off()
}

