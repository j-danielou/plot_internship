source("R/function_plot_ts.R")

#------------------ El Niño Area ----------------
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plot_internship/ts_csv/oisst/oisst_mhw.csv")
bran = read.table(file = "C:/Users/jdanielou/Desktop/plot_internship/ts_csv/bran/bran_mhw.csv")

oisst=oisst$V1
bran = bran$V1
time = 1:length(bran)

x11(width = 18, height = 12)
plot_diff(time = time, x = bran, y = oisst, Title ="Time-Series of OISST and BRAN20 (1993-01 / 2021-12)", 
          Legend = c("SST BRAN2020", "SST OISST", "Difference (BRAN - OISST)"))

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_bran/ts_oisst_bran_mhw.png", width = 15.5, height = 10, units = "in", res = 300)
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

bran_list <- lapply(regions, function(r) read_ts("bran", r))
names(bran_list) <- regions

# Définir le temps (en supposant que toutes les séries ont la même longueur)
time <- 1:length(bran_list[[1]])

# Plot
for (r in regions) {
  x11(width = 18, height = 12)
  plot_diff(time = time,
            x = bran_list[[r]],
            y = oisst_list[[r]],
            Title = paste0("Time-Series of OISST and BRAN20 (1993-01 / 2021-12), (", toupper(r), ")"),
            Legend = c("SST BRAN2020", "SST OISST", "Difference (BRAN - OISST)"))
  
  dev.copy(png,file=paste0("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_bran/ts_oisst_bran_",r,".png"), width = 15.5, height = 10, units = "in", res = 300)
  dev.off()
}

