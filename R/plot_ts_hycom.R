source("R/function_plot_ts.R")

#------------------ El Niño Area ----------------
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_mhw.csv")
hycom = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_mhw.csv")

oisst=oisst$V1
oisst = oisst[13:276]
hycom = hycom$V1
time = 1:length(hycom)

x11(width = 18, height = 12)
plot_diff(time = time, x = hycom, y = oisst, Title ="Time-Series of OISST and HYCOM 3.1 (1994-01 / 2015-12)", 
          Legend = c("SST HYCOM 3.1", "SST OISST", "Difference (HYCOM - OISST)"))

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/plots_hycom/ts_oisst_hycom_mhw.png", width = 15.5, height = 10, units = "in", res = 300)
dev.off()


#------------------ 6 zones -----------------
# Définir les régions
regions <- c("nw", "nc", "ne", "sw", "sc", "se")

# Fonctions de lecture
read_ts <- function(dataset, region) {
  path <- paste0("C:/Users/jdanielou/Desktop/plot_internship/ts_csv/", dataset, "/", dataset, "_", region, ".csv")
  if (dataset != "hycom") ts <- read.table(file = path)$V1[13:276]
  else ts <- read.table(file = path)$V1
  return(ts)
}

# Lire tous les fichiers en liste nommée
oisst_list <- lapply(regions, function(r) read_ts("oisst", r))
names(oisst_list) <- regions

hycom_list <- lapply(regions, function(r) read_ts("hycom", r))
names(hycom_list) <- regions

# Définir le temps (en supposant que toutes les séries ont la même longueur)
time <- 1:length(hycom_list[[1]])

# Plot
for (r in regions) {
  x11(width = 18, height = 12)
  plot_diff(time = time,
            x = hycom_list[[r]],
            y = oisst_list[[r]],
            Title = paste0("Time-Series of OISST and HYCOM 3.1 (1994-01 / 2015-12), (", toupper(r), ")"),
            Legend = c("SST HYCOM 3.1", "SST OISST", "Difference (HYCOM - OISST)"))
  
  dev.copy(png,file=paste0("C:/Users/jdanielou/Desktop/plots_internship/plot/plots_hycom/ts_oisst_hycom_",r,".png"), width = 15.5, height = 10, units = "in", res = 300)
  dev.off()
}

