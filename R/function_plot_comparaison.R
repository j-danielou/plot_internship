#------------------ 11 zones (subregions & El Niño zones) -----------------
# Définir les régions
regions = c("full", "mhw", "nw", "nc", "ne", "sw", "sc", "se", "zone_1.2", "zone_3", "zone_3.4", "zone_4")

# Fonctions de lecture
read_ts = function(dataset, region) {
  path = paste0("C:/Users/jdanielou/Desktop/plot_internship/ts_csv/", dataset, "/", dataset, "_", region, ".csv")
  ts = read.table(file = path)$V1
  return(ts)
}

# Lire tous les fichiers en liste nommée
oisst_list = lapply(regions, function(r) read_ts("oisst", r))
names(oisst_list) = regions

glorys_list = lapply(regions, function(r) read_ts("glorys", r))
names(glorys_list) = regions

bran_list = lapply(regions, function(r) read_ts("bran", r))
names(bran_list) = regions

hycom_list <- lapply(regions, function(r) {
  ts <- read_ts("hycom", r)
  c(rep(NA, 12), ts, rep(NA, 72))
})
names(hycom_list) <- regions

time = seq.Date(from = as.Date("1993-1-1"), as.Date("2021-12-31"), by = 'month')

x11(width = 15, height = 12)
par(mfrow=c(3,1), mar=c(0,3,0,1), oma=c(3,1,1,3))
plot(time, glorys_list$full, axes=FALSE, type="l", ylim = c(14,21), col = "blue", lwd = 2)
lines(time,oisst_list$full, col="red", lwd = 2)
axis(2, las=1, cex.axis = 2)
box()
mtext(3, adj=0.05, text="GLORYS", cex=1.5, line=-2.5)
plot(time, bran_list$full, axes=FALSE, type="l", ylim = c(14,21), col = "blue", lwd = 2)
lines(time,oisst_list$full, col="red", lwd = 2)
axis(4, las=1, cex.axis = 2)
box()
mtext(3, adj= 0.05, text="BRAN", cex=1.5, line=-2.5)
plot(time, hycom_list$full, axes=FALSE, type="l", ylim = c(14,21), col = "blue", lwd = 2)
lines(time,oisst_list$full, col="red", lwd = 2)
mtext(3, adj=0.05, text="HYCOM", cex=1.5, line=-3)
axis(2, las=1, cex.axis = 2)
axis.Date(1, at = seq(min(time), max(time), by = "2 years"), format = "%Y", cex.axis = 2)
box()

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/article_plots/ts_full/ts_oisst_models_full.png", width = 15, height = 12, units = "in", res = 150)
dev.off()



dif_glorys = glorys_list$full - oisst_list$full
dif_bran = bran_list$full - oisst_list$full
dif_hycom = hycom_list$full - oisst_list$full

x11(width = 15, height = 12)
par(mfrow=c(3,1), mar=c(0,3,0,1), oma=c(3,1,1,3))
plot(time, dif_glorys, type="h", axes=FALSE,col="purple", ylim = c(-2.75,0.5))
abline(h=0, lty=3)
axis(2, las=1, cex.axis = 2)
box()
plot(time, dif_bran, type="h", axes=FALSE,col="purple", ylim = c(-2.75,0.5))
abline(h=0, lty=3)
axis(4, las=1, cex.axis = 2)
box()
plot(time, dif_hycom, type="h", axes=FALSE,col="purple", ylim = c(-2.75,0.5))
abline(h=0, lty=3)
axis(2, las=1, cex.axis = 2)
axis.Date(1, at = seq(min(time), max(time), by = "2 years"), format = "%Y", cex.axis = 2)
box()

dev.copy(png,file="C:/Users/jdanielou/Desktop/plots_internship/plot/article_plots/ts_full/dif_oisst_models_full.png", width = 15, height = 12, units = "in", res = 150)
dev.off()


