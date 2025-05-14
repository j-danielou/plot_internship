
models = list(
  oisst_climatologies = readRDS("C:/Users/jdanielou/Desktop/rds/oisst_climatologies.rds"),
  bran_climatologies = readRDS("C:/Users/jdanielou/Desktop/rds/bran_climatologies.rds"),
  hycom_climatologies = readRDS("C:/Users/jdanielou/Desktop/rds/hycom_climatologies.rds"),
  glorys_climatologies = readRDS("C:/Users/jdanielou/Desktop/rds/glorys_climatologies.rds")
)

clusters = paste0("cluster", 1:6)
colors = c("black", "blue", "darkgreen", "red")
model_names = names(models)

x11(width = 10, height = 12)
par(mfrow = c(6, 1), mar = c(1.3, 4, 0, 1), oma = c(3, 1, 2, 0),
    cex.axis = 1.2, cex.lab = 1.4)

for (i in 1:6) {
  cluster = clusters[i]
  
  y_vals = unlist(lapply(models, function(m) m[[cluster]]$climatology))
  ylim_cluster = range(y_vals, na.rm = TRUE) + c(-0.2, 0.2)
  
  months_numeric = as.numeric(models[[1]][[cluster]]$month)
  month_labels = levels(models[[1]][[cluster]]$month)
  
  plot(x = months_numeric,
       y = models[[1]][[cluster]]$climatology,
       type = "l", col = colors[1], lty = 1, lwd = 1.5,
       ylim = ylim_cluster,
       xaxt = ifelse(i < 6, "n", "s"),
       xlab = "", ylab = ifelse(i==3,"SST (°C)", ""),
       main = "")
  
  for (j in 2:length(models)) {
    lines(models[[j]][[cluster]]$climatology, col = colors[j], lwd = 1.5)
  }
  
  if (i == 6) {
    axis(1, at = 1:12, labels = month_labels, cex.axis = 1.2)
  }
}

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("top", horiz = TRUE, bty = "n",
       legend = c("OISST", "BRAN", "HYCOM", "GLORYS"), 
       col = colors, lwd = 2, cex = 1.2)
