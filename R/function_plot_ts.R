plot_diff = function(time, x, y, col=c("blue", "red"), Title, Legend, ...) {
  matches <- regmatches(Title, gregexpr("[0-9]{4}", Title))[[1]]
  year1 <- as.numeric(matches[1])
  year2 <- as.numeric(matches[2])+1
  
  dif = x - y
  opar = par(no.readonly = TRUE)
  on.exit(par(opar))
  layout(matrix(c(1,1,1,1,1,2), ncol=1))
  par(mar=c(0,0,0,0), oma=c(4,4,4,4))
  plot(time, x, ylim=c(0.97, 1.03)*range(c(x, y)), type="l", 
       axes=FALSE, col=col[1], ...)
  title( paste(Title, "RMSE = ",round(sqrt(mean((y - x)^2, na.rm = TRUE)),3)), cex.main = 2, line = -2) 
  legend("topright", legend = Legend,
         col = c(col[1], col[2], "purple"), lwd = 2, bty = "n", lty = c(1, 1, 1), cex = 1.5)
  lines(time, y, col=col[2], ...)
  axis(2, las=1)
  box()
  ylim = 1.05*c(-1, 1)*max(abs(dif))
  plot(time, dif, type="h", axes=FALSE,col="purple", ylim=ylim)
  abline(h=0, lty=3)
  axis(4, las=1)
  axis(1, at = seq(1, length(time), length.out = 10),  
       labels = round(seq(year1, year2, length.out = 10)), las = 1, cex.axis = 1)
  box()
  return(invisible(NULL))
}