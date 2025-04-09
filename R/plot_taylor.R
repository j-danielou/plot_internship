source("function_taylor.R")

oisst = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_sp.csv")
hycom = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_sp.csv")
bran = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_sp.csv")
glorys = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_sp.csv")

oisst_west = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_west.csv")
hycom_west = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_west.csv")
bran_west = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_west.csv")
glorys_west = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_west.csv")

oisst_center = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_center.csv")
hycom_center = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_center.csv")
bran_center = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_center.csv")
glorys_center = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_center.csv")

oisst_east = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_east.csv")
hycom_east = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_east.csv")
bran_east = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_east.csv")
glorys_east = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_east.csv")


oisst = oisst$V1
oisst_west = oisst_west$V1
oisst_center = oisst_center$V1
oisst_east = oisst_east$V1

glorys = glorys$V1
glorys_west = glorys_west$V1
glorys_center = glorys_center$V1
glorys_east = glorys_east$V1

bran = bran$V1
bran_west = bran_west$V1
bran_center = bran_center$V1
bran_east = bran_east$V1

hycom = hycom$V1
hycom_west = hycom_west$V1
hycom_center = hycom_center$V1
hycom_east = hycom_east$V1

years <- rep(1993:2021, each = 12)
oisst_years = split(oisst, years)
glorys_years = split(glorys, years)
bran_years = split(bran, years)
hycom_years = split(hycom, years[13:276])

years <- rep(1993:2021, each = 1)

dates <- seq(as.Date("1993-01-01"), by = "month", length.out = length(oisst))

# Extraire les mois
months <- as.numeric(format(dates, "%m"))

# Fonction pour attribuer une saison selon le mois (hémisphère sud)
get_south_season <- function(month) {
  if (month %in% c(12, 1, 2)) return("Été")
  if (month %in% c(3, 4, 5))  return("Automne")
  if (month %in% c(6, 7, 8))  return("Hiver")
  if (month %in% c(9,10,11))  return("Printemps")
}

# Appliquer la fonction à chaque mois
seasons <- sapply(months, get_south_season)

# Grouper les données par saison
oisst_season <- split(oisst, seasons)
glorys_season <- split(glorys, seasons)
bran_season <- split(bran, seasons)
hycom_season <- split(hycom, seasons[13:276])

seasons <- c("Hiver", "Printemps", "Été", "Automne")
season_colors <- c("Hiver" = "navy", "Printemps" = "forestgreen", "Été" = "darkorange", "Automne" = "firebrick")
season_pch <- c("Hiver" = 15, "Printemps" = 16, "Été" = 17, "Automne" = 18)

x11(width = 20, height = 18)
par(mfrow=c(3,3), oma = c(0, 10, 0, 0))

#-----------------PLOT GLORYS YEARS--------------------
taylor.diagram(oisst_years[["1993"]],glorys_years[["1993"]], "", col="grey40", pcex=1, tcex=1.2,
               pos.cor=TRUE)

for(year in years[2:29]) {
  year = as.character(year)
  taylor.diagram(oisst_years[[year]],glorys_years[[year]], "", col="grey40", pcex=1, tcex=1.2,
                 pos.cor=TRUE, labpos = 1, add=TRUE)
}
taylor.diagram(oisst, glorys, "", col="red", pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

legend(1,1.8, legend = c("Year (1993 - 2021)", "All Years Combined"),
       col = c("grey", "red"), bty = "n", pch = c(19,19), pt.cex = 1.5, cex=1.2)

#-----------------PLOT GLORYS SEASONS--------------------
taylor.diagram(oisst_season[["Hiver"]],glorys_season[["Hiver"]], "", pch = season_pch[["Hiver"]], 
               col = season_colors[["Hiver"]], pcex=2, tcex=1.2, pos.cor=TRUE)

for(season in seasons[-1]) {
  taylor.diagram(oisst_season[[season]], glorys_season[[season]],
                 "", col = season_colors[[season]], pch = season_pch[[season]],
                 pcex = 2 , tcex = 1.2, pos.cor = TRUE, labpos = 1, add = TRUE)
}
legend(1.2,1.8, legend = seasons, col = season_colors, pch = season_pch, pt.cex = 1.5, cex = 1.2, bty = "n")

#-----------------PLOT GLORYS SPA--------------------
taylor.diagram(oisst_west,glorys_west, "", col="peru", pch=15,pcex=2, tcex=1.2,
               pos.cor=TRUE)

taylor.diagram(oisst_center,glorys_center, "", pch = 16, col="magenta3", pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

taylor.diagram(oisst_east, glorys_east, "", col="turquoise3", pch = 17, pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

legend(1.2,1.8,legend = c("West", "Center", "East"), col = c("peru", "magenta3", "turquoise3"), pch = c(16:18), pt.cex = 1.5, cex = 1.2, bty = "n")


#-----------------PLOT BRAN YEARS--------------------
taylor.diagram(oisst_years[["1993"]],bran_years[["1993"]], "", col="grey40", pcex=1, tcex=1.2,
               pos.cor=TRUE)

for(year in years[2:29]) {
  year = as.character(year)
  taylor.diagram(oisst_years[[year]],bran_years[[year]], "", col="grey40", pcex=1, tcex=1.2,
                 pos.cor=TRUE, labpos = 1, add=TRUE)
}
taylor.diagram(oisst, bran, "", col="red", pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

#-----------------PLOT BRAN SEASONS--------------------
taylor.diagram(oisst_season[["Hiver"]],bran_season[["Hiver"]], "", pch=15, col="navy", pcex=2, tcex=1.2,
               pos.cor=TRUE)

for(season in seasons) {
  taylor.diagram(oisst_season[[season]],bran_season[[season]], "",col = season_colors[[season]], pch = season_pch[[season]], 
                 pcex=2, tcex=1.2, pos.cor=TRUE, labpos = 1, add=TRUE)
}

#-----------------PLOT BRAN SPA--------------------
taylor.diagram(oisst_west,bran_west, "", col="peru", pch=15, pcex=2, tcex=1.2,
               pos.cor=TRUE)

taylor.diagram(oisst_center,bran_center, "", col="magenta3", pch=16, pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

taylor.diagram(oisst_east, bran_east, "", col="turquoise3", pch=17, pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)


#-----------------PLOT HYCOM YEARS--------------------
taylor.diagram(oisst_years[["1994"]],hycom_years[["1994"]], "", col="grey40", pcex=1, tcex=1.2,
               pos.cor=TRUE)

for(year in years[3:22]) {
  year = as.character(year)
  taylor.diagram(oisst_years[[year]],hycom_years[[year]], "", col="grey40", pcex=1, tcex=1.2,
                 pos.cor=TRUE, labpos = 1, add=TRUE)
}
taylor.diagram(oisst[13:276], hycom, "", col="red", pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

#-----------------PLOT HYCOM SEASONS--------------------
taylor.diagram(oisst_season[["Hiver"]][4:69],hycom_season[["Hiver"]], "", pch=15, col="navy", pcex=2, tcex=1.2,
               pos.cor=TRUE)

for(season in seasons) {
  taylor.diagram(oisst_season[[season]][4:69],hycom_season[[season]], "", col = season_colors[[season]], pch = season_pch[[season]],
                 pcex=2, tcex=1.2, pos.cor=TRUE, labpos = 1, add=TRUE)
}

#-----------------PLOT HYCOM SPA--------------------
taylor.diagram(oisst_west[13:276],hycom_west, "", col="peru", pch=15, pcex=2, tcex=1.2,
               pos.cor=TRUE)

taylor.diagram(oisst_center[13:276],hycom_center, "", col="magenta3", pch=16, pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

taylor.diagram(oisst_east[13:276], hycom_east, "", col="turquoise3", pch=17, pcex=2, tcex=1.2,
               pos.cor=TRUE, labpos = 1, add=TRUE)

mtext("GLORYS12v1", side = 2, outer = TRUE, line = 0, at = 0.85, cex = 1.2)
mtext("BRAN2020", side = 2, outer = TRUE, line = 0, at = 0.52, cex = 1.2)
mtext("HYCOM 3.1", side = 2, outer = TRUE, line = 0, at = 0.18, cex = 1.2)


dev.copy(jpeg,file="C:/Users/jdanielou/Desktop/plots_internship/plot/taylor.jpeg", width = 15.5, height = 10, units = "in", res = 300)
dev.off()



