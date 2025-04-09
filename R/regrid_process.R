library(gts)
library(ncdf4)
library(fields)

oisst = read_gts("Y:/reanalysis/regional/southpacific/oisst-v2.1/oisst-v2r1-southpacific-sst-monthly-198109-202203.nc")
glorys = read_gts("Y:/reanalysis/regional/southpacific/glorys-v1/glorys-v1-southpacific-sst-monthly-199301-202312.nc")
bran = read_gts("Y:/reanalysis/regional/southpacific/bran-2020/bran-2020-southpacific-sst-monthly-199301-202312.nc")
hycom = read_gts("Y:/reanalysis/regional/southpacific/hycom-3.1/gofs-3.1-southpacific-sst-monthly-199401-201512.nc")


oisst$grid$mask <- mask(oisst$x)

oisst <- window(oisst, start = c(1993, 01), end = c(2021, 12))
glorys = window(glorys, start = c(1993, 01), end = c(2021, 12))
bran = window(bran, start = c(1993, 01), end = c(2021, 12))
hycom = window(hycom, start = c(1993, 01), end = c(2021, 12))

oisst_mhw = subset(oisst, lat = c(-7,5), lon = c(160, 280))
glorys_mhw = subset(glorys, lat = c(-7,5), lon = c(160, 280))
bran_mhw = subset(bran, lat = c(-7,5), lon = c(160, 280))
hycom_mhw = subset(hycom, lat = c(-7,5), lon = c(160, 280))

oisst_nw = subset(oisst, lon = c(125, 180), lat = c(-27.5,5))
oisst_nc = subset(oisst, lon = c(180, 240), lat = c(-27.5,5))
oisst_ne = subset(oisst, lon = c(240, 295), lat = c(-27.5,5))
oisst_sw = subset(oisst, lon = c(125, 180), lat = c(-60,-27.5))
oisst_sc = subset(oisst, lon = c(180, 240), lat = c(-60,-27.5))
oisst_se = subset(oisst, lon = c(240, 295), lat = c(-60,-27.5))

glorys_nw = subset(glorys, lon = c(120, 185), lat = c(-32.5,10))
glorys_nc = subset(glorys, lon = c(175, 245), lat = c(-32.5,10))
glorys_ne = subset(glorys, lon = c(235, 300), lat = c(-32.5,10))
glorys_sw = subset(glorys, lon = c(120, 185), lat = c(-65,-32.5))
glorys_sc = subset(glorys, lon = c(175, 245), lat = c(-65,-32.5))
glorys_se = subset(glorys, lon = c(235, 300), lat = c(-65,-32.5))

bran_nw = subset(bran, lon = c(120, 185), lat = c(-32.5,10))
bran_nc = subset(bran, lon = c(175, 245), lat = c(-32.5,10))
bran_ne = subset(bran, lon = c(235, 300), lat = c(-32.5,10))
bran_sw = subset(bran, lon = c(120, 185), lat = c(-65,-32.5))
bran_sc = subset(bran, lon = c(175, 245), lat = c(-65,-32.5))
bran_se = subset(bran, lon = c(235, 300), lat = c(-65,-32.5))

hycom_nw = subset(hycom, lon = c(120, 180), lat = c(-65,-32.5))
hycom_nc = subset(hycom, lon = c(180, 240), lat = c(-65,-32.5))
hycom_ne = subset(hycom, lon = c(240, 300), lat = c(-65,-32.5))
hycom_sw = subset(hycom, lon = c(120, 180), lat = c(-65,-32.5))
hycom_sc = subset(hycom, lon = c(180, 240), lat = c(-65,-32.5))
hycom_se = subset(hycom, lon = c(240, 300), lat = c(-65,-32.5))

regrid_glorys_nw = regrid(object = glorys_nw, grid = oisst_nw)
write_ncdf(regrid_glorys_nw, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_nw_v2.nc")

regrid_glorys_nc = regrid(object = glorys_nc, grid = oisst_nc)
write_ncdf(regrid_glorys_nc, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_nc.nc")

regrid_glorys_ne = regrid(object = glorys_ne, grid = oisst_ne)
write_ncdf(regrid_glorys_ne, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_ne.nc")

regrid_glorys_sw = regrid(object = glorys_sw, grid = oisst_sw)
write_ncdf(regrid_glorys_sw, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_sw.nc")

regrid_glorys_sc = regrid(object = glorys_sc, grid = oisst_sc)
write_ncdf(regrid_glorys_sc, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_sc.nc")

regrid_glorys_se = regrid(object = glorys_se, grid = oisst_se)
write_ncdf(regrid_glorys_se, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_glorys_se.nc")

regrid_bran_nw = regrid(object = bran_nw, grid = oisst_nw)
write_ncdf(regrid_bran_nw, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_nw.nc")

regrid_bran_nc = regrid(object = bran_nc, grid = oisst_nc)
write_ncdf(regrid_bran_nc, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_nc.nc")

regrid_bran_ne = regrid(object = bran_ne, grid = oisst_ne)
write_ncdf(regrid_bran_ne, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_ne.nc")

regrid_bran_sw = regrid(object = bran_sw, grid = oisst_sw)
write_ncdf(regrid_bran_sw, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_sw.nc")

regrid_bran_sc = regrid(object = bran_sc, grid = oisst_sc)
write_ncdf(regrid_bran_sc, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_sc.nc")

regrid_bran_se = regrid(object = bran_se, grid = oisst_se)
write_ncdf(regrid_bran_se, filename = "C:/Users/jdanielou/Desktop/plots_internship/regrid_files/regrid_bran_se.nc")

