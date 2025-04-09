library(gts)
library(ncdf4)

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

oisst_nw = subset(oisst, lon = c(120, 180), lat = c(-27.5,5))
oisst_nc = subset(oisst, lon = c(180, 240), lat = c(-27.5,5))
oisst_ne = subset(oisst, lon = c(240, 300), lat = c(-27.5,5))
oisst_sw = subset(oisst, lon = c(120, 180), lat = c(-60,-27.5))
oisst_sc = subset(oisst, lon = c(180, 240), lat = c(-60,-27.5))
oisst_se = subset(oisst, lon = c(240, 300), lat = c(-60,-27.5))

glorys_nw = subset(glorys, lon = c(120, 180), lat = c(-27.5,5))
glorys_nc = subset(glorys, lon = c(180, 240), lat = c(-27.5,5))
glorys_ne = subset(glorys, lon = c(240, 300), lat = c(-27.5,5))
glorys_sw = subset(glorys, lon = c(120, 180), lat = c(-60,-27.5))
glorys_sc = subset(glorys, lon = c(180, 240), lat = c(-60,-27.5))
glorys_se = subset(glorys, lon = c(240, 300), lat = c(-60,-27.5))


bran_nw = subset(bran, lon = c(120, 180), lat = c(-27.5,5))
bran_nc = subset(bran, lon = c(180, 240), lat = c(-27.5,5))
bran_ne = subset(bran, lon = c(240, 300), lat = c(-27.5,5))
bran_sw = subset(bran, lon = c(120, 180), lat = c(-60,-27.5))
bran_sc = subset(bran, lon = c(180, 240), lat = c(-60,-27.5))
bran_se = subset(bran, lon = c(240, 300), lat = c(-60,-27.5))

hycom_nw = subset(hycom, lon = c(120, 180), lat = c(-27.5,5))
hycom_nc = subset(hycom, lon = c(180, 240), lat = c(-27.5,5))
hycom_ne = subset(hycom, lon = c(240, 300), lat = c(-27.5,5))
hycom_sw = subset(hycom, lon = c(120, 180), lat = c(-60,-27.5))
hycom_sc = subset(hycom, lon = c(180, 240), lat = c(-60,-27.5))
hycom_se = subset(hycom, lon = c(240, 300), lat = c(-60,-27.5))

OISST_mhw <- mean(oisst_mhw, by= "time" )
GLORYS_mhw <- mean(glorys_mhw, by= "time" )
HYCOM_mhw <- mean(hycom_mhw, by= "time" )
BRAN_mhw <- mean(bran_mhw, by= "time" )

write.table(OISST_mhw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_mhw.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS_mhw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_mhw.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN_mhw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_mhw.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM_mhw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_mhw.csv",  row.names = FALSE, col.names = FALSE)

OISST_west <- mean(oisst_west, by= "time" )
GLORYS_west <- mean(glorys_west, by= "time" )
HYCOM_west <- mean(hycom_west, by= "time" )
BRAN_west <- mean(bran_west, by= "time" )

write.table(OISST_west, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_west.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS_west, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_west.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN_west, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_west.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM_west, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_west.csv",  row.names = FALSE, col.names = FALSE)

OISST_center <- mean(oisst_center, by= "time" )
GLORYS_center <- mean(glorys_center, by= "time" )
HYCOM_center <- mean(hycom_center, by= "time")
BRAN_center <- mean(bran_center, by= "time" )

write.table(OISST_center, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_center.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS_center, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_center.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN_center, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_center.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM_center, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_center.csv",  row.names = FALSE, col.names = FALSE)

OISST_east <- mean(oisst_east, by= "time" )
GLORYS_east <- mean(glorys_east, by= "time" )
HYCOM_east <- mean(hycom_east, by= "time" )
BRAN_east <- mean(bran_east, by= "time" )

write.table(OISST_east, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_east.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS_east, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_east.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN_east, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_east.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM_east, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_east.csv",  row.names = FALSE, col.names = FALSE)

OISST <- mean(oisst, by= "time" )
GLORYS <- mean(glorys, by= "time" )
HYCOM <- mean(hycom, by= "time" )
BRAN <- mean(bran, by= "time" )

write.table(OISST, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_sp.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_sp.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_sp.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_sp.csv",  row.names = FALSE, col.names = FALSE)


OISST_nw <- mean(oisst_nw, by= "time")
OISST_nc <- mean(oisst_nc, by= "time") 
OISST_ne <- mean(oisst_ne, by= "time") 
OISST_sw <- mean(oisst_sw, by= "time") 
OISST_sc <- mean(oisst_sc, by= "time") 
OISST_se <- mean(oisst_se, by= "time") 

write.table(OISST_nw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_nw.csv",  row.names = FALSE, col.names = FALSE)
write.table(OISST_nc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_nc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(OISST_ne, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_ne.csv",  row.names = FALSE, col.names = FALSE)   
write.table(OISST_sw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_sw.csv",  row.names = FALSE, col.names = FALSE)   
write.table(OISST_sc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_sc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(OISST_se, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_se.csv",  row.names = FALSE, col.names = FALSE)   
                  
                  
GLORYS_nw <- mean(glorys_nw, by= "time")
GLORYS_nc <- mean(glorys_nc, by= "time") 
GLORYS_ne <- mean(glorys_ne, by= "time") 
GLORYS_sw <- mean(glorys_sw, by= "time") 
GLORYS_sc <- mean(glorys_sc, by= "time") 
GLORYS_se <- mean(glorys_se, by= "time") 

write.table(GLORYS_nw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_nw.csv",  row.names = FALSE, col.names = FALSE)
write.table(GLORYS_nc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_nc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(GLORYS_ne, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_ne.csv",  row.names = FALSE, col.names = FALSE)   
write.table(GLORYS_sw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_sw.csv",  row.names = FALSE, col.names = FALSE)   
write.table(GLORYS_sc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_sc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(GLORYS_se, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_se.csv",  row.names = FALSE, col.names = FALSE)                   


BRAN_nw <- mean(bran_nw, by= "time")
BRAN_nc <- mean(bran_nc, by= "time") 
BRAN_ne <- mean(bran_ne, by= "time") 
BRAN_sw <- mean(bran_sw, by= "time") 
BRAN_sc <- mean(bran_sc, by= "time") 
BRAN_se <- mean(bran_se, by= "time") 

write.table(BRAN_nw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_nw.csv",  row.names = FALSE, col.names = FALSE)
write.table(BRAN_nc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_nc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(BRAN_ne, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_ne.csv",  row.names = FALSE, col.names = FALSE)   
write.table(BRAN_sw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_sw.csv",  row.names = FALSE, col.names = FALSE)   
write.table(BRAN_sc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_sc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(BRAN_se, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_se.csv",  row.names = FALSE, col.names = FALSE)  


HYCOM_nw <- mean(hycom_nw, by= "time")
HYCOM_nc <- mean(hycom_nc, by= "time") 
HYCOM_ne <- mean(hycom_ne, by= "time") 
HYCOM_sw <- mean(hycom_sw, by= "time") 
HYCOM_sc <- mean(hycom_sc, by= "time") 
HYCOM_se <- mean(hycom_se, by= "time") 

write.table(HYCOM_nw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_nw.csv",  row.names = FALSE, col.names = FALSE)
write.table(HYCOM_nc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_nc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(HYCOM_ne, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_ne.csv",  row.names = FALSE, col.names = FALSE)   
write.table(HYCOM_sw, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_sw.csv",  row.names = FALSE, col.names = FALSE)   
write.table(HYCOM_sc, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_sc.csv",  row.names = FALSE, col.names = FALSE)   
write.table(HYCOM_se, file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_se.csv",  row.names = FALSE, col.names = FALSE) 



