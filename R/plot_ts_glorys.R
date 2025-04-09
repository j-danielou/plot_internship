source("function_plot_ts.R")
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_mhw.csv")
glorys = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/glorys/glorys_mhw.csv")

oisst=oisst$V1
glorys = glorys$V1
time = 1:length(glorys)

x11(width = 18, height = 12)
plot_diff(time = time, x = glorys, y = oisst, Title ="Time-Series of OISST and GLORYS12v1 (1993-01 / 2021-12)", 
          Legend = c("SST GLORYS12v1", "SST OISST", "Difference (GLORYS - OISST)"))

dev.copy(jpeg,file="C:/Users/jdanielou/Desktop/plots_internship/plot/ts_oisst_glorys_mhw.jpeg", width = 15.5, height = 10, units = "in", res = 300)
dev.off()
