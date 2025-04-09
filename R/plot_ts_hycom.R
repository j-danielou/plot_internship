source("function_plot_ts.R")
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_mhw.csv")
hycom = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/hycom/hycom_mhw.csv")

oisst=oisst$V1
oisst = oisst[13:276]
hycom = hycom$V1
time = 1:length(hycom)

x11(width = 18, height = 12)
plot_diff(time = time, x = hycom, y = oisst, Title ="Time-Series of OISST and HYCOM 3.1 (1994-01 / 2015-12)", 
          Legend = c("SST HYCOM 3.1", "SST OISST", "Difference (HYCOM - OISST)"))

dev.copy(jpeg,file="C:/Users/jdanielou/Desktop/plots_internship/plot/ts_oisst_hycom_mhw.jpeg", width = 15.5, height = 10, units = "in", res = 300)
dev.off()
