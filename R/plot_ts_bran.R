source("function_plot_ts.R")
oisst = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/oisst/oisst_mhw.csv")
bran = read.table(file = "C:/Users/jdanielou/Desktop/plots_internship/ts_csv/bran/bran_mhw.csv")

oisst=oisst$V1
bran = bran$V1
time = 1:length(bran)

x11(width = 18, height = 12)
plot_diff(time = time, x = bran, y = oisst, Title ="Time-Series of OISST and BRAN20 (1993-01 / 2021-12)", 
          Legend = c("SST BRAN2020", "SST OISST", "Difference (BRAN - OISST)"))

dev.copy(jpeg,file="C:/Users/jdanielou/Desktop/plots_internship/plot/ts_oisst_bran_mhw.jpeg", width = 15.5, height = 10, units = "in", res = 300)
dev.off()
