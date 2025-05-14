library(dplyr)
library(lubridate)

compute_anomalies_by_cluster = function(cluster_data_list, model) {
  
  anomalies_list = list()
  
  for (i in 1:6) {
    df = cluster_data_list[[paste0("cluster", i)]]
    
    # Moyenne spatiale pour chaque date
    daily_mean = df %>%
      group_by(time) %>%
      summarise(sst_mean = mean(sst, na.rm = TRUE), .groups = "drop")
    
    
    daily_mean$month = month(daily_mean$time, label = TRUE)
    
    # Calcul de la climatologie mensuelle
    climatology = daily_mean %>%
      group_by(month) %>%
      summarise(climatology = mean(sst_mean, na.rm = TRUE), .groups = "drop")
    
    
    anomalies = daily_mean %>%
      left_join(climatology, by = "month") %>%
      mutate(anomaly = sst_mean - climatology)
    
    #
    anomalies_list[[paste0("cluster", i)]] = anomalies
  }
  
  # Sauvegarde sur disque
  saveRDS(anomalies_list, file = paste0("C:/Users/jdanielou/Desktop/rds/", model, "_anomalies.rds"))
  
  return(anomalies_list)
}

model_by_cluster = readRDS("C:/Users/jdanielou/Desktop/rds/hycom_by_cluster.rds")
compute_anomalies_by_cluster(cluster_data_list = model_by_cluster, model = "hycom")


model_anomalis = readRDS(file = "C:/Users/jdanielou/Desktop/rds/hycom_anomalies.rds")


