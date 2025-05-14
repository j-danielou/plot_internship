compute_monthly_climatology_by_cluster = function(cluster_data_list, model){
  
  climatologies = list()
  
  for (i in 1:6) {
    df = cluster_data_list[[paste0("cluster", i)]]
    
    # Moyenne spatiale journalière
    daily_mean = df %>%
      group_by(time) %>%
      summarise(sst_mean = mean(sst, na.rm = TRUE), .groups = "drop")
    
    # Ajouter le mois
    daily_mean$month = month(daily_mean$time, label = TRUE)
    
    # Climatologie mensuelle
    clim = daily_mean %>%
      group_by(month) %>%
      summarise(climatology = mean(sst_mean, na.rm = TRUE), .groups = "drop")
    
    climatologies[[paste0("cluster", i)]] = clim
    saveRDS(climatologies, file = paste0("C:/Users/jdanielou/Desktop/rds/", model, "_climatologies.rds"))
  }
  
  return(climatologies)
}

glorys_by_cluster = readRDS("C:/Users/jdanielou/Desktop/rds/glorys_by_cluster.rds")
compute_monthly_climatology_by_cluster(cluster_data_list = glorys_by_cluster, model = "glorys")

