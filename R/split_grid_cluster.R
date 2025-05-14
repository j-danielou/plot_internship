library(gts)
library(ncdf4)
library(fields)
library(RColorBrewer)
library(dplyr)
library(lubridate)


masks = readRDS("C:/Users/jdanielou/Desktop/rds/masks_cluster.rds")
df_all = readRDS(("C:/Users/jdanielou/Desktop/rds/df_all.rds"))

coords_by_cluster = list()

for (i in 1:6) {

  cluster_mask = masks[[i]]
  
  coords_by_cluster[[paste0("cluster", i)]] = df_all[cluster_mask, c("lon", "lat")]
}

saveRDS(coords_by_cluster,"C:/Users/jdanielou/Desktop/rds/coord_by_cluster.rds")



split_grid_by_cluster = function(grid_path, coords_by_cluster, save_path = NULL) {
  library(dplyr)
  
  grid_df = readRDS(grid_path)
  
  grid_by_cluster = list()
  
  for (i in 1:6) {
    coords = coords_by_cluster[[paste0("cluster", i)]]
    
    grid_by_cluster[[paste0("cluster", i)]] = grid_df %>%
      inner_join(coords, by = c("lon", "lat"))
  }
  
  if (!is.null(save_path)) {
    saveRDS(grid_by_cluster, file = save_path)
  }
  
  return(grid_by_cluster)
}

split_grid_by_cluster(grid_path = "C:/Users/jdanielou/Desktop/rds/sst_oisst.rds", 
                      coords_by_cluster = coords_by_cluster, save_path = "C:/Users/jdanielou/Desktop/rds/oisst_by_cluster.rds")







