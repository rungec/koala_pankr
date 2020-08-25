#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(raster)
library(tidyverse)
library(ggplot2)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters_data/"

#load data k_grid
load(paste0("Gridded_data/koala_raw_pankrclasses_", cell_area, ".Rdata"))
k_grid <- k_grid %>% st_transform(3577) #GDA94 Albers

#test data
#t <- k_grid %>% filter(cellid %in% c(1:90, 98, 203, 210:215)) %>% st_transform(3577)
#plot(t, col = sf.colors(12, categorical = TRUE))

#Set up function
clusterFun <- function(k_grid, curr_class, min_area_list, oupdir){
  
  cur_pols <- k_grid %>% 
                filter(pankr_class %in% curr_class) %>% #First we select polygons of a given type
                st_union() %>%  #we then join adjacent polygons together
                st_cast("POLYGON") %>% st_sf() %>%
                mutate(area_ha = as.numeric(st_area(.)/10000)) 
  
  df <- data.frame()
  for(i in 1:length(min_area_list)){
    curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]])
      
      if(nrow(curr_filter)>0) {
        st_write(curr_filter, paste0(oupdir, "pankr_", curr_class, "_clusterthresh_", min_area_list[[i]], "_ha.shp"), append=FALSE) 
        d <- curr_filter %>% summarise(keep_polygons_bigger_than = min_area_list[[i]], 
                                       total_area_ha = sum(area_ha)) %>% st_drop_geometry()
      } else {
      d <- data.frame(class = curr_class,
                      min_polygon_size = min_area_list[[i]], 
                      total_area_ha = 0)
      }
    df <- rbind(df, d)
  }
   
  return(df)
}  
  
#Run Clusters
d1 <- clusterFun(k_grid, curr_class=1, min_area_list = list(0, 100, 10000), oupdir = oupdir)
d2 <- clusterFun(k_grid, curr_class=2, min_area_list = list(0, 100, 10000), oupdir = oupdir)

d_all <- rbind(d1, d2) 
write.csv(d_all, paste0(oupdir, "Cluster_threshold_sensitivity.csv"))

#Plot sensitivity to threshold
ggplot(d_all, aes(x=min_polygon_size, y=total_area_ha, col=curr_class)) +
  geom_line() +
  theme_minimal()

###END