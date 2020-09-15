#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(raster)
library(tidyverse)
library(ggplot2)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"

#load data k_grid
load(paste0("Gridded_data/koala_raw_pankrclasses_", cell_area, ".Rdata"))
k_grid <- k_grid %>% st_transform(3577) #GDA94 Albers

#test data
#t <- k_grid %>% filter(cellid %in% c(1:90, 98, 203, 210:215)) %>% st_transform(3577)
#plot(t, col = sf.colors(12, categorical = TRUE))

####
####
#EDIT function to work with changes to pankr categorise

#Set up function
clusterFun <- function(data, datatitle, min_area_list, oupdir){
  df <- data.frame()
  for (i in 1:ncol(data)){
    if(i<ncol(data)){
      curr_scenario <- names(data)[i]
      cur_pols <- data %>% 
                dplyr::select(curr_scenario) %>% filter(curr_scenario==1) %>%
               # st_buffer() %>%
                st_union() %>%  #we then join adjacent polygons together
                st_cast("POLYGON") %>% st_sf() %>%
                mutate(area_ha = as.numeric(st_area(.)/10000)) 
  
      
        for(i in 1:length(min_area_list)){
          curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]])
      
          if(nrow(curr_filter)>0) {
            st_write(curr_filter, paste0(oupdir, datatitle, "_", curr_scenario, "_clusterthresh_", min_area_list[[i]], "_ha.gpkg")) 
            d <- curr_filter %>% st_drop_geometry() %>% summarise(pankr_type = datatitle,
                                                                  scenario = curr_scenario, 
                                                                  keep_polygons_bigger_than = min_area_list[[i]], 
                                                                  total_area_ha = sum(area_ha)) 
          } else {
            d <- data.frame(pankr_type = datatitle,
                            scenario = curr_scenario, 
                            keep_polygons_bigger_than = min_area_list[[i]], 
                            total_area_ha = 0)
        }
      df <- rbind(df, d)
      }

    } else if(i==ncol(data)){
        print("finished")
    }}
    write.csv(df, paste0(oupdir, "Cluster_threshold_sensitivity", datatitle, ".csv"))  
    return(df)
}  
  
#Run Clusters
d1 <- clusterFun(known_pankr, datatitle="Known", min_area_list = list(0, 100, 10000), oupdir = oupdir)



d_all <- rbind(d1, d2) 
write.csv(d_all, paste0(oupdir, "Cluster_threshold_sensitivity.csv"))

#Plot sensitivity to threshold
ggplot(d_all, aes(x=min_polygon_size, y=total_area_ha, col=curr_class)) +
  geom_line() +
  theme_minimal()

###END