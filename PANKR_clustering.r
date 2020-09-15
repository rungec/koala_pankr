#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(raster)
library(tidyverse)
library(ggplot2)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"

#####################
#load data k_grid
filelist <- list.files("Gridded_data/", pattern="_raw_", full.names=TRUE)
for(currfile in filelist){
  load(currfile)
}

#####################
#Set up function
clusterFun <- function(data, datatitle, min_area_list, oupdir){
  df <- data.frame()
  data <- data %>% st_transform(3577) #GDA94 Albers
  for (i in 1:ncol(data)){
    if(i<ncol(data)){
      curr_scenario <- names(data)[i]
      curr_pols <- data %>% 
                dplyr::select(curr_scenario) %>% filter(.data[[curr_scenario]]==1) %>%
               # st_buffer() %>%
                st_union() %>%  #we then join adjacent polygons together
                st_cast("POLYGON") %>% st_sf() %>%
                mutate(area_ha = as.numeric(st_area(.)/10000)) 
  
      for(i in 1:length(min_area_list)){
          curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]])
      
          if(nrow(curr_filter)>0) {
            st_write(curr_filter, paste0(oupdir, datatitle, "_", curr_scenario, "_clusterthresh_", min_area_list[[i]], "ha.gpkg")) 
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

#####################  
#Run Clusters
d1 <- clusterFun(known_pankr, datatitle="Known", min_area_list = list(0, 100, 1000, 10000), oupdir = oupdir)
d2 <- clusterFun(recovery_pankr, datatitle="Recovery", min_area_list = list(0, 100, 1000, 10000), oupdir = oupdir)
d3 <- clusterFun(bushfire_pankr, datatitle="Bushfire", min_area_list = list(0, 100, 1000, 10000), oupdir = oupdir)
d4 <- clusterFun(drought_refugia, datatitle="Drought", min_area_list = list(0, 100, 1000, 10000), oupdir = oupdir)
d5 <- clusterFun(climate_refugia, datatitle="Climate", min_area_list = list(0, 100, 1000, 10000), oupdir = oupdir)

d_all <- rbind(d1, d2, d3, d4, d5) 
d_all_spread <- d_all %>% pivot_wider(names_from = keep_polygons_bigger_than, values_from = total_area_ha)
write.csv(d_all_spread, paste0(oupdir, "Cluster_threshold_sensitivity.csv"))

#####################
#Plot sensitivity to threshold
p <- ggplot(d_all, aes(x=keep_polygons_bigger_than +1, y=total_area_ha, col=scenario)) +
  geom_step() + 
  scale_x_log10() +
  xlab("Drop polygons smaller than") + ylab("Prioritised area (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap('pankr_type', scales="free_y")
ggsave(paste0(oupdir, "Cluster_threshold_sensitivity.png"), p)


###END