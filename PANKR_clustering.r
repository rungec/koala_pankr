#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(tidyverse)
library(ggplot2)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"
testid <- "Test4/"
currkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")

#####################
#load data k_grid
filelist <- list.files("Gridded_data/intermediate4/", pattern="_raw_100ha.Rdata", full.names=TRUE)
for(currfile in filelist){
  load(currfile)
}

#load and extract koala occurrence 
current_koala <- st_read(currkoaladir) %>% 
  st_transform(3577) %>%
  st_buffer(1000) %>% st_geometry()

#####################
#Set up function
#the function dissolves connected planning units (that meet NIKA classification)
#it then drops clusters smaller than a given area, define in min_area_list
#this function can either drop clusters based on the area of habitat in a cluster or the area of planning units in a cluster
#use area_type="habitat" or "pu"
clusterFun <- function(data, datatitle, min_area_list, oupdir, area_type, ...){
  df <- data.frame()
  nscenarios=(ncol(data)-1)/2
  data <- data %>% st_transform(3577) #GDA94 Albers
  for (i in 1:nscenarios){
      curr_scenario <- names(data)[i]
      area_scenario <- paste0(curr_scenario, "_ha")
      
      #union and dissolve the planning units
      curr_pols <- data %>% 
                dplyr::select(all_of(c(curr_scenario, area_scenario))) %>% filter(.data[[curr_scenario]]==1) 
      curr_union <- curr_pols %>%  st_union() %>%  #we then join adjacent polygons together
                st_cast("POLYGON") 
      clumps <- unlist(st_intersects(curr_pols, curr_union))
      curr_pols <- cbind(curr_pols, clumps)
      
      #calculate areas
      curr_pols <- curr_pols %>% mutate(area_ha = as.numeric(st_area(.)/10000))
      curr_pols <- curr_pols %>% mutate(current_koala = lengths(st_intersects(curr_pols, current_koala, sparse=TRUE)))
      
      #group by clumps and summarise area
      clump_pols <- curr_pols %>% group_by(clumps) %>% summarise(area_habitat_ha = sum(.data[[area_scenario]], na.rm=TRUE),
                                                         area_pu_ha = sum(area_ha, na.rm=TRUE),
                                                         current_koala = sum(current_koala)) %>% ungroup()
      
      #select the type of area we want to base our decisions on (habitat or planning units)
      if(area_type == "habitat") {
        clump_pols <- clump_pols %>% mutate(area_col = area_habitat_ha)
      } else if (area_type == "pu"){
        clump_pols <- clump_pols %>% mutate(area_col = area_pu_ha)
      }
      
      #drop clumps smaller than min_area
      for(a in 1:length(min_area_list)){
        if(datatitle %in% c("Known2")){
          curr_filter <- clump_pols %>% filter(area_col > min_area_list[[a]] & current_koala > 0)
        } else if(datatitle %in% c("Recovery2")){
          curr_filter <- filter(clump_pols, area_col > min_area_list[[a]] & current_koala == 0)
        } else {
          curr_filter <- filter(clump_pols, area_col > min_area_list[[a]])
        }
          if(nrow(curr_filter)>0) {
            if(min_area_list[[a]]==0) {
            save(curr_filter, file=paste0(oupdir, testid, datatitle, "_", curr_scenario, "_clusterthresh_", area_type, "_", min_area_list[[a]], "ha.Rdata")) 
            #st_write(curr_filter, paste0(oupdir, testid, datatitle, "_", curr_scenario, "_clusterthresh_", area_type, "_", min_area_list[[a]], "ha.gpkg")) 
            }
              d <- curr_filter %>% st_drop_geometry() %>% summarise(pankr_type = datatitle,
                                                                  scenario = curr_scenario, 
                                                                  keep_polygons_bigger_than = min_area_list[[a]], 
                                                                  total_area_ha = sum(area_col)) 
          } else {
            d <- data.frame(pankr_type = datatitle,
                            scenario = curr_scenario, 
                            keep_polygons_bigger_than = min_area_list[[a]], 
                            total_area_ha = 0)
        }
      df <- rbind(df, d)
      }

    print("finished")
    }
    write.csv(df, paste0(oupdir, testid, "Cluster_threshold_sensitivity_", datatitle, "_", area_type, ".csv"))  
    return(df)
}  

#####################  
#Run Clusters
d1 <- clusterFun(known_pankr, datatitle="Known", min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
d1b <- clusterFun(known2_pankr, datatitle="Known2", min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
d2 <- clusterFun(recovery_pankr, datatitle="Recovery", min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
d2b <- clusterFun(recovery2_pankr, datatitle="Recovery2", min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
# d3 <- clusterFun(bushfire_pankr, datatitle="Bushfire", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)
# d4 <- clusterFun(drought_refugia, datatitle="Drought", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)
# d5 <- clusterFun(climate_refugia, datatitle="Climate", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)

#d_all <- rbind(d1, d2, d3, d4, d5, d1b, d2b) 
d_all <- rbind(d1, d2, d1b, d2b) 
d_all_spread <- d_all %>% pivot_wider(names_from = keep_polygons_bigger_than, values_from = total_area_ha)
write.csv(d_all_spread, paste0(oupdir, testid, "Cluster_threshold_sensitivity.csv"))

#####################
#Plot sensitivity to threshold
p <- ggplot(d_all, aes(x=keep_polygons_bigger_than +1, y=total_area_ha, col=scenario)) +
  geom_step() + 
  scale_x_log10() +
  xlab("Drop polygons smaller than") + ylab("Prioritised area (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap('pankr_type', scales="free_y")
ggsave(paste0(oupdir, testid, "Cluster_threshold_sensitivity.png"), p)

###END