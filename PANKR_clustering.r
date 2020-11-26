#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(tidyverse)
library(ggplot2)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"
testid <- "Test5/"
currkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")
histkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_1970to2000_1kmres_noDup.shp")

#####################
#load and extract koala occurrence 
current_koala <- st_read(currkoaladir) %>% 
  st_transform(3577) %>%
  st_buffer(1000) %>% st_geometry()

#loads an RData file, and allows it to be assigned a variable name
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#####################
#Set up function
#the function dissolves connected planning units (that meet NIKA classification)
#it then drops clusters smaller than a given area, define in min_area_list
#this function can either drop clusters based on the area of habitat in a cluster or the area of planning units in a cluster
#use area_type="habitat" or "pu"
clusterFun <- function(dataname, min_area_list, oupdir, area_type, ...){
  #load data
  data <- loadRData(paste0("Gridded_data/intermediate5/koala_", dataname, "_raw_100ha.Rdata"))
  data <- data %>% st_transform(3577) #GDA94 Albers
  #set up variables
  datatitle <- str_to_title(strsplit(dataname, "_")[[1]][1])
  if("ha" %in% names(data)){
    nscenarios=(ncol(data)-1)/2 
  } else {
    nscenarios=(ncol(data)-1)
  }
  
  df <- data.frame()
  
  #iterate over each scenario
  for (i in 1:nscenarios){
      curr_scenario <- names(data)[i]
      
      #select the type of area we want to base our decisions on (habitat or planning units)
      if(area_type == "habitat") {
        
        area_scenario <- paste0(curr_scenario, "_ha")
        #union and dissolve the planning units, selecting only the rows in the current scenario
        print("starting dissolve")
        curr_pols <- data %>% 
          dplyr::select(all_of(c(curr_scenario, area_scenario))) %>% filter(.data[[curr_scenario]]==1) 
        curr_union <- curr_pols %>%  st_union() %>%  #we then join adjacent polygons together
          st_cast("POLYGON") 
        
        clumps <- unlist(st_intersects(curr_pols, curr_union))
        #make a table listing the pus in the current scenario and which clump they fall within
        curr_pols <- cbind(curr_pols, clumps)
        curr_pols <- curr_pols %>% mutate(curr_koala = lengths(st_intersects(curr_pols, current_koala, sparse=TRUE)))
        
        #group by clumps and summarise area for each clump
        print("starting clump")
        clump_pols <- curr_pols %>% group_by(clumps) %>% summarise(area_col = sum(.data[[area_scenario]], na.rm=TRUE),
                                                                   curr_koala = sum(curr_koala)) %>% ungroup()
        
      } else if (area_type == "pu") {
        
        #union and dissolve the planning units, selecting only the rows in the current scenario
        print("starting dissolve")
        curr_pols <- data %>% 
                  dplyr::select(all_of(c(curr_scenario))) %>% filter(.data[[curr_scenario]]==1) 
        curr_union <- curr_pols %>%  st_union() %>%  #we then join adjacent polygons together
                  st_cast("POLYGON") 
        clumps <- unlist(st_intersects(curr_pols, curr_union))
        #make a table listing the pus in the current scenario and which clump they fall within
        curr_pols <- cbind(curr_pols, clumps)
        
        #calculate areas
        curr_pols <- curr_pols %>% mutate(area_ha = as.numeric(st_area(.)/10000))
        curr_pols <- curr_pols %>% mutate(curr_koala = lengths(st_intersects(curr_pols, current_koala, sparse=TRUE)))
          
        #group by clumps and summarise area for each clump
        print("starting clump")
        clump_pols <- curr_pols %>% group_by(clumps) %>% summarise(area_col = sum(area_ha, na.rm=TRUE),
                                                                     curr_koala = sum(curr_koala)) %>% ungroup()
      }
      
      #drop clumps smaller than min_area
      print("starting drop clumps")
      for(a in 1:length(min_area_list)){
        if(datatitle %in% c("Known2", "Lost2")){
          curr_filter <- clump_pols %>% filter(area_col > min_area_list[[a]] & curr_koala > 0)
        } else if(datatitle %in% c("Recovery2")){
          curr_filter <- filter(clump_pols, area_col > min_area_list[[a]] & curr_koala == 0)
        } else { #for known and known3 scenarios
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

    print(paste0("finished scenario ", i))
    }
    write.csv(df, paste0(oupdir, testid, "Cluster_threshold_sensitivity_", datatitle, "_", area_type, ".csv"))  
    return(df)
}  

#####################  
#Run Clusters, dropping clusters by the area of planning units
#d1a <- clusterFun('known_pankr', min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
#d1b <- clusterFun('known2_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
#d1c <- clusterFun('known3_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
d2a <- clusterFun('recovery_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
d2b <- clusterFun('recovery2_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
#d3a <- clusterFun('lost_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
#d3b <- clusterFun('lost2_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
#d3c <- clusterFun('lost3_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "pu", oupdir = oupdir)
# d3 <- clusterFun('bushfire_pankr', datatitle="Bushfire", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)
# d4 <- clusterFun('drought_refugia', datatitle="Drought", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)
d5 <- clusterFun('climate_suitable', datatitle="Climate", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)
d6 <- clusterFun('monitoring_pankr', datatitle="Monitoring", min_area_list = list(0, 1000, 10000), area_type = "pu", oupdir = oupdir)

d_all_p <- rbind(d2a, d2b, d5, d6) 
#d_all_p <- rbind(d1a, d1b, d1c, d2a, d2b, d3a, d3b, d3c, d4, d5, d6) 
d_all_spread <- d_all_p %>% pivot_wider(names_from = keep_polygons_bigger_than, values_from = total_area_ha)
write.csv(d_all_spread, paste0(oupdir, testid, "Cluster_threshold_sensitivity_pu.csv"))

#Run clusters, dropping clusters by the area of koala habitat
#d1a <- clusterFun('known_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)
d1b <- clusterFun('known2_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)
d1c <- clusterFun('known3_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)
#d3a <- clusterFun('lost_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)
d3b <- clusterFun('lost2_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)
d3c <- clusterFun('lost3_pankr',  min_area_list = list(0, 1000, 10000, 100000), area_type = "habitat", oupdir = oupdir)


d_all_h <- rbind(d1a, d1b, d1c, d3a, d3b, d3c) 
d_all_spread <- d_all_h %>% pivot_wider(names_from = keep_polygons_bigger_than, values_from = total_area_ha)
write.csv(d_all_spread, paste0(oupdir, testid, "Cluster_threshold_sensitivity_habitat.csv"))

d_all <- rbind(d1a, d2a, d2b, d3a, d3c, d5) 
d_all_spread <- d_all %>% pivot_wider(names_from = keep_polygons_bigger_than, values_from = total_area_ha)
write.csv(d_all_spread, paste0(oupdir, testid, "Cluster_threshold_sensitivity_tmp.csv"))

#####################
#Plot sensitivity to threshold
p <- ggplot(d_all_p, aes(x=keep_polygons_bigger_than +1, y=total_area_ha, col=scenario)) +
  geom_step() + 
  scale_x_log10() +
  xlab("Drop polygons smaller than") + ylab("Prioritised PU area (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap('pankr_type', scales="free_y")
ggsave(paste0(oupdir, testid, "Cluster_threshold_sensitivity_pu.png"), p)

p <- ggplot(d_all_h, aes(x=keep_polygons_bigger_than +1, y=total_area_ha, col=scenario)) +
  geom_step() + 
  scale_x_log10() +
  xlab("Drop polygons smaller than") + ylab("Prioritised habitat area (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap('pankr_type', scales="free_y")
ggsave(paste0(oupdir, testid, "Cluster_threshold_sensitivity_habitat.png"), p)

###END