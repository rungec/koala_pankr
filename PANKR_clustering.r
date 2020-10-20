#This file takes a .shp that has columns indicating which type of PANKR each polygon is categorised as
#It clumps polygons of the same class, and removes isolated polygons and clumps that are too small
#follows on from PANKR_categorise.r

library(sf)
library(tidyverse)
library(ggplot2)
library(tmap)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"
testid <- "Test3/"
currkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")

#####################
#load data k_grid
filelist <- list.files("Gridded_data/intermediate3/", pattern="_raw_100ha.Rdata", full.names=TRUE)
for(currfile in filelist){
  load(currfile)
}

#load and extract koala occurrence 
current_koala <- st_read(currkoaladir) %>% 
  st_transform(3577) %>%
  st_buffer(1000) %>% st_geometry()

#####################
#Set up function
clusterFun <- function(data, datatitle, min_area_list, oupdir, ...){
  df <- data.frame()
  data <- data %>% st_transform(3577) #GDA94 Albers
  for (i in 1:ncol(data)){
    if(i<ncol(data)){
      curr_scenario <- names(data)[i]
      curr_pols <- data %>% 
                dplyr::select(all_of(curr_scenario)) %>% filter(.data[[curr_scenario]]==1) %>%
               # st_buffer() %>%
                st_union() %>%  #we then join adjacent polygons together
                st_cast("POLYGON") %>% st_sf() %>%
                mutate(area_ha = as.numeric(st_area(.)/10000))
      curr_pols <- curr_pols %>% mutate(current_koala = lengths(st_intersects(curr_pols, current_koala, sparse=TRUE)))
  
      for(i in 1:length(min_area_list)){
        if(datatitle %in% c("Known2")){
          curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]] & current_koala > 0)
        } else if(datatitle %in% c("Recovery2")){
          curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]] & current_koala == 0)
        } else {
          curr_filter <- filter(curr_pols, area_ha > min_area_list[[i]])
        }
          if(nrow(curr_filter)>0) {
            if(min_area_list[[i]]==0) {
            save(curr_filter, file=paste0(oupdir, testid, datatitle, "_", curr_scenario, "_clusterthresh_", min_area_list[[i]], "ha.Rdata")) 
            #st_write(curr_filter, paste0(oupdir, datatitle, "_", curr_scenario, "_clusterthresh_", min_area_list[[i]], "ha.gpkg")) 
            }
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
    write.csv(df, paste0(oupdir, testid, "Cluster_threshold_sensitivity", datatitle, ".csv"))  
    return(df)
}  

#####################  
#Run Clusters
d1 <- clusterFun(known_pankr, datatitle="Known", min_area_list = list(0, 1000, 10000, 100000), oupdir = oupdir)
d1b <- clusterFun(known2_pankr, datatitle="Known2", min_area_list = list(0, 1000, 10000, 100000), oupdir = oupdir)
d2 <- clusterFun(recovery_pankr, datatitle="Recovery", min_area_list = list(0, 1000, 10000, 100000), oupdir = oupdir)
d2b <- clusterFun(recovery2_pankr, datatitle="Recovery2", min_area_list = list(0, 1000, 10000, 100000), oupdir = oupdir)
# d3 <- clusterFun(bushfire_pankr, datatitle="Bushfire", min_area_list = list(0, 1000, 10000), oupdir = oupdir)
# d4 <- clusterFun(drought_refugia, datatitle="Drought", min_area_list = list(0, 1000, 10000), oupdir = oupdir)
# d5 <- clusterFun(climate_refugia, datatitle="Climate", min_area_list = list(0, 1000, 10000), oupdir = oupdir)

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


####################
#Plot known2 and recovery2 scenario
plotfun <- function(nscenarios, plottitle, ...) {
  for (i in 1:nscenarios){
    load(file=paste0(oupdir, testid, plottitle, "_scenario_", i, "_clusterthresh_0ha.Rdata"))
    data <- curr_filter %>% mutate(plotid = 1)
    p <- tm_shape(region) + 
    #p <- tm_shape(region, bbox=nsw_region) + 
    #p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette=greypal[1]) +
      tm_shape(data) +
      tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
      tm_shape(region) + tm_borders()
    #tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_nswe_scenario_", i, ".png"), height=1920, width=1080)
    #tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_seq_scenario_", i, ".png"), height=1920, width=1080)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, ".png"), height=1920, width=1080)
  } }
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
# nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
# nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
# seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
# seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))


plotfun(nscenarios=10, plottitle="Known2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

####################
#Plot scenarios for NSW
plotfun <- function(nscenarios, plottitle, ...) {
  for (i in 1:nscenarios){
    load(file=paste0(oupdir, testid, plottitle, "_scenario_", i, "_clusterthresh_0ha.Rdata"))
    data <- curr_filter %>% mutate(plotid = 1)
    p <- tm_shape(region, bbox=nsw_region) + 
      #p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette=greypal[1]) +
      tm_shape(data) +
      tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
      tm_shape(region) + tm_borders()
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_nswe_scenario_", i, ".png"), height=1920, width=1080)
    #tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_seq_scenario_", i, ".png"), height=1920, width=1080)
     } }
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
# seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
# seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))


plotfun(nscenarios=10, plottitle="Known", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=10, plottitle="Known2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

####################
#Plot scenarios for NSW
plotfun <- function(nscenarios, plottitle, ...) {
  for (i in 1:nscenarios){
    load(file=paste0(oupdir, testid, plottitle, "_scenario_", i, "_clusterthresh_0ha.Rdata"))
    data <- curr_filter %>% mutate(plotid = 1)
    #p <- tm_shape(region, bbox=nsw_region) + 
    p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette=greypal[1]) +
      tm_shape(data) +
      tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
      tm_shape(region) + tm_borders()
    #tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_nswe_scenario_", i, ".png"), height=1920, width=1080)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_seq_scenario_", i, ".png"), height=1920, width=1080)
  } }
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
#nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
#nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
 seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
 seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))


plotfun(nscenarios=10, plottitle="Known", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=10, plottitle="Known2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)


####################
#Plot maps of NIKA comparing area thresholds
plotfun <- function(currscenario, plottitle, sub, ...) {
    load(paste0(oupdir, testid, "data/", plottitle, "_scenario_", currscenario, "_clusterthresh_0ha.Rdata"))
  if(sub=="nsw"){
    p <- tm_shape(region, bbox=nsw_region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() 
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_area_thresholds_nswe_scenario_", currscenario, ".png"), height=1620, width=1080)
   } else if(sub=="seq") {
    p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() + 
      tm_layout(legend.outside=TRUE, legend.outside.position="right")
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_area_thresholds_seq_scenario_", currscenario, ".png"), height=1420, width=1080)
  } else {
    p <- tm_shape(region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() 
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_area_thresholds_scenario_", currscenario, ".png"), height=1920, width=1080)
  }  
} 
greypal <- c("white", RColorBrewer::brewer.pal(5, "YlGnBu")[2:5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))


plotfun(currscenario=5, plottitle="Known", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=5, plottitle="Known", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=5, plottitle="Known", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=5, plottitle="Known2", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=5, plottitle="Known2", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=5, plottitle="Known2", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)

plotfun(currscenario=9, plottitle="Known", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=9, plottitle="Known", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=9, plottitle="Known", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=9, plottitle="Known2", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=9, plottitle="Known2", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=9, plottitle="Known2", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known2", sub="NA", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known2", sub="nsw", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(currscenario=10, plottitle="Known2", sub="seq", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)




###END