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

####################
#Plot known2 and recovery2 scenario
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))

plotfun <- function(nscenarios, plottitle, sub, ...) {
  for (i in 1:nscenarios){
    load(paste0(oupdir, testid, "data/", plottitle, "_scenario_", i, "_clusterthresh_0ha.Rdata"))
    data <- curr_filter %>% mutate(plotid = 1)
    
    if(sub=="nsw"){
      p <- tm_shape(region, bbox=nsw_region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
        tm_shape(region) + tm_borders() +
        tm_layout(frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_nswe_scenario_", i, ".png"), height=1072, width=716)
    } else if(sub=="seq") {
      p <- tm_shape(region, bbox=seq_region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
        tm_shape(region) + tm_borders() + 
        tm_layout(legend.outside=TRUE, legend.outside.position="right", frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_seq_scenario_", i, ".png"), height=1072, width=1056)
    } else {
      p <- tm_shape(region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', labels=c("Meets criteria"), legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2]) +
        tm_shape(region) + tm_borders() +
        tm_layout(frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, ".png"), height=1920, width=1080)
    }  
  }} 


plotfun(nscenarios=10, plottitle="Known", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=10, plottitle="Known2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

plotfun(nscenarios=10, plottitle="Known", sub="nsw", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery", sub="nsw", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=10, plottitle="Known2", sub="nsw", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2",sub="nsw", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

plotfun(nscenarios=10, plottitle="Known", sub="seq", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery", sub="seq", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=10, plottitle="Known2", sub="seq", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(nscenarios=6, plottitle="Recovery2", sub="seq", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

####################
#Plot adaptation scenarios
####################################

plotfun <- function(dataname, plottitle, ...) {
  data <- load(paste0(oupdir, "Test1/koala_", dataname, "_raw_100ha.Rdata"))
  for(i in 1:ncol(data)){
    if(i<ncol(data)){
      colid = names(data)[i]
      p <- tm_shape(data) +
        tm_fill(col=colid, title=paste0(plottitle, ": Scenario ", i), legend.position=c("top", "right"), colorNA="grey90", ...) +
        tm_shape(region) + tm_borders() +
        tm_layout(frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/Test1/", plottitle, "_", colid, ".png"), height=1920, width=1080)
    } else {
      print("finished")
    }
  }}
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))

# plotfun("bushfire_pankr", plottitle="Bushfire refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun("drought_refugia", plottitle="Drought refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun("climate_refugia", plottitle="Climate refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun("climate_current", plottitle="Current climate", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)


####################
#Plot maps of NIKA comparing area thresholds
plotfun <- function(currscenario, plottitle, sub, ...) {
  load(paste0(oupdir, testid, "data/", plottitle, "_scenario_", currscenario, "_clusterthresh_0ha.Rdata"))
  if(sub=="nsw"){
    p <- tm_shape(region, bbox=nsw_region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() +
      tm_layout(frame=FALSE)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_area_thresholds_nswe_scenario_", currscenario, ".png"), height=1072, width=716)
  } else if(sub=="seq") {
    p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() + 
      tm_layout(legend.outside=TRUE, legend.outside.position="right", frame=FALSE)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_area_thresholds_seq_scenario_", currscenario, ".png"), height=1072, width=1056)
  } else {
    p <- tm_shape(region) + 
      tm_fill(palette="grey90") +
      tm_shape(curr_filter) +
      tm_fill(col='area_ha', title=paste0("Area (ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() +
      tm_layout(frame=FALSE)
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


