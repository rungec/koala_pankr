#This file plots different scenarios for types of important koala areas
#follows on from PANKR_categorise.r and PANKR_clustering.r

library(sf)
library(tidyverse)
library(ggplot2)
library(tmap)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
oupdir <- "Clusters/"
testid <- "Test5/"
currkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")

#Function for loading
#loads an RData file, and allows it to be assigned a variable name
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

####################
#Plot planning units that meet criteria in each scenario
####################
#Load shapefiles
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))

#FUNCTION
plotfun <- function(nscenarios, plottitle, sub, ...) {
  for (i in nscenarios){
    data <- loadRData(paste0(oupdir, testid, "data/", plottitle, "_scenario_", i, "_clusterthresh_pu_0ha.Rdata"))
    data <- data %>% mutate(plotid = 1)
    
    if("nsw" %in% sub){
      p <- tm_shape(region, bbox=nsw_region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2], ...) +
        tm_shape(region) + tm_borders() +
        tm_layout(frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, "_nswe.png"), height=1072, width=716)
    } 
    if("seq" %in% sub) {
      p <- tm_shape(region, bbox=seq_region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', colorNA="grey90", palette=greypal[2], ...) +
        tm_shape(region) + tm_borders() + 
        tm_layout(legend.outside=TRUE, legend.outside.position="right", frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, "_seq.png"), height=1072, width=1056)
    } 
    if ("wholerange" %in% sub) {
      p <- tm_shape(region) + 
        tm_fill(palette="grey90") +
        tm_shape(data) +
        tm_fill(col='plotid', title=paste0(plottitle, ": Scenario ", i), style='cat', legend.position=c("top", "right"), colorNA="grey90", palette=greypal[2], ...) +
        tm_shape(region) + tm_borders() +
        tm_layout(frame=FALSE)
      tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, ".png"), height=1920, width=1080)
    }  
  }} 


plotfun(nscenarios=1:12, plottitle="Climate", sub="wholerange", labels=c("Climate suitable"), showNA=FALSE)
plotfun(nscenarios=1:10, plottitle="Known", sub="wholerange", labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:10, plottitle="Known3", sub="wholerange", labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:8, plottitle="Lost", sub="wholerange", labels=c("Vulnerable"), showNA=FALSE)
plotfun(nscenarios=1:8, plottitle="Lost3", sub="wholerange", labels=c("Vulnerable"), showNA=FALSE)
plotfun(nscenarios=1:4, plottitle="Habitat", sub="wholerange", labels=c("Vulnerable"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Monitoring", sub="wholerange", labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Recovery", sub="wholerange", labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Recovery2", sub="wholerange", labels=c("Meets criteria"), showNA=FALSE)

plotfun(nscenarios=1:10, plottitle="Known2", sub=c("nsw", "seq", "wholerange"), labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:10, plottitle="Known3", sub=c("nsw", "seq", "wholerange"), labels=c("Meets criteria"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Lost2", sub="wholerange", labels=c("Vulnerable"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Lost", sub="wholerange", labels=c("Vulnerable"), showNA=FALSE)


####################
#Plot scenarios comparing area thresholds
####################
#Load shapefiles
greypal <- c("white", RColorBrewer::brewer.pal(5, "YlGnBu")[2:5])
# region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
# nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
# nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
# seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
# seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))

#FUNCTION
plotfun <- function(nscenarios, plottitle, sub, area_type, ...) {
  for (i in nscenarios){
    data <- loadRData(paste0(oupdir, testid, "data/", plottitle, "_scenario_", i, "_clusterthresh_", area_type, "_0ha.Rdata"))

  if("nsw" %in% sub){
    p <- tm_shape(region, bbox=nsw_region) + 
      tm_fill(palette="grey90") +
      tm_shape(data) +
      tm_fill(col='area_col', title=paste0("Area", area_type, "(ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() +
      tm_layout(frame=FALSE)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, "_nswe_area_threshold_", area_type, ".png"), height=1072, width=716)
  } 
    if("seq" %in% sub) {
    p <- tm_shape(region, bbox=seq_region) + 
      tm_fill(palette="grey90") +
      tm_shape(data) +
      tm_fill(col='area_col', title=paste0("Area", area_type, "(ha)"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() + 
      tm_layout(legend.outside=TRUE, legend.outside.position="right", frame=FALSE)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, "_seq_area_threshold_", area_type, ".png"), height=1072, width=1056)
  } 
    if("wholerange" %in% sub) {
    p <- tm_shape(region) + 
      tm_fill(palette="grey90") +
      tm_shape(data) +
      tm_fill(col='area_col', title=paste0("Area", area_type, "(ha)"), legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() +
      tm_layout(frame=FALSE)
    tmap_save(p, paste0("figures/scenarios/", testid, plottitle, "_scenario_", i, "_area_threshold_", area_type, ".png"), height=1920, width=1080)
  }  
} }

plotfun(nscenarios=1:10, plottitle="Known", sub=c("nsw", "seq"), area_type="habitat", breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k"), palette=greypal, showNA=FALSE)
plotfun(nscenarios=1:8, plottitle="Lost", sub="wholerange", area_type="habitat", breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k"), palette=greypal, showNA=FALSE)
plotfun(nscenarios=1:8, plottitle="Lost3", sub="wholerange", area_type="habitat", breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k"), palette=greypal, showNA=FALSE)


plotfun(nscenarios=c(5, 9, 10), plottitle="Known", sub=c("wholerange", "nsw", "seq"), area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known", sub=c("wholerange", "nsw", "seq"), area_type="pu", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known2", sub=c("wholerange", "nsw", "seq"), area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known2", sub=c("wholerange", "nsw", "seq"), area_type="pu", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Lost3", sub="wholerange", area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Monitoring", sub="wholerange", area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)

plotfun(nscenarios=c(5, 9, 10), plottitle="Known", sub=c("wholerange", "nsw", "seq"), area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known", sub=c("wholerange", "nsw", "seq"), area_type="pu", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known2", sub=c("wholerange", "nsw", "seq"), area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=c(5, 9, 10), plottitle="Known2", sub=c("wholerange", "nsw", "seq"), area_type="pu", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Lost3", sub="wholerange", area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)
plotfun(nscenarios=1:6, plottitle="Monitoring", sub="wholerange", area_type="habitat", palette=greypal, breaks=c(0, 1000, 10000, 100000, 100000000), labels=c("<1k", "1k-10k","10k-100k", ">100k"), showNA=FALSE)

##########################
#Plot adaptation scenarios
##########################

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


###END


