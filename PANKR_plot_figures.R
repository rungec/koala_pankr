#This file plots figures for NESP4.4.12 final report
#follows on from PANKR_categorise.r and PANKR_clustering.r

library(sf)
library(tidyverse)
library(ggplot2)
library(tmap)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
inpdir <- "Clusters/Test5/"
oupdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Reports/Report figures/"
shpdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Storymap/shapefiles/"
currkoaladir <- paste0(dirname(getwd()), "/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")

#Function for loading
#loads an RData file, and allows it to be assigned a variable name
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#Load shapefiles
region <- st_read(paste0(dirname(getwd()),"/Data_inp/IBRA7_regions_states_koala_dissolve.shp"))
nsw_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
seq_region <- st_read(paste0(dirname(getwd()),"/Data_inp/Habitat_maps/SEQ/SEQRP_study_area.shp"))
seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))

######################
#Figure A: Patches meeting known NIKA criteria, by area of habitat in patch
######################
df <- loadRData(paste0(inpdir, "data/","Known3_scenario_5_clusterthresh_habitat_0ha.Rdata"))
#st_write(df, paste0(shpdir,"Known3_scenario_5_clusterthresh_habitat_0ha.shp"))
greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")[2:5]

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(df) +
  tm_fill(col='area_col', title="Area (ha)", legend.position=c("top", "right"), colorNA="grey90", palette=greypal, 
          breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k")) +
 # tm_shape(region) + tm_borders() +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))
p2 <- tm_shape(region, bbox=seq_region) + 
  tm_fill(palette="grey90") +
  tm_shape(df) +
  tm_fill(col='area_col', title="Area (ha)", legend.position=c("top", "right"), colorNA="grey90", palette=greypal, 
          breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k")) +
  #tm_shape(region) + tm_borders() +
  tm_layout(legend.show=FALSE, frame=TRUE, title="(b)", title.position = c("LEFT", "TOP"))
p3 <- tm_shape(region, bbox=nsw_region) + 
  tm_fill(palette="grey90") +
  tm_shape(df) +
  tm_fill(col='area_col', title="Area (ha)", legend.position=c("top", "right"), colorNA="grey90", palette=greypal, 
          breaks=c(0, 1000, 10000, 10000000), labels=c("<1k", "1k-10k",">10k")) +
  #tm_shape(region) + tm_borders() +
  tm_layout(legend.show=FALSE, frame=TRUE, title="(c)", title.position = c("LEFT", "TOP"))


tmap_save(p, paste0(oupdir, "Fig_A_Known_a.png"), height=1920, width=1080)
tmap_save(p2, paste0(oupdir, "Fig_A_Known_b.png"), height=1072, width=1056)
tmap_save(p3, paste0(oupdir, "Fig_A_Known_c.png"), height=1072, width=716)
tmap_save(p, paste0(oupdir, "Fig_A_Known_a.eps"), height=1920, width=1080)
tmap_save(p2, paste0(oupdir, "Fig_A_Known_b.eps"), height=1072, width=1056)
tmap_save(p3, paste0(oupdir, "Fig_A_Known_c.eps"), height=1072, width=716)


######################
#Figure B: Climate suitable habitat
######################




######################
#Figure C: Current koalas overlaid on habitat likely to be lost to climate change
######################
###Mid range, majority of models
kdf <- loadRData(paste0(inpdir, "data/","Lost3_scenario_3_clusterthresh_habitat_0ha.Rdata"))
hdf <- loadRData(paste0(inpdir, "data/","Habitat_scenario_1_clusterthresh_habitat_0ha.Rdata"))
#check scenario
#st_write(kdf, paste0(shpdir,"Lost3_scenario_5_clusterthresh_habitat_0ha.shp"))
#st_write(hdf, paste0(shpdir,"Habitat_scenario_1_clusterthresh_habitat_0ha.shp"))
greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")[2:5]

kdf <- kdf %>% mutate(plotid = 1)
hdf <- hdf %>% mutate(plotid = 1)

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(hdf) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[4]) +
  tm_shape(kdf) +
  tm_fill(col='plotid', legend.show=FALSE, colorNA="grey90", palette="mediumorchid2") +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("Habitat", "Koalas"), title="At risk", col=c(greypal[4], "mediumorchid2"), border.col="white") +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_Ca_Lost_mid.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_Ca_Lost_mid.eps"), height=1920, width=1080)

###Core range, all models
kdf <- loadRData(paste0(inpdir, "data/","Lost3_scenario_2_clusterthresh_habitat_0ha.Rdata"))
hdf <- loadRData(paste0(inpdir, "data/","Habitat_scenario_4_clusterthresh_habitat_0ha.Rdata"))
#check scenario
#st_write(kdf, paste0(shpdir,"Lost3_scenario_5_clusterthresh_habitat_0ha.shp"))
#st_write(hdf, paste0(shpdir,"Habitat_scenario_1_clusterthresh_habitat_0ha.shp"))
greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")[2:5]

kdf <- kdf %>% mutate(plotid = 1)
hdf <- hdf %>% mutate(plotid = 1)

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(hdf) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[4]) +
  tm_shape(kdf) +
  tm_fill(col='plotid', legend.show=FALSE, colorNA="grey90", palette="mediumorchid2") +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("Habitat", "Koalas"), title="At risk", col=c(greypal[4], "mediumorchid2"), border.col="white") +
  tm_layout(frame=FALSE, title="(b)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_Cb_Lost_core.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_Cb_Lost_core.eps"), height=1920, width=1080)
