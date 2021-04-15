#This file plots figures for NESP4.4.12 final report
#follows on from PANKR_categorise.r and PANKR_clustering.r

library(sf)
library(tidyverse)
library(ggplot2)
library(tmap)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
inpdir <- "Clusters/Test5/"
oupdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/NIKA/Report figures/"
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
tmap_save(p, paste0(oupdir, "Fig_A_Known_a.tiff"), height=1920, width=1080)
tmap_save(p2, paste0(oupdir, "Fig_A_Known_b.tiff"), height=1072, width=1056)
tmap_save(p3, paste0(oupdir, "Fig_A_Known_c.tiff"), height=1072, width=716)



######################
#Figure C: At risk koalas overlaid on habitat likely to be lost to climate change
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
tmap_save(p, paste0(oupdir, "Fig_Ca_Lost_mid.tiff"), height=1920, width=1080)

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
tmap_save(p, paste0(oupdir, "Fig_Cb_Lost_core.tiff"), height=1920, width=1080)

#############################
#Figure D: Monitoring areas: habitat >10km and >50km from recent sightings (30% coverage)
#############################
curr_hab <- loadRData(paste0(inpdir, "data/","Current_scenario_5_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)
curr_k_1km <- loadRData(paste0(inpdir, "data/","Known_scenario_1_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
curr_k_10km <- loadRData(paste0(inpdir, "data/","Monitoring_scenario_1_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)
curr_k_50km <- loadRData(paste0(inpdir, "data/","Monitoring_scenario_2_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)

greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(curr_hab) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  tm_shape(curr_k_50km) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[4]) +
  tm_shape(curr_k_10km) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[2]) +
  tm_shape(curr_k_1km) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[1]) +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("unsurveyed","<50km", "<10km", "<1km"), title="Distance to record", col=c(greypal[5], greypal[4], greypal[2], greypal[1]), border.col="grey90") +
  tm_layout(frame=FALSE, title="(b)", title.position = c("LEFT", "TOP"))


tmap_save(p, paste0(oupdir, "Fig_D_dist2koala.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_D_dist2koala.tiff"), height=1920, width=1080)


#############################
#Figure E: Potentially extinct populations (30% coverage)
#############################
hist_k_10km <- loadRData(paste0(inpdir, "data/","Monitoring_scenario_5_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)
hist_k_50km <- loadRData(paste0(inpdir, "data/","Monitoring_scenario_6_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)
hist_k_1km <- loadRData(paste0(inpdir, "data/","Monitoring_scenario_4_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)
curr_hab <- loadRData(paste0(inpdir, "data/","Current_scenario_5_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)

greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(curr_hab) +
  tm_fill(col='plotid', title="Habitat", labels=c("Current"), legend.position=c("top", "right"), palette=greypal[5]) +
  tm_shape(hist_k_50km) +
  tm_fill(col='plotid', legend.show=FALSE, palette="mediumorchid3") +
  tm_shape(hist_k_10km) +
  tm_fill(col='plotid', legend.show=FALSE, palette="mediumorchid1") +
  tm_shape(hist_k_1km) +
  tm_fill(col='plotid', legend.show=FALSE, palette="mediumorchid1") +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("<50km", "<10km"), title="Historical records", col=c( "mediumorchid3", "mediumorchid1"), border.col="grey90") +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_E_extinctkoala.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_E_extnctkoala.tiff"), height=1920, width=1080)

#############################
#Figure F: NIKA under different coverage and habitat thresholds
#############################

h10 <- loadRData(paste0(inpdir, "data/","Known3_scenario_9_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
h30 <- loadRData(paste0(inpdir, "data/","Known3_scenario_10_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
h50 <- loadRData(paste0(inpdir, "data/","Known3_scenario_5_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")

#Plot habitat of different coverage thresholds
p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(h10) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[2]) +
  tm_shape(h30) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[3]) +
  tm_shape(h50) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("50%","30%", "10%"), title="Coverage", col=c(greypal[5], greypal[3], greypal[2]), border.col="grey90") +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_F_coverage.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_F_coverage.tiff"), height=1920, width=1080)


#Plot NIKA under likely and possible habitat thresholds

lik <- loadRData(paste0(inpdir, "data/","Known3_scenario_5_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
pos <- loadRData(paste0(inpdir, "data/","Known3_scenario_7_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(pos) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[3]) +
  tm_shape(lik) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("likely","possible"), title="Habitat", col=c(greypal[5], greypal[3]), border.col="grey90") +
  tm_layout(frame=FALSE, title="(b)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_F_quality.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_F_quality.tiff"), height=1920, width=1080)


######################
#Figure B: Climate suitable habitat (50% coverage)
######################
curr_mid <- loadRData(paste0(inpdir, "data/","Current_scenario_2_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid=1)
core_all <- loadRData(paste0(inpdir, "data/","Climate_scenario_1_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid=1)
mid_maj <- loadRData(paste0(inpdir, "data/","Climate_scenario_8_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid=1)

greypal <- RColorBrewer::brewer.pal(5, "YlGnBu")

#Plot habitat under climate change
p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(curr_mid) +
  tm_fill(col='plotid', legend.show=FALSE, palette="grey70") +
  tm_shape(mid_maj) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  tm_shape(core_all) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[3]) +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("Current","Mid 50%", "Core 100%"), title="Models", col=c("grey50", greypal[5], greypal[3]), border.col="grey90") +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_B_Climate.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_B_Climate.tiff"), height=1920, width=1080)


#############################
#Figure G: NIKA under different climate thresholds
#############################

core_nika <- loadRData(paste0(inpdir, "data/","Known3e_scenario_11_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
nika <- loadRData(paste0(inpdir, "data/","Known3_scenario_5_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)


#Plot habitat of different coverage thresholds
p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(nika) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  tm_shape(core_nika) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[3]) +
  #tm_shape(region) + #tm_borders() +
  tm_layout(frame=FALSE) +
  tm_add_legend(type=c("fill"), labels=c("Mid 50%","Core 100%"), title="Models", col=c(greypal[5], greypal[3]), border.col="grey90") +
  tm_layout(frame=FALSE, title="(b)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_G_nika_climate.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_G_nika_climate.tiff"), height=1920, width=1080)

######################
#Figure H: NIKA overlaid on current habitat
######################
df <- loadRData(paste0(inpdir, "data/","Known3_scenario_5_clusterthresh_habitat_0ha.Rdata")) %>% mutate(plotid = 1)
curr_hab <- loadRData(paste0(inpdir, "data/","Current_scenario_5_clusterthresh_pu_0ha.Rdata")) %>% mutate(plotid = 1)

#st_write(df, paste0(shpdir,"Known3_scenario_5_clusterthresh_habitat_0ha.shp"))
greypal <- c("grey70", RColorBrewer::brewer.pal(5, "YlGnBu")[2:5])

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(curr_hab) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[1]) +
  tm_shape(df) +
  tm_fill(col='plotid', legend.show=FALSE, palette=greypal[5]) +
  # tm_shape(region) + tm_borders() +
  tm_layout(frame=FALSE, title="(a)", title.position = c("LEFT", "TOP"))

tmap_save(p, paste0(oupdir, "Fig_H_Known_a.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_H_Known_a.tiff"), height=1920, width=1080)


######################
#Figure I: Current likely and possible habitat
######################
#Load data
load("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/Gridded_data/clean/koala_gridded_vars_100ha_tidy_v3.Rdata")

likely_hab <- k_fix %>% mutate(likely = case_when(habitat_area_total > 50 ~ 1, TRUE ~ 0)) %>%
                dplyr::select(likely) %>% filter(likely==1)
possible_hab <- k_fix %>% mutate(possible = case_when(habitat_area_total_s2 > 30 ~ 1, TRUE ~ 0)) %>%
                dplyr::select(possible) %>% filter(possible==1)

p <- tm_shape(region) + 
  tm_fill(palette="grey90") +
  tm_shape(possible_hab) +
  tm_fill(col='possible', legend.show=FALSE, palette=greypal[3], style='cat') +
  tm_shape(likely_hab) +
  tm_fill(col='likely', legend.show=FALSE, palette=greypal[5], style='cat') +
  tm_add_legend(type=c("fill"), labels=c("Likely 50% cover","Possible 30% cover"), title="Habitat", col=c(greypal[5], greypal[3]), border.col="grey90") +
    # tm_shape(region) + tm_borders() +
  tm_layout(frame=FALSE)

tmap_save(p, paste0(oupdir, "Fig_I_Habitat.png"), height=1920, width=1080)
tmap_save(p, paste0(oupdir, "Fig_I_Habitat.tiff"), height=1920, width=1080)

