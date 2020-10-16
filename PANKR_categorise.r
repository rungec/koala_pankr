#This script categorises each polygon as either one of the different types of PANKRs (or not one)
#input is a shp with attributes that are used to classify each type of pankr
#follows on from PANKR_data_prep.r

#################
library(sf)
library(tidyverse)
library(tmap)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/Gridded_data/")
datadir <- "Data_inp/"
oupdir <- "intermediate3/"
plotdir <- paste0(dirname(getwd()), "/figures/")
cell_area="100ha"


#Load data
load("clean/koala_gridded_vars_100ha_tidy.Rdata")


#Categorise
#scenarios where we classify polygons as known koala habitat only if those 100ha polygons have koala records
known_pankr <- k_fix %>% 
  mutate(scenario_1 = case_when(current_koala > 0 & habitat_area_total > 0 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala > 0 & habitat_area_total_s2 > 0 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(current_koala > 0 & habitat_area_total > 50  ~ 1, TRUE ~ 0),
         scenario_4 = case_when(current_koala > 0 & habitat_area_total_s2 > 50 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(current_koala > 0 & habitat_area_total > 50 & climate_2070_perc90ofrecords > 6 ~ 1, TRUE ~ 0),
         scenario_6 = case_when(current_koala > 0 & habitat_area_total > 50 & climate_2070_perc90ofrecords ==12 ~ 1, TRUE ~ 0)) %>%
  dplyr::select(starts_with('scenario'))
save(known_pankr, file=paste0(oupdir, "koala_known_pankr_raw_", cell_area, ".Rdata")) 
                 
recovery_pankr <- k_fix %>% 
  mutate(scenario_1 = case_when(current_koala == 0 & habitat_area_total > 0 & recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala == 0 & habitat_area_total_s2 > 0 & recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(current_koala == 0 & recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_4 = case_when(current_koala == 0 & habitat_area_total > 0 & recoverable_area_ha > 0 & climate_2070_perc90ofrecords >6 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_2070_perc90ofrecords >6 ~ 1, TRUE ~ 0),
         scenario_6 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_2070_perc90ofrecords ==12 ~ 1, TRUE ~ 0))  %>%
  dplyr::select(starts_with('scenario'))
save(recovery_pankr, file=paste0(oupdir, "koala_recovery_pankr_raw_", cell_area, ".Rdata")) 

#scenarios where we classify polygons as known koala habitat once they have been aggregated (in PANKR_clustering.r)
known2_pankr <- k_fix %>% 
  mutate(scenario_1 = case_when(habitat_area_total > 0 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(habitat_area_total_s2 > 0 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(habitat_area_total > 50  ~ 1, TRUE ~ 0),
         scenario_4 = case_when(habitat_area_total_s2 > 50 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(habitat_area_total > 50 & climate_2070_perc90ofrecords > 6 ~ 1, TRUE ~ 0),
         scenario_6 = case_when(habitat_area_total > 50 & climate_2070_perc90ofrecords ==12 ~ 1, TRUE ~ 0)) %>%
  dplyr::select(starts_with('scenario'))
save(known2_pankr, file=paste0(oupdir, "koala_known2_pankr_raw_", cell_area, ".Rdata")) 

recovery2_pankr <- k_fix %>% 
  mutate(scenario_1 = case_when(habitat_area_total > 0 & recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(habitat_area_total_s2 > 0 & recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(recoverable_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_4 = case_when(habitat_area_total > 0 & recoverable_area_ha > 0 & climate_2070_perc90ofrecords >6 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(recoverable_area_ha > 0 & climate_2070_perc90ofrecords >6 ~ 1, TRUE ~ 0),
         scenario_6 = case_when(recoverable_area_ha > 0 & climate_2070_perc90ofrecords ==12 ~ 1, TRUE ~ 0))  %>%
  dplyr::select(starts_with('scenario'))
save(recovery2_pankr, file=paste0(oupdir, "koala_recovery2_pankr_raw_", cell_area, ".Rdata")) 

# #refugial areas 
# bushfire_pankr <- k_fix %>% 
#   mutate(scenario_1 = case_when(is.na(firefreq_88to15) ~ 1, TRUE ~ 0),
#          scenario_2 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) ~ 1, TRUE ~ 0),
#          scenario_3 = case_when(firefreq_88to15 ==1 | is.na(firefreq_88to15) & 
#                                   (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 ~ 1, TRUE ~ 0),
#          scenario_4 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) & pawc_mean > 77 ~ 1, TRUE ~ 0),
#          scenario_5 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) & pawc_mean > 77 &  
#                                 (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 ~ 1, TRUE ~ 0)) %>%
#   dplyr::select(starts_with('scenario'))
# save(bushfire_pankr, file=paste0(oupdir, "koala_bushfire_pankr_raw_", cell_area, ".Rdata"))           
#                                     
# drought_refugia <- k_fix %>% 
#  mutate(scenario_1 = case_when(soildepth_mean > 1.0 & pawc_mean > 100 & permanent_water_area_ha > 0 ~ 1, TRUE ~ 0),
#         scenario_2 = case_when(soildepth_mean > 1.0 & pawc_mean > 100  ~ 1, TRUE ~ 0),
#         scenario_3 = case_when(soildepth_mean > 0.958 & pawc_mean > 77 & permanent_water_area_ha > 0 ~ 1, TRUE ~ 0)) %>%
#   dplyr::select(starts_with('scenario'))
# save(drought_refugia, file=paste0(oupdir, "koala_drought_refugia_raw_", cell_area, ".Rdata")) 
# 
# climate_refugia <- k_fix %>%
#  mutate(scenario_1 = case_when(climate_2070_perc90ofrecords==12 ~ 1, TRUE ~ 0),
#         scenario_2 = case_when(climate_2070_perc95ofrecords==12 ~ 1, TRUE ~ 0),
#         scenario_3 = case_when(climate_2070_perc99ofrecords==12 ~ 1, TRUE ~ 0),
#         scenario_4 = case_when(climate_2070_perc90ofrecords > 6 ~ 1, TRUE ~ 0),
#         scenario_5 = case_when(climate_2070_perc95ofrecords > 6 ~ 1, TRUE ~ 0),
#         scenario_6 = case_when(climate_2070_perc99ofrecords > 6 ~ 1, TRUE ~ 0),
#         scenario_7 = case_when(climate_2070_perc90ofrecords > 10 ~ 1, TRUE ~ 0),
#         scenario_8 = case_when(climate_2070_perc95ofrecords > 10 ~ 1, TRUE ~ 0),
#         scenario_9 = case_when(climate_2070_perc99ofrecords > 10 ~ 1, TRUE ~ 0)) %>%
#  dplyr::select(starts_with('scenario'))
# save(climate_refugia, file=paste0(oupdir, "koala_climate_refugia_raw_", cell_area, ".Rdata"))


####################################
#Plot each scenario

plotfun <- function(data, plottitle, ...) {
  for(i in 1:ncol(data)){
    if(i<ncol(data)){
    colid = names(data)[i]
 # p <- tm_shape(data, bbox=nsw_region) +
  p <- tm_shape(data) +
    tm_fill(col=colid, title=paste0(plottitle, ": Scenario ", i), legend.position=c("top", "right"), colorNA="grey90", ...) +
    tm_shape(region) + tm_borders()
  tmap_save(p, paste0(plotdir, plottitle, "_", colid, ".png"), height=1920, width=1080)
    } else {
    print("finished")
  }
}}
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(datadir, "IBRA7_regions_states_koala_dissolve.shp"))
# nsw_region <- st_read(paste0(datadir, "Habitat_maps/NSW/KMRs_eastern.shp")) 
# nsw_region <- st_bbox(nsw_region, crs=st_crs(nsw_region))
# seq_region <- st_read(paste0(datadir, "Habitat_maps/SEQ/SEQRP_study_area.shp")) 
# seq_region <- st_bbox(seq_region, crs=st_crs(seq_region))
# 

plotfun(known_pankr, plottitle="Known", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(recovery_pankr, plottitle="Recovery", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun(bushfire_pankr, plottitle="Bushfire refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun(drought_refugia, plottitle="Drought refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
# plotfun(climate_refugia, plottitle="Climate refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

#st_write(known_pankr, paste0(oupdir, "koala_known_pankr_raw_", cell_area, ".gpkg"))

###END