#This script categorises each polygon as either one of the different types of PANKRs (or not one)
#input is a shp with attributes that are used to classify each type of pankr
#follows on from PANKR_data_prep.r

#################
library(sf)
library(tidyverse)
library(raster)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"
cell_area = "100ha" 

#Load data
load(paste0(oupdir, "koala_gridded_vars_", cell_area, "SEQ_tidy.Rdata"))

#still to do:
#how to integrate across habitat data?
#what is recoverable/future habitat?


#Categorise
known_pankr <- kfix %>% 
  mutate(scenario_1 = case_when(current_koala > 0 & intact_area_ha > 0 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala > 0 & snes_likelyhabitat_ha > 0 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(current_koala > 0 & snes_maybehabitat_ha > 0 ~ 1, TRUE ~ 0),
         scenario_4 = case_when(current_koala > 0 & intact_area_ha > 0 & 
                                  climate_2070_perc95ofrecords ==12 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(current_koala > 0 & intact_area_ha > 0 & 
                                  climate_2070_perc95ofrecords > 6 ~ 1, TRUE ~ 0),
         scenario_6 = case_when(current_koala > 0 & intact_area_ha > 0 & 
                                  (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 & 
                                  climate_2070_perc95ofrecords ==12 ~ 1, TRUE ~ 0),
         scenario_7 = case_when(current_koala > 0 & intact_area_ha > 0 & 
                                  (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 & 
                                  climate_2070_perc95ofrecords > 6 ~ 1, TRUE ~ 0))  %>%
  dplyr::select(starts_with('scenario'))
save(known_pankr, file=paste0(oupdir, "koala_known_pankr_raw_", cell_area, ".Rdata")) 
                 
recovery_pankr <- kfix %>% 
  mutate(scenario_1 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_Current_perc95ofrecords >3  ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala == 0 & recoverable_area_ha > 0 & (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_Current_perc95ofrecords >3 & 
                                  climate_2070_perc95ofrecords ==12 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_Current_perc95ofrecords >3 & 
                                  climate_2070_perc95ofrecords > 6 ~ 1, TRUE ~ 0),
         scenario_2 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_Current_perc95ofrecords >3 & (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & 
                                  climate_2070_perc95ofrecords ==12 ~ 1, TRUE ~ 0),
         scenario_3 = case_when(current_koala == 0 & recoverable_area_ha > 0 & climate_Current_perc95ofrecords >3 & (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & 
                                  climate_2070_perc95ofrecords > 6  ~ 1, TRUE ~ 0))  %>%
  dplyr::select(starts_with('scenario'))
save(recovery_pankr, file=paste0(oupdir, "koala_recovery_pankr_raw_", cell_area, ".Rdata")) 
 
bushfire_pankr <- kfix %>% 
  mutate(scenario_1 = case_when(is.na(firefreq_88to15) ~ 1, TRUE ~ 0),
         scenario_2 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) ~ 1, TRUE ~ 0),
         scenario_3 = case_when(firefreq_88to15 ==1 | is.na(firefreq_88to15) & 
                                  (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 ~ 1, TRUE ~ 0),
         scenario_4 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) & pawc_mean > 77 ~ 1, TRUE ~ 0),
         scenario_5 = case_when(firefreq_88to15 == 1 | is.na(firefreq_88to15) & pawc_mean > 77 &  
                                (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & climate_Current_perc95ofrecords > 3 ~ 1, TRUE ~ 0)) %>%
  dplyr::select(starts_with('scenario'))
save(bushfire_pankr, file=paste0(oupdir, "koala_bushfire_pankr_raw_", cell_area, ".Rdata"))           
                                    
drought_refugia <- kfix %>% 
 mutate(scenario_1 = case_when(soildepth_mean > 1.0 & pawc_mean > 100 & permanent_water_area_ha > 0 ~ 1, TRUE ~ 0),
        scenario_2 = case_when(soildepth_mean > 1.0 & pawc_mean > 100  ~ 1, TRUE ~ 0),
        scenario_3 = case_when(soildepth_mean > 0.958 & pawc_mean > 77 & permanent_water_area_ha > 0 ~ 1, TRUE ~ 0)) %>%
  dplyr::select(starts_with('scenario'))
save(drought_refugia, file=paste0(oupdir, "koala_drought_refugia_raw_", cell_area, ".Rdata")) 

climate_refugia <- kfix %>% 
 mutate(scenario_1 = case_when(climate_2070_perc90ofrecords==12 ~ 1, TRUE ~ 0),
        scenario_2 = case_when(climate_2070_perc95ofrecords==12 ~ 1, TRUE ~ 0),
        scenario_3 = case_when(climate_2070_perc99ofrecords==12 ~ 1, TRUE ~ 0),
        scenario_4 = case_when(climate_2070_perc90ofrecords > 6 ~ 1, TRUE ~ 0),
        scenario_5 = case_when(climate_2070_perc95ofrecords > 6 ~ 1, TRUE ~ 0),
        scenario_6 = case_when(climate_2070_perc99ofrecords > 6 ~ 1, TRUE ~ 0)) %>%
 dplyr::select(starts_with('scenario'))
save(climate_refugia, file=paste0(oupdir, "koala_climate_refugia_raw_", cell_area, ".Rdata")) 
                 

####################################
#Plot each scenario

library(tmap)
plotfun <- function(data, plottitle, ...) {
  for(i in 1:ncol(data)){
    if(i<ncol(data)){
    colid = names(data)[i]
  p <- tm_shape(data) +
    tm_fill(col=colid, title=paste0(plottitle, ": Scenario ", i), legend.position=c("top", "right"), colorNA="grey90", ...) +
    tm_shape(region) + tm_borders()
  tmap_save(p, paste0(oupdir, "figures/", plottitle, "_", colid, ".png"), height=1920, width=1080)
    } else {
    print("finished")
  }
}}
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read(paste0(datadir, "IBRA7_regions_states_koala_dissolve.shp"))


plotfun(known_pankr, plottitle="Known", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(recovery_pankr, plottitle="Recovery", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(bushfire_pankr, plottitle="Bushfire refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
#plotfun(drought_refugia, plottitle="Drought refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)
plotfun(climate_refugia, plottitle="Climate refugia", palette=greypal, style='cat', labels=c("Not selected", "Meets criteria"), showNA=FALSE)

st_write(known_pankr, paste0(oupdir, "koala_known_pankr_raw_", cell_area, ".gpkg"))

###END