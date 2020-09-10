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
kpankr <- kfix %>% 
          mutate(known_pankr = case_when(current_koala > 1 & intact_area_ha > 0 ~ 1,
                                         current_koala > 1 & snes_likelyhabitat_ha > 0 ~ 2,
                                         current_koala > 1 & snes_maybehabitat_ha > 0 ~ 3,
                                         current_koala > 1 & intact_area_ha > 0 & climate_2070_perc90ofrecords ==12 ~ 4,
                                         current_koala > 1 & intact_area_ha > 0 & climate_2070_perc90ofrecords > 5 ~ 5,
                                            TRUE ~ 0),
                 recovery_pankr = case_when(current_koala == 0 & recoverable_area_ha > 0 ~ 1,
                                            TRUE ~ 0),
                 bushfire_pankr = case_when(is.na(firefreq_88to15) ~ 1,
                                            firefreq_88to15 == 1 | is.na(firefreq_88to15) ~ 2,
                                            firefreq_88to15 < 3 | is.na(firefreq_88to15) ~ 3,
                                            firefreq_88to15 == 1 | is.na(firefreq_88to15) & pawc_mean > 77 ~ 4,
                                            TRUE ~ 0),
                 drought_refugia = case_when(soildepth_mean > 1.0 & pawc_mean > 100 & permanent_water_area_ha > 0 ~ 1,
                                            soildepth_mean > 1.0 & pawc_mean > 100  ~ 2,
                                             soildepth_mean > 0.958 & pawc_mean > 77 & permanent_water_area_ha > 0 ~ 3,
                                             TRUE ~ 0),
                 climate_refugia = case_when(climate_2070_perc90ofrecords==12 ~ 1,
                                             climate_2070_perc95ofrecords==12 ~ 2,
                                             climate_2070_perc99ofrecords==12 ~ 3,
                                             climate_2070_perc90ofrecords > 5 ~ 4,
                                             climate_2070_perc95ofrecords > 5 ~ 5,
                                             climate_2070_perc99ofrecords > 5 ~ 6,
                                             TRUE ~ 0)) %>% 
            dplyr::select(cellid,known_pankr, recovery_pankr, bushfire_pankr, drought_refugia, climate_refugia)


save(kpankr, file=paste0(oupdir, "koala_raw_pankrclasses_", cell_area, ".Rdata"))

####################################
library(tmap)
plotfun <- function(data, colid, plottitle, ...) {
  p <- tm_shape(data) +
    tm_fill(col=colid, title=plottitle, legend.position=c("top", "right"), colorNA="grey90", ...) +
    tm_shape(region) + tm_borders()
  tmap_save(p, paste0(oupdir, colid, ".png"), height=1920, width=1080)
}
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[2:5])


###END