#This script categorises each polygon as either one of the different types of PANKRs (or not one)
#input is a shp with attributes that are used to classify each type of pankr
#follows on from PANKR_data_prep.r


#################
library(sf)
library(tidyverse)
library(raster)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")

cell_area = "100ha" 

#Load data
load(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, ".Rdata"))

#still to do:
#how to integrate across habitat data?
#what is recoverable/future habitat?
#add columns for high_med suitability and low uncertainty climate now & future
#add a lookup table to make it easier to edit the values


#reclassify climate
k_grid <- k_grid %>% mutate(currclim_high_suitability = )


#Categorise
k_grid <- k_grid %>% 
          mutate(known_pankr = case_when(current_koala > 1 & habitat_area > XXX ~ 1,
                                        TRUE ~ 0),
                 recovery_pankr = case_when(current_koala ==0 & habitat_area > XXX ~ 1,
                                            TRUE ~ 0),
                 bushfire_pankr = case_when(firefreq_88to15 < XXX ~ 1,
                                            TRUE ~ 0),
                 climate_refugia = case_when(soildepth_mean > XXX & pawc_mean > XXX & permanent_water_area_ha > XXX ~ 1,
                                             TRUE ~ 0))
















save(k_grid, paste0("Gridded_data/koala_raw_pankrclasses_", cell_area, ".Rdata"))

###END