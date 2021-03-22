library(raster)
library(sf)
library(tidyverse)
require(rgeos)
require(rgdal)
require(fasterize)
require(exactextractr)

setwd("D:/Box Sync/DAWE/Land_use_change")

koalarangedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Koala_Commonwealth/snes_public_grids_08Aug2019 _filegeodatabase/snes_public.gdb"
krange_pol <- st_read(koalarangedir, layer='koala') %>% st_transform(3577) %>% mutate(habitat_rank = case_when(pres_rank==2 ~ 1,
                                                                                                               pres_rank==1 ~ 2)) %>% select(habitat_rank)

nvisdir <- "Pre_clearing_vege/GRID_NVIS6_0_AUST_PRE_MVS/GRID_NVIS6_0_AUST_PRE_MVS/aus6_0p_mvs"
nvistbldir <- "Pre_clearing_vege/mvg-mvs-numbers-descriptions.csv"

################################
#Load koala occurence data
occdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Data_inp/"
#current koala sightings
currkoala <- st_read(paste0(occdir, "Koala_Qld_noSEQ_2000on_1kmres_noDup.shp")) %>% select(OBJECTID, LATITUDE, LONGITUDE)
st_crs(currkoala) <- CRS("+init=epsg:4283") #set proj as GDA_94 EPSG4283
currkoala <- st_transform(currkoala, 3577) #project to albers 
#currkb <- st_buffer(currkoala, 1000)
#historic koala sightings
histkoala <- st_read(paste0(occdir, "Koala_Qld_noSEQ_merge_1970topresent_1kmres_noDup.shp")) %>% select(OBJECTID, LATITUDE, LONGITUDE)
st_crs(histkoala) <- CRS("+init=epsg:4283") #set proj as GDA_94 EPSG4283
histkoala <- st_transform(histkoala, 3577)
#histkb <- st_buffer(histkoala, 1000)
allkoala <- rbind(currkoala, histkoala)

###############################
#load nvis
nvis_rast <- raster(nvisdir)
#crop to extent of koala range
nvis_rast <- crop(nvis_rast, krange_pol, snap='out', filename="temp/nvis_cropped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

###############################
#extract the NVIS values that koalas overlap
nvis_k_extract <- raster::extract(nvis_rast, allkoala)
#join to NVIS MVS descriptions
nvis_k_extract <- nvis_k_extract %>% table() %>% as_tibble() 
names(nvis_k_extract) <- c("MVS_NO", "count_kocc")
nvis_k_extract <- nvis_k_extract %>% mutate(MVS_NO = as.numeric(MVS_NO))
nvistbl <- read_csv(nvistbldir)
nvis_k_join <- nvis_k_extract %>% left_join(nvistbl, by=c("MVS_NO"="MVS_NO"))
write_csv(nvis_k_join, "NVIS_MVS_koalacount.csv")
  

  
  
  