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
nvistbldir <- "Pre_clearing_vege/NVIS_MVS_koalacount.csv"

qld_redddir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Habitat_Qld/Datasets/REDD_QldnoSEQ_summary_for_RE_ranking_v2.csv"
qld_reddtbl <- read_csv(qld_redddir)
qld_reshp <- "Pre_clearing_vege/Qld_regional_ecosystem_preclearing/data.gdb"

#environmental suitability raster (see queensland koala habitat mapping report for methods)
env_suitdir <- "D:\Box Sync\GPEM_Postdoc\Koala_NESP\08_Project_outputs\Habitat_Qld\Datasets\env_suitable.shp"

###############################
#load nvis
nvis_rast <- raster(nvisdir)
#crop to extent of koala range
nvis_rast <- crop(nvis_rast, krange_pol, snap='out', filename="temp/nvis_cropped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

##############################
#Load qld regional ecosystems
re_shp <- read_sf(qld_reshp)
#Split out RE1 in format matching REDD table
re_shp <- re_shp %>% mutate(RE1_fixed = case_when(str_detect(RE_LABEL, "/") ~ str_extract(RE_LABEL, "[^//]+"),
                                    TRUE ~ RE_LABEL))
#join to the regional ecosystem suitability table
re_shp <- re_shp %>% left_join(qld_reddtbl, by=c("RE1_fixed"="re_id"))


  