#This script clips the regional ecosystem mapping v11.0 to bioregions containing koala
#summarises the number and % of RE polygons overlapped by koala occurrences
#and makes a list of all RE containing potential koala trees (Eucalyptus or Corymbia)

#################
require(sf)
require(tidyverse)
require(raster)

#Dirs
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output")
REdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Vegetation/Qld_vege_mapping/Regional_ecosystem_v11/"
occdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Occurrence records"

#Load data
#current koala sightings
currkoala <- st_read(occdir, "Koala_Qld_noSEQ_2000on_1kmres_noDup")
st_crs(currkoala) <- CRS("+init=epsg:4283") #set proj as GDA_94 EPSG4283

#Load REDD lookup table
redd <- read_csv(paste0(REdir, 'redd-v11-1-2019.csv'))
#Add column designating any description mentioning Eucalyptus
redd <- redd %>% mutate(Euc_present = case_when(str_detect(`Short Description`, "Eucalyptus") ~ "Euc",
                                                TRUE ~ "Not"))
#Load regional ecosystem mapping
reveg <- st_read(dsn=paste0(REdir, 'data.gdb'), layer='Vegetation_management_regional_ecosystem_map')
#a number of invalid geometries, lets fix them
a <- st_is_valid(reveg, reason=TRUE)
#number of polygons for each RE
table(reveg$.....)

############################
#buffer koala sightings to 1km, the accuracy of the dataset
#first project to GDA_94 Aust Albers EPSG 3577 which has units in m 
currkoala <- st_transform(currkoala, 3577)
currkb <- st_buffer(currkoala, 1000)
currkb <- st_transform(currkb, 4283)


#Split by recode
for (i in unique(revege$re_id)){
  currveg <- revege %>% select(re_id == i)
  #Overlay koala occurrences
  overlap <- st_intersects(currveg, currkb, sparse=FALSE)
  #st_intersection for clip
  #output is matrix TRUE FALSe col = points, row=polygon
  num_polygons = nrow(currvege)
  num_koala_records_in_re = apply(overlap, 1, any)
  num_polygons_with_records = apply(overlap, 2, any)
}

#Join to redd dataset and output





