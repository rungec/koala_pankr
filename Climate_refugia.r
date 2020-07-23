#This script extimates current and 2070 climate refugia for koala.
#It follows on from Extract_climate.r
#Climate data obtained from Briscoe 2016.


#################
require(sf)
require(tidyverse)
require(raster)

#Dirs
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output")
climdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change"
threshdir <- "/Climrasters_thresholded"

#load threshold table
thresholds <- read.csv("Climate_thresholds.csv")

#maxent and nichemapper predictions have different extents, so we load separately
maxestack <- stack(list.files(paste0(climdir, "/Maxent"), ".tif", recursive = TRUE, full.names = TRUE))
crs(maxestack) <- CRS("+init=epsg:4283")

nichestack <- stack(list.files(paste0(climdir, "/NicheMapper"), ".tif", recursive = TRUE, full.names = TRUE))
crs(nichestack) <- CRS("+init=epsg:4283")


#Threshold rasters
reclassfun <- function(currstack, modtype) {
  for (d in c("All records", "No duplicates")){
    for (i in 1:nlayers(currstack)){
      
      currname <- names(currstack[[i]])
      currmod <- 
      val <- thresholds[which(thresholds$Model==currmod & thresholds$Records==d), "perc95ofrecords"][[1]]
      rcltbl <- matrix(c(0, val, 0, val, 1, 1), ncol=3, byrow=TRUE)
      reclassify(currstack[[i]], rcltbl, filename=paste0("Output/Climrasters_thresholded/", modtype, "_", currname, "_", d), format='GTiff', datatype='INT2S')
      
    }}}

reclassfun(nichestack, "NicheMapper")
reclassfun(maxestack, "Maxent")

names(maxestack)
names(nichestack)

#fix maxent and nichemapper extent issue
#drop un-needed nichemapper files



paste0(str_split(names(maxestack)[[i]], "_")[[1]][1:2], collapse="_")

#Combine the thresholded rasters to map refugia