#This script extimates current and 2070 climate refugia for koala.
#It follows on from Extract_climate.r
#Climate data obtained from Briscoe 2016.


#################
require(sf)
require(tidyverse)
require(raster)

#Dirs
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Data_inp")
climdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change"
threshdir <- "Climrasters_thresholded/"

#load threshold table
thresholds <- read.csv(paste0(threshdir, "Climate_thresholds.csv"))

#maxent and nichemapper predictions have different extents (nichemapper includes tasmania), so we load separately
maxestack <- stack(list.files(paste0(climdir, "/Maxent"), ".tif", recursive = TRUE, full.names = TRUE))
crs(maxestack) <- CRS("+init=epsg:4283")

nichestack <- stack(list.files(paste0(climdir, "/NicheMapper"), ".tif", recursive = TRUE, full.names = TRUE))
crs(nichestack) <- CRS("+init=epsg:4283")
#crop the extent and snap cells to that of maxent rasters
nichestack <- resample(nichestack, maxestack, method='ngb')

#Threshold rasters
reclassfun <- function(currstack, modtype, threshcol) {
  for (d in c("All_records", "No_duplicates")){
    for (i in 1:nlayers(currstack)){
      
      dir.create(paste0(threshdir, threshcol), showWarnings = FALSE)
      
      currname <- names(currstack)[i]
      if (str_detect(currname, "mod")) {
         currmod <- str_split(currname, "_")[[1]][2]
      } else if(str_detect(currname, "meanTI_2070")) {
        currmod <- paste0(str_split(currname, "_")[[1]][3:4], collapse="_")
      } else {
        currmod <- paste0(str_split(currname, "_")[[1]][2:3], collapse="_")
      }

      val <- thresholds[which(thresholds$Scenario==currmod & thresholds$Records==d), threshcol][[1]]
      rcltbl <- matrix(c(0, val, 0, val, 1, 1), ncol=3, byrow=TRUE)
      reclassify(currstack[[i]], rcltbl, filename=paste0(threshdir, threshcol, "/", modtype, "_", d, "_", currname), format='GTiff', datatype='INT2S')
      
    }}}

reclassfun(nichestack, "NicheMapper", "perc95ofrecords")
reclassfun(maxestack, "Maxent", "perc95ofrecords")
reclassfun(nichestack, "NicheMapper", "perc90ofrecords")
reclassfun(maxestack, "Maxent", "perc90ofrecords")
reclassfun(nichestack, "NicheMapper", "perc80ofrecords")
reclassfun(maxestack, "Maxent", "perc80ofrecords")
reclassfun(nichestack, "NicheMapper", "perc50ofrecords")
reclassfun(maxestack, "Maxent", "perc50ofrecords")

#Combine the thresholded rasters to map refugia

refugiaFun <- function(threshcol, currtime, dups){
  rastlist <- list.files(paste0(threshdir, "/", threshcol, "/"), pattern=dups, recursive = FALSE, full.names = TRUE)
  currstack <- raster::stack(rastlist)
  currlist <- names(currstack) %>% as_tibble %>%  
                          mutate(Time = case_when(str_detect(value, "2070") ~"2070",
                                                           str_detect(value, "ACCESS") ~"2070",
                                                           str_detect(value, "HadGEM2") ~"2070",
                                                           TRUE ~ "Current")) %>% 
                          filter(Time==currtime)
  currstack <- currstack[[which(names(currstack) %in% currlist$value)]]
  curroup <- sum(currstack)
  writeRaster(curroup, filename=paste0(threshdir, "/Climate_refugia/Refugia_", currtime, "_", threshcol, "_", dups,".tif"), format='GTiff', datatype='INT2S')
} 

#High suitability (80% of spatially sampled records fall within threshold)
refugiaFun("perc80ofrecords", "2070", "No_duplicates")
refugiaFun("perc80ofrecords", "Current", "No_duplicates")
#Medium suitability (90% of spatially sampled records fall within threshold)
refugiaFun("perc90ofrecords", "2070", "No_duplicates")
refugiaFun("perc90ofrecords", "Current", "No_duplicates")
#Low suitability (95% of spatially sampled records fall within threshold)
refugiaFun("perc95ofrecords", "2070", "No_duplicates")
refugiaFun("perc95ofrecords", "Current", "No_duplicates")
#unfiltered rasters
refugiaFun("perc99ofrecords", "2070", "No_duplicates")
refugiaFun("perc99ofrecords", "Current", "No_duplicates")

#Just rename the files and move to another directory
renamefun <- function(currstack, modtype) {
  
  for (i in 1:nlayers(currstack)){
    currname <- names(currstack)[i]
    if (str_detect(currname, "mod")) {
      currmod <- str_split(currname, "_")[[1]][2]
    } else if(str_detect(currname, "meanTI_2070")) {
      currmod <- paste0(str_split(currname, "_")[[1]][3:4], collapse="_")
    } else {
      currmod <- paste0(str_split(currname, "_")[[1]][2:3], collapse="_")
    }
    
    writeRaster(currstack[[i]], filename=paste0(threshdir, "nothreshold/", modtype, "_", currname), format='GTiff', datatype='FLT4S')
  }
} 
renamefun(nichestack, "NicheMapper")
renamefun(maxestack, "Maxent")


