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

#maxent and nichemapper predictions have different extents (nichemapper includes tasmania), so we load separately
maxestack <- stack(list.files(paste0(climdir, "/Maxent"), ".tif", recursive = TRUE, full.names = TRUE))
crs(maxestack) <- CRS("+init=epsg:4283")

nichestack <- stack(list.files(paste0(climdir, "/NicheMapper"), ".tif", recursive = TRUE, full.names = TRUE))
crs(nichestack) <- CRS("+init=epsg:4283")
#crop the extent and snap cells to that of maxent rasters
nichestack <- crop(nichestack, maxestack, snap='near')


#Threshold rasters
reclassfun <- function(currstack, modtype, threshcol) {
  for (d in c("All_records", "No_duplicates")){
    for (i in 1:nlayers(currstack)){
      
      dir.create(paste0("Climrasters_thresholded/", threshcol), showWarnings = FALSE)
      
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
      reclassify(currstack[[i]], rcltbl, filename=paste0("Climrasters_thresholded/", threshcol, "/", modtype, "_", d, "_", currname), format='GTiff', datatype='INT2S')
      
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
#make a list of the rasters (no_duplicates means threshold was defined based on spatially sampled occurrence records)
rastlist <- as_tibble(list.files("Climrasters_thresholded/perc80ofrecords/", "No_duplicates", recursive = FALSE, full.names = FALSE))
rastlist <- rastlist %>% mutate(Tool = str_split(value,"_")[[1]][1], 
                                Time = case_when(str_detect(value, "2070") ~"2070",
                                                 str_detect(value, "ACCESS") ~"2070",
                                                 str_detect(value, "HadGEM2") ~"2070",
                                                     TRUE ~ "Current"),
                                Filename = paste0("Climrasters_thresholded/perc80ofrecords/", value))

#High suitability (80% of spatially sampled records fall within threshold)
currlist <- rastlist %>% filter(Time==2070)
highstack <- raster::stack(currlist$Filename)
raster::calc(highstack, sum(na.rm = TRUE), filename="Climate_refugia/Refugia_high_suitability_nodups.tif", format='GTiff', datatype='INT2S')

#Medium suitability (90% of spatially sampled records fall within threshold)

#Low suitability (95% of spatially sampled records fall within threshold)


