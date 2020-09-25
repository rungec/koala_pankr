#################
library(sf)
library(tidyverse)
library(raster)
library(dismo)
#library(stars)


#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"

cell_area = "100ha" 

###################################
#load predictions
predrast <- raster("Koala_NSW/Koala_habitat_suitability_model/full_region/qvn_sf6_nrsp_KHSM_complex.tif")
#load prediction points
points <- st_read("Occurrence records/NSW_records_used_in_KHSMs/QNV_KoalaRecords_inModel_sf6_4727.shp")

# background data
bg <- randomPoints(predrast, 10000)

###################################
# extract values
pvtest <- raster::extract(predrast, points)
avtest <- raster::extract(predrast, bg)

e2 <- dismo::evaluate(p=pvtest, a=avtest)
sink(file="Summary_stats_for_qvn_sf6_nrsp_KHSM_complex.txt")
e2
threshold(e2)
sink()

png(filename="ROC_qvn_sf6_nrsp_KHSM_complex.png")
plot(e2, 'ROC')
dev.off()
