library(raster)
library(sf)
library(tidyverse)

#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")

inpdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Koala_NSW/"

r <- raster(paste0(inpdir, "KoalaPackage_Claire/combine_KMR_KHSM_arks2.tif"))
wr <- raster(paste0(inpdir, "Koala_habitat_suitability_model/khsmwesternregionsv1/khsmwesternregionsv1/KHSM_WesternRegions_v1.tif"))

curr_koala <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Data_inp/Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")
hist_koala <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Data_inp/Koala_Qld_NSW_merge_1970to2000_1kmres_noDup.shp")

kcurr <- raster::extract(wr, curr_koala)
kvals1 <- kcurr %>% tibble() %>% filter(!is.na(.))
kvals1 <- setNames(kvals1, "habitat_suitability")
khist <- raster::extract(wr, hist_koala)
kvals2 <- khist %>% tibble() %>% filter(!is.na(.))
kvals2 <- setNames(kvals2, "habitat_suitability")

summary(kvals1)
summary(kvals2)
quantile(kvals1$habitat_suitability, prob=c(0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75))
quantile(kvals2$habitat_suitability, prob=c(0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75))

hist(kvals1$habitat_suitability, breaks=10, main="Histogram of koala occurrences in western NSW", xlab="Habitat suitability")
hist(kvals2$habitat_suitability, breaks=10, main="Histogram of historical koala occurrences in western NSW", xlab="Habitat suitability")

hist(kvals2$habitat_suitability, breaks=10, main="Histogram of koala occurrences in western NSW", xlab="Habitat suitability", border="white")
hist(kvals1$habitat_suitability, breaks=10, add=TRUE, col=NA)

###########################
#Reclassify western regions

rcl <- matrix(c(0, 0.109,0.109, 1,NA, 1), ncol=3, nrow=2)

wr_reclass <- reclassify(wr, rcl, filename="Western_regions_90perc_threshold.tif")

