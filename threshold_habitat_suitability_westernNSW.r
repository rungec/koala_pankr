library(raster)
library(sf)
library(tidyverse)
library(stars)

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
wr_reclass <- reclassify(wr, rcl, filename="Data_inp/Habitat_maps/NSW/Western_regions_90perc_threshold.tif")


rcl <- matrix(c(0, 0.159,0.159, 1,NA, 1), ncol=3, nrow=2)
wr_reclass <- reclassify(wr, rcl, filename="Data_inp/Habitat_maps/NSW/Western_regions_80perc_threshold.tif")

summary(wr_reclass)
ncell(wr_reclass)

#########################
##Plot the output
wr_reclass <- read_stars("Data_inp/Habitat_maps/NSW/Western_regions_90perc_threshold.tif")
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[5])
region <- st_read("Data_inp/Habitat_maps/NSW/KMRs.shp") %>% st_transform(st_crs(wr_reclass))
western_region <- region %>% filter(KMR %in% c("Darling Riverine Plains", "Riverina", "Far West"))

#90 percent cutoff
png(filename="Western_regions_90perc_threshold.png", height=1080, width=1080)
plot(st_geometry(region), reset=FALSE, col = greypal[1], border = 'grey70')
plot(st_geometry(western_region), reset=FALSE, col = "white", border = 'grey70', add=TRUE)
plot(wr_reclass, col=greypal[2], main=NULL, add=TRUE)
dev.off()

##80 percent cutoff
wr_reclass <- read_stars("Data_inp/Habitat_maps/NSW/Western_regions_80perc_threshold.tif")
png(filename="Western_regions_80perc_threshold.png", height=1080, width=1080)
plot(st_geometry(region), reset=FALSE, col = greypal[1], border = 'grey70')
plot(st_geometry(western_region), reset=FALSE, col = "white", border = 'grey70', add=TRUE)
plot(wr_reclass, col=greypal[2], main=NULL, add=TRUE)
dev.off()

