#This script interpolates climate projections for the koala, consistent with IUCN guidelines.

library(raster)
library(sf)
library(tidyverse)

setwd("D:/Box Sync/DAWE/Climate_change/Climate_hoskings/")

#Climate projections
#from Adams-Hosking C, Grantham HS, Rhodes JR, McAlpine C, Moss PT. 2011. Modelling climate-change-induced shifts in the distribution of the koala. Wildlife Research 38:122-130. CSIRO PUBLISHING.
#and Briscoe, NJ, Kearney, MR, Taylor, CA & Wintle, BA 2016, 'Unpacking the mechanisms captured by a correlative species distribution model to improve predictions of climate refugia', Global Change Biology, vol. 22, no. 7, pp. 2425-2439.


####################
#LOAD BRISCOE RASTERS
climdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change"
threshdir <- "Climrasters_thresholded/"

#maxent and nichemapper predictions have different extents (nichemapper includes tasmania), so we load separately
maxestack <- stack(list.files(paste0(climdir, "/Maxent"), ".tif", recursive = TRUE, full.names = TRUE))
crs(maxestack) <- CRS("+init=epsg:4283")

nichestack <- stack(list.files(paste0(climdir, "/NicheMapper"), ".tif", recursive = TRUE, full.names = TRUE))
crs(nichestack) <- CRS("+init=epsg:4283")
#crop the extent and snap cells to that of maxent rasters
#nichestack <- resample(nichestack, maxestack, method='ngb')

##############
#load HOSKING rasters
k_curr <- raster(list.files("data/asciis/", full.names = TRUE)[1])
k_2030 <- raster(list.files("data/asciis/", full.names = TRUE)[2])
k_2050 <- raster(list.files("data/asciis/", full.names = TRUE)[3])
#k_2070 <- raster(list.files("Climate_hoskings/data/asciis/", full.names = TRUE)[4])

##############
#Interpolate HOSKING to 2021 & 2042
#current = 1961-1990
k_2021 <- overlay(k_curr, k_2030, fun=function(x, y){x + (2021-1990)*((y-x)/(2030-1990))}, na.rm=FALSE, filename="output/Hoskings_2021_koalarange_interpolated.tif", format="GTiff", overwrite=TRUE)
  
k_2042 <- overlay(k_2030, k_2050, fun=function(x, y){x + (2042-2030)*((y-x)/(2050-2030))}, na.rm=FALSE, filename="output/Hoskings_2042_koalarange_interpolated.tif", format="GTiff", overwrite=TRUE)

##############
#Interpolate BRISCOE to 2021 and 2042
#current = 1990-2009
#assuming 'current' is 2009. Briscoe models use intepolated climate data from 1990-2009 as baseline.

rasterinterpfun <- function(r1, r2, year, oupname){
  overlay(r1, r2, fun=function(x, y){x + ((year-2009)*(y-x)/(2070-2009))}, na.rm=FALSE, 
          filename=paste0("output/Briscoe_", year, "_", oupname, "_interpolated.tif"), format="GTiff", overwrite=TRUE)
}

#2041
k_2042_maxent_av_ACC <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_ACCESS1.3_2070"]], 2042, "Maxent_av_ACC")
k_2042_maxent_av_HAD <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_HadGEM2.CC_2070"]], 2042, "Maxent_av_HAD")

k_2042_maxent_extrA_ACC <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_ACCESS_1.3_2070"]], 2042, "Maxent_ExtA_ACC")
k_2042_maxent_extrA_HAD <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_HadGEM2.CC_2070"]], 2042, "Maxent_ExtA_HAD")

k_2042_maxent_extrB_ACC <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_ACCESS1.3_2070"]], 2042, "Maxent_ExtB_ACC")
k_2042_maxent_extrB_HAD <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_HadGEM2.CC_2070"]], 2042, "Maxent_ExtB_HAD")

k_2042_niche_high_ACC <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Acc70"]], 2042, "NicheM_high_ACC")
k_2042_niche_high_HAD <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Had70"]], 2042, "NicheM_high_HAD")

k_2042_niche_med_ACC <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Acc70"]], 2042, "NicheM_med_ACC")
k_2042_niche_med_HAD <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Had70"]], 2042, "NicheM_med_HAD")

k_2042_niche_low_ACC <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Acc70"]], 2042, "NicheM_low_ACC")
k_2042_niche_low_HAD <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Had70"]], 2042, "NicheM_low_HAD")


####2021
k_2021_maxent_av_ACC <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_ACCESS1.3_2070"]], 2021, "Maxent_av_ACC")
k_2021_maxent_av_HAD <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_HadGEM2.CC_2070"]], 2021, "Maxent_av_HAD")

k_2021_maxent_extrA_ACC <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_ACCESS_1.3_2070"]], 2021, "Maxent_ExtA_ACC")
k_2021_maxent_extrA_HAD <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_HadGEM2.CC_2070"]], 2021, "Maxent_ExtA_HAD")

k_2021_maxent_extrB_ACC <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_ACCESS1.3_2070"]], 2021, "Maxent_ExtB_ACC")
k_2021_maxent_extrB_HAD <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_HadGEM2.CC_2070"]], 2021, "Maxent_ExtB_HAD")

k_2021_niche_high_ACC <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Acc70"]], 2021, "NicheM_high_ACC")
k_2021_niche_high_HAD <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Had70"]], 2021, "NicheM_high_HAD")

k_2021_niche_med_ACC <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Acc70"]], 2021, "NicheM_med_ACC")
k_2021_niche_med_HAD <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Had70"]], 2021, "NicheM_med_HAD")

k_2021_niche_low_ACC <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Acc70"]], 2021, "NicheM_low_ACC")
k_2021_niche_low_HAD <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Had70"]], 2021, "NicheM_low_HAD")

#2030
k_2030_maxent_av_ACC <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_ACCESS1.3_2070"]], 2030, "Maxent_av_ACC")
k_2030_maxent_av_HAD <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_HadGEM2.CC_2070"]], 2030, "Maxent_av_HAD")

k_2030_maxent_extrA_ACC <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_ACCESS_1.3_2070"]], 2030, "Maxent_ExtA_ACC")
k_2030_maxent_extrA_HAD <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_HadGEM2.CC_2070"]], 2030, "Maxent_ExtA_HAD")

k_2030_maxent_extrB_ACC <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_ACCESS1.3_2070"]], 2030, "Maxent_ExtB_ACC")
k_2030_maxent_extrB_HAD <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_HadGEM2.CC_2070"]], 2030, "Maxent_ExtB_HAD")

k_2030_niche_high_ACC <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Acc70"]], 2030, "NicheM_high_ACC")
k_2030_niche_high_HAD <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Had70"]], 2030, "NicheM_high_HAD")

k_2030_niche_med_ACC <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Acc70"]], 2030, "NicheM_med_ACC")
k_2030_niche_med_HAD <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Had70"]], 2030, "NicheM_med_HAD")

k_2030_niche_low_ACC <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Acc70"]], 2030, "NicheM_low_ACC")
k_2030_niche_low_HAD <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Had70"]], 2030, "NicheM_low_HAD")


####2050
k_2050_maxent_av_ACC <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_ACCESS1.3_2070"]], 2050, "Maxent_av_ACC")
k_2050_maxent_av_HAD <- rasterinterpfun(maxestack[["mod_averages_current"]], maxestack[["mod_averages_HadGEM2.CC_2070"]], 2050, "Maxent_av_HAD")

k_2050_maxent_extrA_ACC <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_ACCESS_1.3_2070"]], 2050, "Maxent_ExtA_ACC")
k_2050_maxent_extrA_HAD <- rasterinterpfun(maxestack[["mod_extremesA_current"]], maxestack[["mod_extremesA_HadGEM2.CC_2070"]], 2050, "Maxent_ExtA_HAD")

k_2050_maxent_extrB_ACC <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_ACCESS1.3_2070"]], 2050, "Maxent_ExtB_ACC")
k_2050_maxent_extrB_HAD <- rasterinterpfun(maxestack[["mod_extremesB_current"]], maxestack[["mod_extremesB_HadGEM2.CC_2070"]], 2050, "Maxent_ExtB_HAD")

k_2050_niche_high_ACC <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Acc70"]], 2050, "NicheM_high_ACC")
k_2050_niche_high_HAD <- rasterinterpfun(nichestack[["meanTI_poor_high"]], nichestack[["meanTI_2070_poor_high_Had70"]], 2050, "NicheM_high_HAD")

k_2050_niche_med_ACC <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Acc70"]], 2050, "NicheM_med_ACC")
k_2050_niche_med_HAD <- rasterinterpfun(nichestack[["meanTI_poor_med"]], nichestack[["meanTI_2070_poor_med_Had70"]], 2050, "NicheM_med_HAD")

k_2050_niche_low_ACC <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Acc70"]], 2050, "NicheM_low_ACC")
k_2050_niche_low_HAD <- rasterinterpfun(nichestack[["meanTI_poor_low"]], nichestack[["meanTI_2070_poor_low_Had70"]], 2050, "NicheM_low_HAD")



