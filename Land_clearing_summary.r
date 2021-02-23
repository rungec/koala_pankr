library(raster)
library(sf)
library(tidyverse)
require(rgeos)
require(rgdal)
require(fasterize)

setwd("D:/Box Sync/DAWE/Land_use_change")

#FROM WARD et al. 2019
deforestationdir <- "D:/Box Sync/DAWE/Land_use_change/Ward_data/FL100_FWLepoch_3msk_v2.tif.aux/FL100_FWLepoch_3msk_v2.tif"
#In order, the attributes represent the following 15 periods of change:  2000-2002, 2002-2004, 
#2004-2005, 2005-2006, 2006-2007, 2007-2008, 2008-2009, 2009-2010, 2010-2011, 2011-2012, 
#2012-2013, 2013-2014, 2014-2015, 2015-2016 and 2016-2017. 
#LAND USE Catchment Scale Land Use of Australia - Update December 2018
landusedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Land_cover/ACLUMP/geotiff_clum_50m1218m/geotiff_clum_50m1218m/clum_50m1218m.tif"
#rasters are in GDA_94_Albers EPGS:3577

#IBRA7 bioregions
ibra <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/IBRA7_regions_states.shp") %>% 
  st_transform(3577) %>% select("OBJECTID", "STA_CODE", "REG_NAME_7")

#koala habitat and range map
koalarangedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Koala_Commonwealth/snes_public_grids_08Aug2019 _filegeodatabase/snes_public.gdb"
koalahabdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Habitat_harmonised/Datasets/Harmonised_koala_habitat_v1.gpkg"

###################
#load koala polygons (value=1=likely, value=2=may occur)
krange_pol <- st_read(koalarangedir, layer='koala') %>% st_transform(3577) %>% mutate(habitat_rank = case_when(pres_rank==2 ~ 1,
                                                                                                               pres_rank==1 ~ 2)) %>% select(habitat_rank)
khab_pol <- st_read(koalahabdir) %>% mutate(habitat_rank = case_when(habitat_present_likely==1 ~ 1,
                                                                     habitat_present_possible==1 ~ 2)) %>% select(habitat_rank)

#####################
#load land clearing dataset
defor_rast <- raster(deforestationdir)
#crop to extent of koala range
defor_rast <- crop(defor_rast, krange_pol, snap='out', filename="temp/Ward_cropped", format='GTiff', datatype='INT4S', overwrite=TRUE)
#replace zeros with NAs
defor_rast <- clamp(defor_rast, lower=1, upper=115, filename="temp/Ward_clamped", format='GTiff', datatype='INT4S', overwrite=TRUE)
#create template
rtemp <- raster(extent(defor_rast), res=res(defor_rast))

#####################
#rasterize ibra
ibra_rast <- fasterize(ibra, rtemp, field = "OBJECTID")
writeRaster(ibra_rast, filename="temp/ibra.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
rm(ibra_rast)
#if writeRaster fails, run gc() or rm() or memory.limit()
ibra_rast <- raster("temp/ibra.tif")

#####################
#rasterize koala
k_rast1 <- fasterize(krange_pol, rtemp, field='habitat_rank')
writeRaster(k_rast1, filename="temp/krange.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
rm(k_rast1)
k_rast2 <- fasterize(khab_pol, rtemp, field='habitat_rank')
writeRaster(k_rast2, filename="temp/khab.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
rm(k_rast2)

k_rast1 <- raster("temp/krange.tif")
k_rast2 <- raster("temp/khab.tif")
k_rast <- stack(k_rast1, k_rast2*10)

#combine koala layers
koala_rast <- calc(k_rast, sum, na.rm=TRUE, filename="temp/koala_rast.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
subsdf <- data.frame(from=c(1,10,12,11,21,2,20,22),to=c(1,1,1,1,1,2,2,2))
koala_rast <- raster::subs(koala_rast, subsdf, by="from", which="to", filename="temp/koala_range_combined.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
rm(k_rast)
rm(k_rast1)
rm(k_rast2)

######################
#make zonal raster by combining ibra and koala and deforestation raster
zonal_rast <- overlay(koala_rast, ibra_rast, defor_rast, fun=function(x, y, z){x*1000000 + 1000*y + z}, na.rm=TRUE, filename="temp/zones.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)

#Load lulc raster
lulc_rast <- raster(landusedir)
#extract separate rasters for different land uses


#extract values from land use raster
t <- zonal(lulc_rast, zonal_rast, fun='count', na.rm=TRUE)
