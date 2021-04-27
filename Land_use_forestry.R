#This script calculates the area of koala habitat in different types of forestry

library(raster)
library(sf)
library(tidyverse)
require(rgeos)
require(rgdal)
require(fasterize)
require(exactextractr)

setwd("D:/Box Sync/DAWE/Land_use_change")

#LAND USE Catchment Scale Land Use of Australia - Update December 2018
landusedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Land_cover/ACLUMP/geotiff_clum_50m1218m/geotiff_clum_50m1218m/clum_50m1218m.tif"
#rasters are in GDA_94_Albers EPGS:3577

#IBRA7 bioregions
ibra <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/IBRA7_regions_states.shp") %>% 
  st_transform(3577) %>% select("OBJECTID", "STA_CODE", "REG_NAME_7")

#koala habitat and range map #listed koala only
#for entire koala range including vic and sa use Phascolarctos_cinereus_197.shp
#krange_pol <- st_read("D:/Box Sync/DAWE/Land_use_change/Koala_SDM/Phascolarctos_cinereus_85104.shp") %>% 
#  st_transform(3577)

#krange_pol <- krange_pol %>% mutate(habitat_rank = case_when(KLM %in% c(26, 36) ~ 1, KLM==46 ~ 2)) %>% select(habitat_rank)

# #####################
# #rasterize ibra
# ibra_rast <- fasterize(ibra, rtemp, field = "OBJECTID")
# writeRaster(ibra_rast, filename="temp/ibra.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
# rm(ibra_rast)
# #if writeRaster fails, run gc() or rm() or memory.limit()
 ibra_rast <- raster("temp/ibra.tif")
# 
# #####################
# #rasterize koala
# k_rast1 <- fasterize(krange_pol, rtemp, field='habitat_rank')
# writeRaster(k_rast1, filename="temp/krange.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
# rm(k_rast1)
 koala_rast <- raster("temp/krange.tif")
 crs(koala_rast) <- crs(ibra)

#####################
zonal_rast <- overlay(koala_rast, ibra_rast, fun=function(x, y){x*1000 + y}, na.rm=FALSE, filename="temp/zones_forest1.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
#first digits = 1 if likely habitat, 2 if possible habitat
#next three digits = IBRA objectid
#resample zonal rast to match lulc_rast
zonal_rast <- disaggregate(zonal_rast, fact=2, method='', filename="temp/zones_forest2.tif", format="GTiff", datatype='INT4S', overwrite=TRUE) 

#####################
#make land use raster
#Load lulc raster
lulc_rast <- raster(landusedir)
lulc_rast <- crop(lulc_rast, zonal_rast, snap='out', filename="temp/CLUM_cropped_forest.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

#####################
#extract zones corresponding to forestry
zonal_rast <- raster("temp/zones_forest2.tif")
lulc_rast <- raster("temp/CLUM_cropped_forest.tif")

forestry <- zonal(match(lulc_rast, c(310:314, 410:414)), zonal_rast, fun='count', na.rm=TRUE)
forestrynative <- zonal(match(lulc_rast, c(220:222)), zonal_rast, fun='count', na.rm=TRUE)

write.csv(forestry, "Forestry/forestry_in_krange.csv")
write.csv(forestrynative, "Forestry/forestrynative_in_krange.csv")

######################
##Combine and format data

forestry <- read_csv("Forestry/forestry_in_krange.csv")[,2:3]
forestrynative <- read_csv("Forestry/forestrynative_in_krange.csv")[,2:3]

df <- forestry %>% left_join(forestrynative, by='zone')  
names(df) <- c("zone", "forestry", "forestrynative")

df <- df %>% mutate(koala_id = as.integer(substr(zone, 1, 1)),
                    ibra_id = as.integer(substr(zone, 2, 4)))

#match zone codes back to values
df <- df %>% left_join(st_set_geometry(ibra, NULL), by=c("ibra_id"="OBJECTID"))
df <- df %>% mutate(koala_habitat = case_when(koala_id==1 ~ "likely",
                                              koala_id==2 ~ "may"))


#fill in empty rows (years & bioregions with no clearing)
df <- df %>% complete(nesting(STA_CODE, REG_NAME_7), koala_habitat, fill=list(forestry=0, forestrynative=0))

#convert ncells to hectares
df <- df %>% mutate(forestry_ha = forestry*50*50/10000,
                    forestrynative_ha = forestrynative*50*50/10000)
write_csv(df, "Forestry/Koala_range_in_landuse_forestry.csv")

#################################
#Summarise data

bioreg_df <- read_csv("output/V5_separatenativeforest/bioregion_habitat.csv")

df_s1 <- df %>% filter(koala_habitat=="likely" & STA_CODE!="VIC") %>%
  group_by(STA_CODE, REG_NAME_7, koala_habitat) %>%
  summarise(plantation_forestry_ha = sum(forestry_ha),
            native_forestry_ha = sum(forestrynative_ha)) %>% ungroup() %>%
  left_join(bioreg_df[,c("STA_CODE", "REG_NAME_7", "likely_range_ha", "likely_habitat_ha")], by=c("STA_CODE"="STA_CODE", "REG_NAME_7"="REG_NAME_7")) 
 
write_csv(df_s1, "Forestry/Koala_range_in_landuse_forestry_summary.csv")

#################################
