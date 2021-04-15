library(raster)
library(sf)
library(tidyverse)
require(rgeos)
require(rgdal)
require(fasterize)
require(exactextractr)

setwd("D:/Box Sync/DAWE/Land_use_change")

#FROM WARD et al. 2019
deforestationdir <- "D:/Box Sync/DAWE/Land_use_change/Ward_data/FL100_FWLepoch_3msk_v2.tif.aux/FL100_FWLepoch_3msk_v2.tif"
#In order, the attributes represent the following 15 periods of change:  2000-2002, 2002-2004, 
#2004-2005, 2005-2006, 2006-2007, 2007-2008, 2008-2009, 2009-2010, 2010-2011, 2011-2012, 
#2012-2013, 2013-2014, 2014-2015, 2015-2016 and 2016-2017. 
#LAND USE Catchment Scale Land Use of Australia - Update December 2018
landusedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Land_cover/ACLUMP/geotiff_clum_50m1218m/geotiff_clum_50m1218m/clum_50m1218m.tif"
#rasters are in GDA_94_Albers EPGS:3577

#koala habitat and range map
ggrange_pol <- st_read("D:/Box Sync/DAWE/Land_use_change/Koala_SDM/Greater_glider_snes_Aug2019.shp") %>% 
  st_transform(3577)

###################
#load koala polygons (value=1=likely, value=2=may occur)
ggrange_pol <- ggrange_pol %>% mutate(habitat_rank = case_when(pres_rank==2 ~ 1,
                                                             pres_rank==1 ~ 2)) %>% select(habitat_rank)

#####################
#load land clearing dataset
defor_rast <- raster(deforestationdir)
#crop to extent of koala range
defor_rast <- crop(defor_rast, ggrange_pol, snap='out', filename="temp/Ward_cropped_greaterglider.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)
#replace zeros with NAs
defor_rast <- reclassify(defor_rast, cbind(-Inf, 0, NA), right=TRUE, filename="temp/Ward_clamped_greaterglider.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)
#create template
rtemp <- raster(extent(defor_rast), res=res(defor_rast), crs=crs(defor_rast))
#defor_rast <- raster("temp/Ward_clamped_greaterglider.tif")

#####################
#rasterize great glider range
gg_rast1 <- fasterize(ggrange_pol, rtemp, field='habitat_rank')
writeRaster(gg_rast1, filename="temp/ggrange.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
rm(gg_rast1)

glider_rast <- raster("temp/ggrange.tif")
crs(glider_rast) <- crs(defor_rast)
#####################
#make zonal raster by combining glider and deforestation raster
zonal_rast <- overlay(glider_rast, defor_rast, fun=function(x, y){x*1000 + y}, na.rm=FALSE, filename="temp/gliderzones.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
#first digits = 1 if likely habitat, 2 if possible habitat
#next three digits = deforestation year id

######################
#extract values from land use raster
#Load lulc raster
lulc_rast <- raster(landusedir)
lulc_rast <- crop(lulc_rast, zonal_rast, snap='out', filename="temp/CLUM_cropped_glider.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

#resample zonal rast to match lulc_rast
zonal_rast <- disaggregate(zonal_rast, fact=2, method='', filename="temp/gliderzones3.tif", format="GTiff", datatype='INT4S', overwrite=TRUE) 

#################################
rm(ggrange_pol)
rm(defor_rast)
rm(rtemp)
rm(glider_rast)
zonal_rast <- raster("temp/gliderzones3.tif")
lulc_rast <- raster("temp/CLUM_cropped_glider.tif")

#extract separate vectors for different land uses
#value = number of 50m cells
cropping <- zonal(match(lulc_rast, c(330:353, 365, 430:454, 510:515, 520:528)), zonal_rast, fun='count', na.rm=TRUE)
grazing <- zonal(match(lulc_rast, c(210, 320:325, 420:424)), zonal_rast, fun='count', na.rm=TRUE)
urban <- zonal(match(lulc_rast, c(530:538, 540:545, 550:555, 567, 570:575, 590:595)), zonal_rast, fun='count', na.rm=TRUE)
forestry <- zonal(match(lulc_rast, c(220:222, 310:314, 410:414)), zonal_rast, fun='count', na.rm=TRUE)
energy <- zonal(match(lulc_rast, c(560:566, 580:584)), zonal_rast, fun='count', na.rm=TRUE)

write.csv(cropping, "temp/cropping_glider.csv")
write.csv(grazing, "temp/grazing_glider.csv")
write.csv(urban, "temp/urban_glider.csv")
write.csv(forestry, "temp/forestry_glider.csv")
write.csv(energy, "temp/energy_glider.csv")

#################################
cropping <- read_csv("temp/cropping_glider.csv")[,2:3]
grazing <- read_csv("temp/grazing_glider.csv")[,2:3]
energy <- read_csv("temp/energy_glider.csv")[,2:3]
forestry <- read_csv("temp/forestry_glider.csv")[,2:3]
urban <- read_csv("temp/urban_glider.csv")[,2:3]
#################################
#Combine and format data
df <- cropping %>% left_join(grazing, by='zone') %>% 
                    left_join(urban, by='zone') %>%
                    left_join(forestry, by='zone') %>%
                    left_join(energy, by='zone')
names(df) <- c("zone", "cropping", "grazing", "urban", "forestry", "energy")

#disaggreage zonal code
df <- df %>% mutate(glider_id = as.integer(substr(zone, 1, 1)),
                    defor_id = as.integer(substr(zone, 2, 4)))
#match zone codes back to values
#df <- df %>% left_join(st_set_geometry(ibra, NULL), by=c("ibra_id"="OBJECTID"))
defor_yrs <- data.frame(value=c(1:15, 101:115), defor_yr=rep(c("2000-2002", "2002-2004","2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"), times=2))
df <- df %>% left_join(defor_yrs, by=c("defor_id"="value"))
df <- df %>% mutate(glider_habitat = case_when(glider_id==1 ~ "likely",
                                              glider_id==2 ~ "may"))
#fill in empty rows (years & bioregions with no clearing)
#df <- df %>% complete(nesting(STA_CODE, REG_NAME_7), defor_yr, koala_habitat, fill=list(cropping=0, grazing=0, urban=0, forestry=0, energy=0))

#convert ncells to hectares
df <- df %>% mutate(cropping_ha = cropping*50*50/10000,
                    grazing_ha = grazing*50*50/10000,
                    urban_ha = urban*50*50/10000,
                    forestry_ha = forestry*50*50/10000,
                    energy_ha = energy*50*50/10000)

write_csv(df, "output/GreaterGlider_Clearing_by_landuse_and_year.csv")

######################################
#Summarise data
df <- read_csv("output/GreaterGlider_Clearing_by_landuse_and_year.csv")


df_s1 <- df %>% filter(glider_habitat=="likely") %>%
  group_by(glider_habitat, defor_yr) %>%
  summarise(grazing_ha = sum(grazing_ha),
            cropping_ha = sum(cropping_ha),
            urban_ha = sum(urban_ha),
            forestry_ha = sum(forestry_ha),
            energy_ha = sum(energy_ha)) %>% ungroup() %>%
  mutate(total_ha_cleared = rowSums(across(grazing_ha:energy_ha)))

df_s2 <- df_s1 %>% select(!grazing_ha:energy_ha) %>% pivot_wider(names_from = defor_yr, values_from = total_ha_cleared, names_prefix = "area_cleared_ha_") %>%
   mutate(area_cleared_ha_2000_2016 = rowSums(across(`area_cleared_ha_2000-2002`:`area_cleared_ha_2015-2016`)),
          area_cleared_ha_2016_2021 = `area_cleared_ha_2016-2017`*(2021-2016))
write_csv(df_s2, "output/GreaterGlider_clearing_by_year.csv")

######################################