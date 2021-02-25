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
                                                                     habitat_present_possible==1 ~ 2)) #%>% select(habitat_rank)

#####################
#load land clearing dataset
defor_rast <- raster(deforestationdir)
#crop to extent of koala range
defor_rast <- crop(defor_rast, krange_pol, snap='out', filename="temp/Ward_cropped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)
#replace zeros with NAs
defor_rast <- reclassify(defor_rast, cbind(-Inf, 0, NA), right=TRUE, filename="temp/Ward_clamped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)
#create template
rtemp <- raster(extent(defor_rast), res=res(defor_rast), crs=crs(defor_rast))
defor_rast <- raster("temp/Ward_clamped.tif")

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
koala_rast <- raster("temp/koala_range_combined.tif")
crs(koala_rast) <- crs(ibra)
#####################
#Calculate the area of koala RANGE in each bioregion
bioreg_likely <- zonal(match(koala_rast, 1), ibra_rast, fun='count')
bioreg_likely <- data.frame(bioreg_likely)
names(bioreg_likely)[2] <- "likely_range_ha"
bioreg_may <- zonal(match(koala_rast, 2), ibra_rast, fun='count')
bioreg_may <- data.frame(bioreg_may)
names(bioreg_may)[2] <- "may_range_ha"

#Calculate the area of koala HABITAT in each bioregion
#extract bioregion for each 100ha habitat polygon
bioreg_id <- exact_extract(ibra_rast, khab_pol, fun='majority')
khab_df <- khab_pol %>% st_set_geometry(NULL) %>% 
                      mutate(bioregion_id = bioreg_id)
khab_df <- khab_df %>% left_join(st_set_geometry(ibra,NULL), by=c("bioregion_id"="OBJECTID"))
write_csv(khab_df, "output/Harmonised_Koala_Habitat_v1_bioregions.csv")
#summarise habitat area by bioregion
bioreg_hab <- khab_df %>% group_by(bioregion_id) %>%
                      summarise(likely_habitat_ha = round(sum(habitat_ha_likely), 0),
                                may_habitat_ha = round(sum(habitat_ha_possible), 0))

#merge the datasets
bioreg_df <- ibra %>% st_set_geometry(NULL) %>% 
                      right_join(bioreg_likely, by=c("OBJECTID"="zone")) %>%
                      left_join(bioreg_may, by=c("OBJECTID"="zone")) %>%
                      left_join(bioreg_hab, by=c("OBJECTID"="bioregion_id")) 
write_csv(bioreg_df, "output/bioregion_habitat.csv")

######################
#make zonal raster by combining ibra and koala and deforestation raster
zonal_rast <- overlay(koala_rast, ibra_rast, defor_rast, fun=function(x, y, z){x*1000000 + 1000*y + z}, na.rm=FALSE, filename="temp/zones2.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
#first digits = 1 if likely habitat, 2 if possible habitat
#next three digits = IBRA objectid
#next three digits = deforestation year id

######################
#extract values from land use raster
#Load lulc raster
lulc_rast <- raster(landusedir)
lulc_rast <- crop(lulc_rast, zonal_rast, snap='out', filename="temp/CLUM_cropped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

#resample zonal rast to match lulc_rast
zonal_rast <- disaggregate(zonal_rast, fact=2, method='', filename="temp/zones3.tif", format="GTiff", datatype='INT4S', overwrite=TRUE) 

#################################
rm(ibra_rast)
rm(khab_pol)
rm(krange_pol)
rm(defor_rast)
rm(rtemp)
rm(koala_rast)
rm(subsdf)
zonal_rast <- raster("temp/zones3.tif")
lulc_rast <- raster("temp/CLUM_cropped.tif")

#extract separate vectors for different land uses
#value = number of 50m cells
cropping <- zonal(match(lulc_rast, c(330:353, 365, 430:454, 510:515, 520:528)), zonal_rast, fun='count', na.rm=TRUE)
grazing <- zonal(match(lulc_rast, c(210, 320:325, 420:424)), zonal_rast, fun='count', na.rm=TRUE)
urban <- zonal(match(lulc_rast, c(530:538, 540:545, 550:555, 567, 570:575, 590:595)), zonal_rast, fun='count', na.rm=TRUE)
forestry <- zonal(match(lulc_rast, c(220:222, 310:314, 410:414)), zonal_rast, fun='count', na.rm=TRUE)
energy <- zonal(match(lulc_rast, c(560:566, 580:584)), zonal_rast, fun='count', na.rm=TRUE)

write.csv(cropping, "temp/cropping.csv")
write.csv(grazing, "temp/grazing.csv")
write.csv(urban, "temp/urban.csv")
write.csv(forestry, "temp/forestry.csv")
write.csv(energy, "temp/energy.csv")

#################################
cropping <- read_csv("temp/cropping.csv")[,2:3]
grazing <- read_csv("temp/grazing.csv")[,2:3]
energy <- read_csv("temp/energy.csv")[,2:3]
forestry <- read_csv("temp/forestry.csv")[,2:3]
urban <- read_csv("temp/urban.csv")[,2:3]
#################################
#Combine and format data
df <- cropping %>% left_join(grazing, by='zone') %>% 
                    left_join(urban, by='zone') %>%
                    left_join(forestry, by='zone') %>%
                    left_join(energy, by='zone')
names(df) <- c("zone", "cropping", "grazing", "urban", "forestry", "energy")

#disaggreage zonal code
df <- df %>% mutate(koala_id = as.integer(substr(zone, 1, 1)),
                    ibra_id = as.integer(substr(zone, 2, 4)),
                    defor_id = as.integer(substr(zone, 5, 7)))
#match zone codes back to values
df <- df %>% left_join(st_set_geometry(ibra, NULL), by=c("ibra_id"="OBJECTID"))
defor_yrs <- data.frame(value=c(1:15, 101:115), defor_yr=rep(c("2000-2002", "2002-2004","2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"), times=2))
df <- df %>% left_join(defor_yrs, by=c("defor_id"="value"))
df <- df %>% mutate(koala_habitat = case_when(koala_id==1 ~ "likely",
                                              koala_id==2 ~ "may"))
#fill in empty rows (years & bioregions with no clearing)
df <- df %>% complete(nesting(STA_CODE, REG_NAME_7), defor_yr, koala_habitat, fill=list(cropping=0, grazing=0, urban=0, forestry=0, energy=0))

#convert ncells to hectares
df <- df %>% mutate(cropping_ha = cropping*50*50/10000,
                    grazing_ha = grazing*50*50/10000,
                    urban_ha = urban*50*50/10000,
                    forestry_ha = forestry*50*50/10000,
                    energy_ha = energy*50*50/10000)

write_csv(df, "output/Koala_Clearing_by_landuse_and_year.csv")

######################################
#Summarise data for 2012-2017 clearing
df <- read_csv("output/Koala_Clearing_by_landuse_and_year.csv")
#summarise by bioregion
df_s1 <- df %>% filter(koala_habitat=="likely" & STA_CODE!="VIC") %>%
              filter(defor_yr %in% c("2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017")) %>% 
              group_by(STA_CODE, REG_NAME_7, koala_habitat) %>%
              summarise(grazing_ha = sum(grazing_ha),
                        cropping_ha = sum(cropping_ha),
                        urban_ha = sum(urban_ha),
                        forestry_ha = sum(forestry_ha),
                        energy_ha = sum(energy_ha)) %>% ungroup() %>%
              mutate(total_ha_cleared = rowSums(across(grazing_ha:energy_ha))) %>%
              left_join(bioreg_df[,c("STA_CODE", "REG_NAME_7", "likely_habitat_ha")], by=c("STA_CODE"="STA_CODE", "REG_NAME_7"="REG_NAME_7")) %>%
              mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
                     percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
                     percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
                     percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
                     percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
              mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
                     percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
                     percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
                     percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
                     percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))
  write_csv(df_s1, "Koala_clearing_stats_by_bioregion_2012to2017.csv")     

#summarise by state
df_s2 <- df_s1 %>% group_by(STA_CODE, koala_habitat) %>%
              summarise(grazing_ha = sum(grazing_ha),
                        cropping_ha = sum(cropping_ha),
                        urban_ha = sum(urban_ha),
                        forestry_ha = sum(forestry_ha),
                        energy_ha = sum(energy_ha),
                        total_ha_cleared = sum(total_ha_cleared),
                        likely_habitat_ha = sum(likely_habitat_ha)) %>% ungroup() %>%
          mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
                 percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
                 percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
                 percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
                 percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
            mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
                   percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
                   percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
                   percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
                   percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))
write_csv(df_s2, "Koala_clearing_stats_by_state_2012to2017.csv")     


#summarise by listed koala
df_s3 <- df_s1 %>% group_by(koala_habitat) %>%
              summarise(grazing_ha = sum(grazing_ha),
                        cropping_ha = sum(cropping_ha),
                        urban_ha = sum(urban_ha),
                        forestry_ha = sum(forestry_ha),
                        energy_ha = sum(energy_ha),
                        total_ha_cleared = sum(total_ha_cleared),
                        likely_habitat_ha = sum(likely_habitat_ha)) %>% ungroup() %>%
  mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
         percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
         percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
         percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
         percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
  mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))

write_csv(df_s3, "Koala_clearing_stats_listedKoala_2012to2017.csv")     

######################################
#Summarise data
#summarise by bioregion
df_s1 <- df %>% filter(koala_habitat=="likely" & STA_CODE!="VIC") %>% group_by(STA_CODE, REG_NAME_7, koala_habitat) %>%
  summarise(grazing_ha = sum(grazing_ha),
            cropping_ha = sum(cropping_ha),
            urban_ha = sum(urban_ha),
            forestry_ha = sum(forestry_ha),
            energy_ha = sum(energy_ha)) %>% ungroup() %>%
  mutate(total_ha_cleared = rowSums(across(grazing_ha:energy_ha))) %>%
  left_join(bioreg_df[,c("STA_CODE", "REG_NAME_7", "likely_habitat_ha")], by=c("STA_CODE"="STA_CODE", "REG_NAME_7"="REG_NAME_7")) %>%
  mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
         percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
         percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
         percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
         percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
  mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))
write_csv(df_s1, "Koala_clearing_stats_by_bioregion.csv")     

#summarise by state
df_s2 <- df_s1 %>% group_by(STA_CODE, koala_habitat) %>%
  summarise(grazing_ha = sum(grazing_ha),
            cropping_ha = sum(cropping_ha),
            urban_ha = sum(urban_ha),
            forestry_ha = sum(forestry_ha),
            energy_ha = sum(energy_ha),
            total_ha_cleared = sum(total_ha_cleared),
            likely_habitat_ha = sum(likely_habitat_ha)) %>% ungroup() %>%
  mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
         percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
         percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
         percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
         percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
  mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))
write_csv(df_s2, "Koala_clearing_stats_by_state.csv")     


#summarise by listed koala
df_s3 <- df_s1 %>% group_by(koala_habitat) %>%
  summarise(grazing_ha = sum(grazing_ha),
            cropping_ha = sum(cropping_ha),
            urban_ha = sum(urban_ha),
            forestry_ha = sum(forestry_ha),
            energy_ha = sum(energy_ha),
            total_ha_cleared = sum(total_ha_cleared),
            likely_habitat_ha = sum(likely_habitat_ha)) %>% ungroup() %>%
  mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
         percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
         percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
         percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
         percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
  mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + total_ha_cleared), 4),
         percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + total_ha_cleared), 4))

write_csv(df_s3, "Koala_clearing_stats_listedKoala.csv")     

######################################
#Summarise data for may and likely habitat
#summarise by bioregion
df_s1 <- df %>% filter(STA_CODE!="VIC") %>% group_by(STA_CODE, REG_NAME_7) %>%
  summarise(grazing_ha = sum(grazing_ha),
            cropping_ha = sum(cropping_ha),
            urban_ha = sum(urban_ha),
            forestry_ha = sum(forestry_ha),
            energy_ha = sum(energy_ha)) %>% ungroup() %>%
  mutate(total_ha_cleared = rowSums(across(grazing_ha:energy_ha))) %>%
  left_join(bioreg_df[,c("STA_CODE", "REG_NAME_7", "likely_habitat_ha", "may_habitat_ha")], by=c("STA_CODE"="STA_CODE", "REG_NAME_7"="REG_NAME_7")) %>%
  mutate(percofclearing_grazing = round(100*grazing_ha/total_ha_cleared, 2),
         percofclearing_cropping = round(100*cropping_ha/total_ha_cleared, 2),
         percofclearing_urban = round(100*urban_ha/total_ha_cleared, 2),
         percofclearing_forestry = round(100*forestry_ha/total_ha_cleared, 2),
         percofclearing_energy = round(100*energy_ha/total_ha_cleared, 2)) %>%
  mutate(percofhabitat_grazing = round(100*grazing_ha/(likely_habitat_ha + may_habitat_ha + total_ha_cleared), 4),
         percofhabitat_cropping = round(100*cropping_ha/(likely_habitat_ha + may_habitat_ha + total_ha_cleared), 4),
         percofhabitat_urban = round(100*urban_ha/(likely_habitat_ha + may_habitat_ha + total_ha_cleared), 4),
         percofhabitat_forestry = round(100*forestry_ha/(likely_habitat_ha + may_habitat_ha + total_ha_cleared), 4),
         percofhabitat_energy = round(100*energy_ha/(likely_habitat_ha + may_habitat_ha + total_ha_cleared), 4))
write_csv(df_s1, "Koala_clearing_stats_by_bioregion_mayorlikely.csv")     

