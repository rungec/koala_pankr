library(raster)
library(sf)
library(tidyverse)
require(rgeos)
require(rgdal)
require(fasterize)
require(exactextractr)

setwd("D:/Box Sync/DAWE/Land_use_change")


#IBRA7 bioregions
ibra <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/IBRA7_regions_states.shp") %>% 
  st_transform(3577) %>% select("OBJECTID", "STA_CODE", "REG_NAME_7")

#koala habitat and range map
koalarangedir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Koala_Commonwealth/snes_public_grids_08Aug2019 _filegeodatabase/snes_public.gdb"
koalahabdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Habitat_harmonised/Datasets/Harmonised_koala_habitat_v1.gpkg"

#tenure of forests
#Australian Bureau of Agricultural and Resource Economics and Sciences,
#Tenure of Australia's forests (2018), Australian Bureau of Agricultural and
#Resource Economics and Sciences, Canberra, December. CC BY 4.0
tenure <- "D:/Box Sync/DAWE/Land_use_change/Forest_tenure/aus_forten181.tif"
tenure_lookup <- "D:/Box Sync/DAWE/Land_use_change/Forest_tenure/lookup_tenure.csv"

###################
#load koala polygons (value=1=likely, value=2=may occur)
#krange_pol <- st_read(koalarangedir, layer='koala') %>% st_transform(3577) %>% mutate(habitat_rank = case_when(pres_rank==2 ~ 1,
                                                                                                               pres_rank==1 ~ 2)) %>% select(habitat_rank)
#khab_pol <- st_read(koalahabdir) %>% mutate(habitat_rank = case_when(habitat_present_likely==1 ~ 1,
                                                                     habitat_present_possible==1 ~ 2)) #%>% select(habitat_rank)

####################
#load tenure data
tenu_rast <- raster(tenure)
tenu_lookup <- read_csv(tenure_lookup)

####################
#make a zonal raster
#koala_rast and ibra_rast were generated in Land_clearing_summary.r
koala_rast <- raster("temp/khab.tif") #1=known or likely, 2=possible. From Harmonised Koala Habitat Map v1.0
ibra_rast <- raster("temp/ibra.tif") #value = OBJECTID of IBRA7

zonal_rast <- overlay(koala_rast, ibra_rast, fun=function(x, y){x*1000 + y}, na.rm=FALSE, filename="temp/zones4.tif", format="GTiff", datatype='INT4S')
#first digit = koala habitat
#next three digits = ibra OBJECTID

####################
#extract the land tenure in koala habitat and bioregion
#crop tenure raster to make it go faster
tenu_rast <- crop(tenu_rast, zonal_rast, snap='out', filename="temp/tenure_cropped.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)

#extract by tenure. See Tenure_of_Australias_forests_2018_Lineage.pdf for tenure definitions
priv <- zonal(match(tenu_rast, c(tenu_lookup$VALUE[tenu_lookup$TEN_TYPE=="PRIV"])), zonal_rast, fun='count', na.rm=TRUE)
lease <- zonal(match(tenu_rast, c(tenu_lookup$VALUE[tenu_lookup$TEN_TYPE=="LEASE"])), zonal_rast, fun='count', na.rm=TRUE)
muf <- zonal(match(tenu_rast, c(tenu_lookup$VALUE[tenu_lookup$TEN_TYPE=="MUF"])), zonal_rast, fun='count', na.rm=TRUE)
ncr <- zonal(match(tenu_rast, c(tenu_lookup$VALUE[tenu_lookup$TEN_TYPE=="NCR"])), zonal_rast, fun='count', na.rm=TRUE)
ocl <- zonal(match(tenu_rast, c(tenu_lookup$VALUE[tenu_lookup$TEN_TYPE=="OCL"])), zonal_rast, fun='count', na.rm=TRUE)

write.csv(priv, "temp/priv.csv")
write.csv(lease, "temp/lease.csv")
write.csv(muf, "temp/muf.csv")
write.csv(ncr, "temp/ncr.csv")
write.csv(ocl, "temp/ocl.csv")

####################
#make a layer intersecting crown land and koala habitat
tenu_rast <- crop(tenu_rast, koala_rast, snap='out', filename="temp/tenure_cropped_khab.tif", format='GTiff', datatype='INT4S', overwrite=TRUE)
crown_rast <- overlay(koala_rast, tenu_rast, fun=function(x, y){x*10000 + y}, na.rm=FALSE, filename="output/crownland_koalahabitat.tif", format="GTiff", datatype='INT4S', overwrite=TRUE)
#first digit = koala habitat
#next three digits = ibra OBJECTID


#################################
priv <- read_csv("temp/priv.csv")[2:3]
lease <- read_csv("temp/lease.csv")[2:3]
muf <- read_csv("temp/muf.csv")[2:3]
ncr <- read_csv("temp/ncr.csv")[2:3]
ocl <- read_csv("temp/ocl.csv")[2:3]

################################
#Combine and format data
df <- priv %>% left_join(lease, by='zone') %>% 
  left_join(muf, by='zone') %>%
  left_join(ncr, by='zone') %>%
  left_join(ocl, by='zone')
names(df) <- c("zone", "priv_ha", "lease_ha", "muf_ha", "ncr_ha", "ocl_ha")
#as cells of underlying data = 100m2 ==1ha

#disaggreage zonal code
df <- df %>% mutate(koala_id = as.integer(substr(zone, 1, 1)),
                    ibra_id = as.integer(substr(zone, 2, 4)))

#match zone codes back to values
df <- df %>% left_join(st_set_geometry(ibra, NULL), by=c("ibra_id"="OBJECTID"))
df <- df %>% mutate(koala_habitat = case_when(koala_id==1 ~ "likely",
                                              koala_id==2 ~ "may"))

#save
write_csv(df, "output/Land_tenure_of_koala_habitat_bybioregion.csv")

###############################
#summarise by state and calculate % koala habitat in each tenure
df_s1 <- df %>% filter(STA_CODE %in% c("QLD", "NSW", "ACT") & koala_habitat=='likely') %>% 
            group_by(STA_CODE) %>%
            summarise(priv_ha = sum(priv_ha),
                      lease_ha = sum(lease_ha),
                      muf_ha = sum(muf_ha),
                      ncr_ha = sum(ncr_ha),
                      ocl_ha = sum(ocl_ha)) %>%
            mutate(perc_priv = 100*priv_ha/rowSums(across(priv_ha:ocl_ha)),
                   perc_lease = 100*lease_ha/rowSums(across(priv_ha:ocl_ha)),
                   perc_muf = 100*muf_ha/rowSums(across(priv_ha:ocl_ha)),
                   perc_ncr = 100*ncr_ha/rowSums(across(priv_ha:ocl_ha)),
                   perc_ocl = 100*ocl_ha/rowSums(across(priv_ha:ocl_ha)))
              

#save
write_csv(df_s1, "output/Land_tenure_of_koala_habitat_bystate.csv")





