#this script extracts landscape values and runs clustering


#################
library(sf)
library(raster)
library(slga) #for download of Soil and Landscape Grid of Australia https://www.clw.csiro.au/aclep/soilandlandscapegrid/GetData-R_package.html
library(tidyverse)


#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
datadir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/"
oupdir <- "Gridded_data/"
bbdir <- paste0(datadir, "Bioregions/koala_IBRA7/")
waterdir <- paste0(datadir, "Barriers/Rivers/Aust/")
firedir <- paste0(datadir, "Fire/National_fire_freq/ff_88to152.tif")


source(paste0(dirname(getwd()), "/R_scripts/koala_pankr/extractfun.r"))
source(paste0(dirname(getwd()), "/R_scripts/koala_pankr/makegridfun.r"))

cell_diameter = 1000 #metres

######################
#load study region and project to GDA_94 Aust Albers EPSG 3577 which has units in m
######################
bb <- st_read(paste0(bbdir, "IBRA7_regions_states_koala_dissolve.shp"))
bb <- st_transform(bb, 3577)

#make hexagons
if(file.exists(paste0(oupdir, "koala_templatehexgrid_",cell_diameter,"_m.Rdata"))==FALSE){
  k_grid <- makegridfun(bb, cell_diameter) 
  save(k_grid, file = paste0(oupdir, "koala_templatehexgrid_",cell_diameter,"_m.Rdata"))
  st_write(k_grid, paste0(oupdir, "koala_templatehexgrid_",cell_diameter,"_m.shp"), driver='ESRI Shapefile')
} else {
  load(paste0(oupdir, "koala_templatehexgrid_",cell_diameter,"_m.Rdata"))
}

######################
##Extract attributes for datasets that span the whole study region
######################
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))==FALSE){
  
#load and extract koala occurrence
  current_koala <- st_read(paste0(datadir, "Occurrence records"), "Koala_Qld_NSW_merge_2000on_1kmres_noDup") %>% 
    st_transform(3577) %>%
    st_buffer(1000)
  historic_koala  <- st_read(paste0(datadir, "Occurrence records"), "Koala_Qld_NSW_merge_1970to2000_1kmres_noDup") %>% 
    st_transform(3577) %>% #GDA94_Albers
    st_buffer(1000)
  
  k_grid <- k_grid %>% bind_cols(current_koala = lengths(st_intersects(k_grid, current_koala, sparse=TRUE)), 
                                 historic_koala = lengths(st_intersects(k_grid, historic_koala, sparse=TRUE))) %>% st_sf()

    save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(current_koala)
  rm(historic_koala)
  
#load and extract plant available water content
  k_grid <- k_grid %>% st_transform(4283) #GDA94
  
  pawc <- get_soils_data(product='NAT', attribute='AWC', component='VAL', depth=1, aoi=k_grid) 
  pawc_mean <- raster::extract(pawc, k_grid, fun=mean, na.rm=TRUE)
  pawc_max <- raster::extract(pawc, k_grid, fun=max, na.rm=TRUE)
  k_grid<- k_grid %>% bind_cols(pawc_mean = pawc_mean, pawc_max = pawc_max) %>% st_sf()

  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(pawc)
  
#load and extract soil depth
  soildepth <- get_soils_data(product='NAT', attribute='DES', component='VAL', aoi=bb) 
  k_grid$soildepth_mean <- raster::extract(soildepth, k_grid, fun='mean', na.rm=TRUE)
  k_grid$soildepth_max <- raster::extract(soildepth, k_grid, fun='max', na.rm=TRUE)
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(soildepth)
  
#load and extract bushfire freq
  k_grid <- k_grid %>% st_transform(4326) #WGS84
  firefreq <- raster(firedir)
  k_grid$firefreq_88to15 <- raster::extract(firefreq, k_grid, fun='max', na.rm=TRUE)
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  
#load and extract permanent water
  k_grid <- k_grid %>% st_transform(4283) #GDA94
  water <- st_read(paste0(waterdir, "SurfaceHydrologyPolygonsNational.gdb"))
  water <- water %>% filter(PERENNIALITY=='Perennial')
  k_grid$permanent_water <- lengths(st_intersects(k_grid, water))
  k_grid$dist2water <- st_distance(k_grid, st_combine(water))
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  st_write(k_grid, paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.shp"), driver='ESRI Shapefile')
  rm(water)
  
  } else {
  load(paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
}
head(k_grid)

######################
#Extract climate data
######################
if(file.exists(paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))==FALSE){
  #climstack <- stack(list.files("Climrasters_thresholded", pattern="No_duplicates", recursive = TRUE, full.names = TRUE))
  climstack <- stack(list.files("Climrasters_thresholded", pattern="No_duplicates", recursive = TRUE, full.names = TRUE)) 
  #1=50% 2=80% 3=90% 4=95%
  clim_data <- raster::extract(climstack, k_grid, weights=TRUE, fun='mean', na.rm=TRUE)
 
  head(clim_data)
  k_grid <- k_grid %>% bind_cols(climdata)
  
  save(k_grid, file = paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))
} else {
  load(paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))
}
head(k_grid)

######################
#Extract habitat data
######################
regions <- st_read(bbdir, "IBRA7_regions_states_koala.shp")

if()
  k_nsw <- extracthabitat(k_vars_clim, ,) 
  k_
  
  
  
  paste0("koala_gridded_vars_", curregion, cell_diameter,"_m.Rdata"))
  
  
  
  #check
  head(k_all)
  save(k_all, file = paste0("koala_gridded_allvars__m.Rdata"))
###END








