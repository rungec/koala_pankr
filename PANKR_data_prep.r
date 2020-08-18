#this script extracts landscape values and runs clustering


#################
library(sf)
library(tidyverse)
library(raster)
library(slga) #for download of Soil and Landscape Grid of Australia https://www.clw.csiro.au/aclep/soilandlandscapegrid/GetData-R_package.html
library(lwgeom)


#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/")
datadir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/"
oupdir <- "Gridded_data/"
bbdir <- paste0(datadir, "Bioregions/koala_IBRA7/")
waterdir <- paste0(datadir, "Barriers/Rivers/Aust/")
firedir <- paste0(datadir, "Fire/National_fire_freq/ff_88to152.tif")
pawcdir <- paste0(datadir, "Soil/PAWC_1m/PAWC_1m/pawc_1m")


#source(paste0(dirname(getwd()), "/R_scripts/koala_pankr/extractfun.r"))
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
  
  k_grid <- k_grid %>% bind_cols(cellid = c(1:nrow(k_grid)), current_koala = lengths(st_intersects(k_grid, current_koala, sparse=TRUE)), 
                                 historic_koala = lengths(st_intersects(k_grid, historic_koala, sparse=TRUE))) %>% st_sf()

    save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(current_koala)
  rm(historic_koala)
  
#load and extract plant available water content

  pawc <- raster(pawcdir) #250m res, mm/m for top 1m
  #pawc <- get_soils_data(product='NAT', attribute='AWC', component='VAL', depth=1, aoi=k_grid) #90m res and need to download each depth and sum
  pawc_mean <- raster::extract(pawc, k_grid, fun=mean, na.rm=TRUE)
  pawc_max <- raster::extract(pawc, k_grid, fun=max, na.rm=TRUE)
  k_grid <- k_grid %>% st_transform(st_crs(pawc)) 
  k_grid<- k_grid %>% bind_cols(pawc_mean = pawc_mean, pawc_max = pawc_max) %>% st_sf()

  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(pawc)
  rm(pawc_max)
  rm(pawc_mean)
  
#load and extract soil depth
  k_grid <- k_grid %>% st_transform(4283) #GDA94
  soildepth <- get_soils_data(product='NAT', attribute='DES', component='VAL', depth=1, aoi=k_grid) 
  
  soildepth_mean <- raster::extract(soildepth, k_grid, fun=mean, na.rm=TRUE)
  soildepth_max <- raster::extract(soildepth, k_grid, fun=max, na.rm=TRUE)
  k_grid<- k_grid %>% bind_cols(soildepth_mean = soildepth_mean, soildepth_max = soildepth_max) %>% st_sf()
  
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(soildepth)
  rm(soildepth_mean)
  rm(soildepth_max)
  
#load and extract bushfire freq
  firefreq <- raster(firedir)
  k_grid <- k_grid %>% st_transform(st_crs(firefreq)) #WGS84
  firefreq_88to15 <- raster::extract(firefreq, k_grid, fun=max, na.rm=TRUE)
  k_grid<- k_grid %>% bind_cols(firefreq_88to15 = firefreq_88to15) %>% st_sf()
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(firefreq)
  rm(firefreq_88to15)
  
#load and extract permanent water
 #water <- st_read(paste0(waterdir, "SurfaceHydrologyPolygonsNational.gdb")) %>% 
 #             filter(PERENNIALITY=='Perennial') %>% st_transform(3577)
##fix geometry 
 # water <- st_read(paste0(waterdir, "SurfaceHydro_Perennial_dissolve_koala.shp")) %>% 
 #   st_transform(3577)
  #water1 <- st_buffer(water, dist=0)
  #st_write(water1, paste0(waterdir, "SurfaceHydro_Perennial_dissolve_koala_fixgeom.shp"), driver='ESRI Shapefile') 
 
  water <- st_read(paste0(waterdir, "SurfaceHydro_Perennial_dissolve_koala_fixgeom.shp")) %>% 
    st_transform(3577) 
  k_grid <- k_grid %>% st_transform(st_crs(water))
  
  #area of permanent water in cell
  w1 <- st_intersection(k_grid, water) %>% 
    mutate(permanent_water_area_ha= as.numeric(st_area(.)/10000) %>% 
             dplyr::select(cellid, permanent_water_area_ha) %>% as_tibble()
           
           #distance to water
           w2 <- st_distance(k_grid, water)
           
           #join back to dataset
           k_grid<- k_grid %>% bind_cols(dist2water = w2) %>% 
             left_join(w1, by='cellid') %>% 
             mutate(permanent_water_area_ha = case_when(is.na(permanent_water_area_ha) ~ 0, 
                                                        TRUE ~ permanent_water_area_ha)) %>% st_sf()
           
           save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
           st_write(k_grid, paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.shp"), driver='ESRI Shapefile')
           rm(water)
           rm(w1)
           rm(w2)
  
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

  #shorten the names of each model
  climnames <- names(climstack) %>% as_tibble %>%  
    mutate(Model = case_when(str_detect(value, "Maxent") ~"Mx",
                             TRUE ~"Nm"),
           Scenario = case_when(str_detect(value, "average") ~ "av", 
                                str_detect(value, "extremesA") ~ "eA", 
                                str_detect(value, "extremesB") ~ "eB", 
                                str_detect(value, "low") ~ "lo", 
                                str_detect(value, "med") ~ "me",
                                TRUE ~ "hi"),
           Time = case_when(str_detect(value, "Acc70") ~"AC70",
                            str_detect(value, "ACCESS") ~"AC70",
                            str_detect(value, "HadGEM2") ~"HG70",
                            str_detect(value, "Had70") ~"HG70",
                            TRUE ~ "Curr"),
           Threshold = case_when(str_detect(value, "1$") ~ "50", 
                                 str_detect(value, "2$") ~ "80",
                                 str_detect(value, "3$") ~ "90",
                                 TRUE ~ "95")) %>%
    mutate(new_name = paste(Model, Scenario, Time, Threshold, sep="_"))
  
  names(climstack) <- climnames$new_name
  
  clim_data <- raster::extract(climstack, k_grid, weights=TRUE, fun=mean, na.rm=TRUE)
  clim_data <- data.frame(clim_data)
  
  k_grid <- k_grid %>% bind_cols(clim_data) %>% st_sf()
  
  save(k_grid, file = paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))
  st_write(k_grid, paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.shp"), driver='ESRI Shapefile')
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








