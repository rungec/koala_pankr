#this script extracts landscape values for clustering
#tested using R version 4.0.2


#################
library(sf)
library(tidyverse)
library(raster)
library(slga) #for download of Soil and Landscape Grid of Australia https://www.clw.csiro.au/aclep/soilandlandscapegrid/GetData-R_package.html
library(lwgeom)
library(snow)


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
source(paste0(dirname(getwd()), "/R_scripts/koala_pankr/st_parallel.r"))

cell_diameter = 1000 #metres
ncore = 8 #EDIT ME #put 0 if don't want to run in parallel

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
    st_buffer(1000) %>% st_geometry()
  historic_koala  <- st_read(paste0(datadir, "Occurrence records"), "Koala_Qld_NSW_merge_1970to2000_1kmres_noDup") %>% 
    st_transform(3577) %>% #GDA94_Albers
    st_buffer(1000) %>% st_geometry()
  
  k_grid <- k_grid %>% bind_cols(current_koala = lengths(st_intersects(k_grid, current_koala, sparse=TRUE)), 
                                 historic_koala = lengths(st_intersects(k_grid, historic_koala, sparse=TRUE))) 

  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(current_koala)
  rm(historic_koala)
  
#load and extract plant available water content

  pawc <- raster(pawcdir) #250m res, mm/m for top 1m
  #pawc <- get_soils_data(product='NAT', attribute='AWC', component='VAL', depth=1, aoi=k_grid) #90m res and need to download each depth and sum
  k_grid <- k_grid %>% st_transform(st_crs(pawc)) 
  pawc_mean <- rast_parallel(pawc, k_grid, fun=mean, na.rm=TRUE, n_core = ncore)
  pawc_max <- rast_parallel(pawc, k_grid, fun=max, na.rm=TRUE, n_core = ncore)
  k_grid<- k_grid %>% bind_cols(pawc_mean = pawc_mean, pawc_max = pawc_max)

  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(pawc)
  rm(pawc_max)
  rm(pawc_mean)
  
#load and extract soil depth
  k_grid <- k_grid %>% st_transform(4283) #GDA94
  soildepth <- get_soils_data(product='NAT', attribute='DES', component='VAL', depth=1, aoi=k_grid) 
  
  soildepth_mean <- rast_parallel(soildepth, k_grid, fun=mean, na.rm=TRUE, n_core = ncore)
  soildepth_max <- rast_parallel(soildepth, k_grid, fun=max, na.rm=TRUE, n_core = ncore)
  k_grid<- k_grid %>% bind_cols(soildepth_mean = soildepth_mean, soildepth_max = soildepth_max)
  
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_diameter,"_m.Rdata"))
  rm(soildepth)
  rm(soildepth_mean)
  rm(soildepth_max)
  
#load and extract bushfire freq
  firerast <- raster(firedir)
  k_grid <- k_grid %>% st_transform(st_crs(firerast)) #WGS84
  firefreq_88to15 <- rast_parallel(firerast, k_grid, fun=max, na.rm=TRUE, n_core = ncore)
  k_grid<- k_grid %>% bind_cols(firefreq_88to15 = firefreq_88to15)
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
    st_transform(3577) %>% st_geometry()
  k_grid <- k_grid %>% st_transform(st_crs(water))
  
  #area of permanent water in cell
  #because water is dissolved into a single, multipart polygon we get one row for every cell that overlaps water
  
  w1 <- st_intersection(k_grid, water) %>% 
  #w1 <- st_parallel(k_grid, st_intersection, ncores, y = water) %>% 
    mutate(permanent_water_area_ha= as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
             dplyr::select(cellid, permanent_water_area_ha) 
           
 #distance to water
  w2 <- st_distance(k_grid, water)
  #w2 <- st_parallel(k_grid, st_distance, ncores, y = water)

 #join back to dataset
 k_grid<- k_grid %>% bind_cols(dist2water = w2) %>% 
   left_join(w1, by='cellid') %>% 
   mutate(permanent_water_area_ha = case_when(is.na(permanent_water_area_ha) ~ 0, 
                                              TRUE ~ permanent_water_area_ha))
 
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
  
  clim_data <- rast_parallel(climstack, k_grid, weights=TRUE, fun=mean, na.rm=TRUE, n_core = ncore)
  clim_data <- data.frame(clim_data)
  
  k_grid <- k_grid %>% bind_cols(clim_data)
  
  save(k_grid, file = paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))
  st_write(k_grid, paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.shp"), driver='ESRI Shapefile')
  rm(climnames)
  rm(climstack)
  rm(clim_data)
  
} else {
  load(paste0(oupdir, "koala_gridded_clim_",cell_diameter,"_m.Rdata"))
}
head(k_grid)

######################
#Extract habitat data
######################
#regions <- st_read(paste0(bbdir, "IBRA7_koala_management_regions.shp"))
lookup <- read.csv(paste0("habitatfilelist.csv"))
k_grid <- k_grid %>% st_transform(3577) #GDA94 albers 

#for each region, we calculate the area of koala habitat in each grid cell, and the quality of the koala habtiat in each grid cell.
#quality is not comparable across regions
#we then add those two columns for each region to k_grid.

for(i in 1:nrow(lookup)){
  curregion <- lookup$Shortname[i]
  
  if(lookup$state[i]=='NSW'){

    curr_rast <- raster(list.files(paste0(datadir, lookup$Filename[i]), pattern=".tif$", recursive=TRUE, full.names=TRUE))
    k_grid <- k_grid %>% st_transform(st_crs(curr_rast))
    #area of polygon that is not classed as no data
    habitat_area_ha <- rast_parallel(curr_rast, k_grid, fun=function(x, ...)length(na.omit(x))*res(curr_rast)[1]*res(curr_rast)[2]/10000, n_core = ncore) 

    #mean suitability of cells 
    habitat_rank_mean <- rast_parallel(curr_rast, k_grid, fun=mean, na.rm=TRUE, n_core = ncore) 
    
    habitat <- data.frame(habitat_area_ha, habitat_rank_mean)
    habitat <- setNames(habitat, paste(names(habitat), curregion, sep="_"))
   
    #here we don't need  a left_join because raster extract keeps a row for each cellid in k_grid
    k_grid <- k_grid %>% bind_cols(habitat)
    save(k_grid, file = paste0(oupdir, "koala_gridded_vars_",cell_diameter,"_m.Rdata"))
    
    rm(curr_rast)
    rm(habitat)
    rm(habitat_rank_mean)
    rm(habitat_area_ha)
        
  } else if (lookup$state[i]=='SEQ'){
    
    #we use the HSM categories 4:10 (low-high quality habitat, see documentation)
    curr_shp <- st_read(paste0(datadir, lookup$Filename[i])) %>% 
                  st_transform(3577) %>% 
                  group_by(HSM_CATEGO) %>% 
                  summarise(area = sum(AREA_HA))

    habitat <- st_intersection(k_grid, curr_shp) %>% 
    #habitat <- st_parallel(k_grid, st_intersection, ncores, y = curr_shp) %>% 
      mutate(area_ha = as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
      group_by(cellid) %>% 
      summarise(habitat_area_ha = sum(area_ha),
                habitat_rank_mean = sum(HSM_CATEGO*area_ha)/sum(area_ha)) %>% 
      dplyr::select(cellid, habitat_area_ha, habitat_rank_mean )
    
    habitat <- setNames(habitat, c("cellid", paste(names(habitat)[2:3], curregion, sep="_")))
    
    k_grid <- k_grid %>% left_join(habitat, by='cellid') %>% st_sf()
    save(k_grid, file = paste0(oupdir, "koala_gridded_vars_",cell_diameter,"_m.Rdata"))
    
    rm(curr_rast)
    rm(habitat)

  } else if (lookup$state[i]=='QLD'){
    
    
    
    
    
  }

save(k_grid, file = paste0(oupdir, "koala_gridded_habitat", curregion, cell_diameter,"_m.Rdata"))    
}

###END








