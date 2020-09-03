#this script extracts landscape values for clustering
#tested using R version 4.0.2


#################
library(sf)
library(tidyverse)
library(raster)
library(slga) #for download of Soil and Landscape Grid of Australia https://www.clw.csiro.au/aclep/soilandlandscapegrid/GetData-R_package.html
library(lwgeom)
library(snow)
#library(stars)


#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"
bbdir <- paste0(datadir, "IBRA7_regions_states_koala_dissolve.shp")
currkoaladir <- paste0(datadir, "Koala_Qld_NSW_merge_2000on_1kmres_noDup.shp")
histkoaladir <- paste0(datadir, "Koala_Qld_NSW_merge_1970to2000_1kmres_noDup.shp")
waterdir <- paste0(datadir, "SurfaceHydro_Perennial_dissolve_koala.shp")
firedir <- paste0(datadir, "National_fire_freq/ff_88to152.tif")
pawcdir <- paste0(datadir, "PAWC_1m/PAWC_1m/pawc_1m")
climdir <- paste0(datadir, "Climrasters_thresholded/Climate_refugia/")
lulcdir <- paste0(datadir, "clum_50m1218m.tif")
lulclookupdir <- paste0(datadir, "CLUM_recovery_categorisation.csv")
snesdir <- paste0(datadir, "Phascolarctos_cinereus_85104Clipped.shp")


#source(paste0(dirname(getwd()), "/R_scripts/koala_pankr/extractfun.r"))
#source(paste0(getwd(), "/R_scripts/koala_pankr/makegridfun.r"))
source(paste0(getwd(), "/R_scripts/koala_pankr/st_parallel.r"))
source(paste0(getwd(), "/R_scripts/koala_pankr/fastbindfun.r"))

cell_area = "100ha" 
ncore = 1 #EDIT number of cores to use for parallel #put 1 if don't want to run in parallel
n_splits = 1000 #split the data into smaller chunks for faster processing

######################
#load study region and project to GDA_94 Aust Albers EPSG 3577 which has units in m
######################
#bb <- st_read(paste0(bbdir, "IBRA7_regions_states_koala_dissolve.shp"))
#bb <- st_transform(bb, 3577)

#make hexagons #make grid is really slow so did this is ARCGIS using generatetesselation tool
# if(file.exists(paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))==FALSE){
#   k_grid <- makegridfun(bb, currsize=1000) 

#   save(k_grid, file = paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))
#   st_write(k_grid, paste0(oupdir, "koala_templatehexgrid_",cell_area,".shp"), driver='ESRI Shapefile')
# } else {
#   load(paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))
# }

#add a columnn 'split' to template grid for splitting into smaller chunks
load(paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))
k_grid <- k_grid %>% mutate(splits =  rep(1:n_splits, each = nrow(k_grid) / n_splits, length.out = nrow(k_grid)))
#save(k_grid, file = paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))
  # st_write(k_grid, paste0(oupdir, "koala_templatehexgrid_",cell_area,".shp"), driver='ESRI Shapefile')

######################
##Extract attributes for datasets that span the whole study region
######################
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"1.Rdata"))==FALSE){
  print(paste0("Starting koala occurrences ", Sys.time()))
  
#load and extract koala occurrence #this runs fast
  current_koala <- st_read(currkoaladir) %>% 
    st_transform(3577) %>%
    st_buffer(1000) %>% st_geometry()
  historic_koala  <- st_read(histkoaladir) %>% 
    st_transform(3577) %>% #GDA94_Albers
    st_buffer(1000) %>% st_geometry()
  
  k_grid <- k_grid %>% bind_cols(current_koala = lengths(st_intersects(k_grid, current_koala, sparse=TRUE)), 
                                 historic_koala = lengths(st_intersects(k_grid, historic_koala, sparse=TRUE))) 

  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"1.Rdata"))
  rm(current_koala)
  rm(historic_koala)
} else {
  load(paste0(oupdir, "koala_gridded_data_",cell_area,"1.Rdata"))
}

#######################

if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"2.Rdata"))==FALSE){
#load and extract plant available water content 
  print(paste0("Starting PAWC ", Sys.time()))
  pawc <- raster(pawcdir) #250m res, mm/m for top 1m
  #pawc <- get_soils_data(product='NAT', attribute='AWC', component='VAL', depth=1, aoi=k_grid) #90m res and need to download each depth and sum
  k_grid <- k_grid %>% st_transform(st_crs(pawc)) 
  
   #split and extract data
  print(paste0("starting splits ", Sys.time()))
  for (i  in 1:n_splits){
    print(paste0("starting split ", i, " ", Sys.time()))
    kc <- k_grid %>% filter(splits == i) %>% dplyr::select(cellid)
    pawc_kc <- raster::crop(pawc, kc, snap='out')
    
    print(Sys.time())
    pawc_mean <- raster::extract(pawc_kc, kc, fun=mean, na.rm=TRUE)
    pawc_max <- raster::extract(pawc_kc, kc, fun=max, na.rm=TRUE)
    print(Sys.time())
    kc <- kc %>% bind_cols(pawc_mean = pawc_mean, pawc_max=pawc_max) %>% st_set_geometry(NULL)
    write_csv(kc, path = paste0(oupdir, "temp/koala_gridded_data_",cell_area, "_pawcsplit_", i, ".csv"))
    rm(kc)
    rm(pawc_kc)
    rm(pawc_mean)
    rm(pawc_max)
    
  }
  rm(pawc)

  k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="pawc", grid=k_grid)
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"2.Rdata"))
  

} else {
  load(paste0(oupdir, "koala_gridded_data_",cell_area,"2.Rdata"))
}

#######################
  
#load and extract soil depth
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"3.Rdata"))==FALSE){
  print(paste0("Starting soil depth ", Sys.time()))
  k_grid <- k_grid %>% st_transform(4283) #GDA94
  
  #split the template grid into smaller chunks and extract data
  print(paste0("starting splits ", Sys.time()))
  for (i  in 1:n_splits){
    print(paste0("starting split ", i, " ", Sys.time()))
    kc <- k_grid %>% filter(splits == i) %>% dplyr::select(cellid)
    print(Sys.time())
    #download data
    soildepth <- get_soils_data(product='NAT', attribute='DES', component='VAL', depth=1, aoi=kc) 
    #calculate mean
    soildepth_mean <- raster::extract(soildepth, kc, fun=mean, na.rm=TRUE)
    #soildepth_max <- raster::extract(soildepth, kc, fun=max, na.rm=TRUE)
    print(Sys.time())
    kc <- kc %>% bind_cols(soildepth_mean = soildepth_mean) %>% st_set_geometry(NULL)
    write_csv(kc, path = paste0(oupdir, "temp/koala_gridded_data_",cell_area, "_soilsplit_", i, ".csv"))
    rm(kc)
    rm(soildepth)
    rm(soildepth_mean)
  }
  
  k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="soil", grid=k_grid) 
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"3.Rdata"))

  } else {
    load(paste0(oupdir, "koala_gridded_data_",cell_area,"3.Rdata"))
  }

  
#load and extract bushfire freq
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"4.Rdata"))==FALSE){
  print(paste0("Starting bushfire freq ", Sys.time()))
  firerast <- raster(firedir)
  k_grid <- k_grid %>% st_transform(st_crs(firerast)) #WGS84

  print(paste0("starting splits ", Sys.time()))
  for (i  in 1:n_splits){
    print(paste0("starting split ", i, " ", Sys.time()))
    kc <- k_grid %>% filter(splits == i) %>% dplyr::select(cellid)
    fire_kc <- raster::crop(firerast, kc, snap='out')
    
    print(Sys.time())
    firefreq_88to15 <- raster::extract(fire_kc, kc, fun=max, small=TRUE, na.rm=TRUE)
    print(Sys.time())
    kc <- kc %>% bind_cols(firefreq_88to15 = firefreq_88to15) %>% st_set_geometry(NULL)
    write_csv(kc, path = paste0(oupdir, "temp/koala_gridded_data_",cell_area, "_firesplit_", i, ".csv"))
    rm(kc)
    rm(fire_kc)
    rm(firefreq_88to15)
    
  }
  rm(firerast)
 
  k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="fire", grid=k_grid)
  save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"4.Rdata"))

  } else {
    load(paste0(oupdir, "koala_gridded_data_",cell_area,"4.Rdata"))
  }

  
#load and extract permanent water
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"5.Rdata"))==FALSE){
 #water <- st_read(paste0(waterdir, "SurfaceHydrologyPolygonsNational.gdb")) %>% 
 #             filter(PERENNIALITY=='Perennial') %>% st_transform(3577)
##fix geometry 
 # water <- st_read(paste0(waterdir, "SurfaceHydro_Perennial_dissolve_koala.shp")) %>% 
 #   st_transform(3577)
  #water1 <- st_buffer(water, dist=0)
  #st_write(water1, paste0(waterdir, "SurfaceHydro_Perennial_dissolve_koala_fixgeom.shp"), driver='ESRI Shapefile') 
  print(paste0("Starting perennial water ", Sys.time())) 
  water <- st_read(waterdir) %>% 
    st_transform(3577) %>% st_geometry()
  k_grid <- k_grid %>% st_transform(st_crs(water))
  
  #area of permanent water in cell
  #because water is dissolved into a single, multipart polygon we get one row for every cell that overlaps water
  
  #w1 <- st_intersection(k_grid, water) %>% 
  w1 <- st_parallel(k_grid, st_intersection, n_cores = ncore, y = water) %>% 
    mutate(permanent_water_area_ha= as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
             dplyr::select(cellid, permanent_water_area_ha) 
           
 #distance to water
  print(paste0("Starting distance to water ", Sys.time()))
  #w2 <- st_distance(k_grid, water)
  #w2 <- st_parallel(k_grid, st_distance, n_cores = ncore, y = water)

 #join back to dataset
 k_grid<- k_grid %>% #bind_cols(dist2water = w2) %>% 
   left_join(w1, by='cellid') %>% 
   mutate(permanent_water_area_ha = case_when(is.na(permanent_water_area_ha) ~ 0, 
                                              TRUE ~ permanent_water_area_ha))
 
 save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"5.Rdata"))
 
 rm(water)
 rm(w1)
 #rm(w2)
  
  } else {
  load(paste0(oupdir, "koala_gridded_data_",cell_area,"5.Rdata"))
  }

#load and extract data on land use
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"6.Rdata"))==FALSE){
 
   lulc <- raster(lulcdir)
  lulclookup <- read_csv(lulclookupdir)
  rcl <- lulclookup %>% dplyr::select(VALUE, Recovery_code)
  k_grid <- k_grid %>% st_transform(crs(lulc))
  lulc <- crop(lulc, k_grid)
  lulc <- raster::subs(lulc, y=rcl, filename=paste0(datadir, "CLUM_reclassify_recovery_potential.tif"), datatype='INT2S')
  
  for(i in 1:n_splits){
    print(paste0("starting split ", i, " ", Sys.time()))
    kc <- k_grid %>% filter(splits == i) %>% dplyr::select(cellid)
    lulc_kc <- raster::crop(lulc, kc, snap='out')
    
    print(Sys.time())
    lulc_matrix <- raster::extract(lulc_kc, kc)
    recoverable_area_ha <- unlist(lapply(lulc_matrix, function(x) if (!is.null(x)) length(which(x==1)) else NA )) *50*50/10000
    unrecoverable_area_ha <- unlist(lapply(lulc_matrix, function(x) if (!is.null(x)) length(which(x==0)) else NA )) *50*50/10000
    intact_area_ha <- unlist(lapply(lulc_matrix, function(x) if (!is.null(x)) length(which(x==2)) else NA )) *50*50/10000
    
    print(Sys.time())
    kc <- kc %>% bind_cols(recoverable_area_ha = recoverable_area_ha, 
                           unrecoverable_area_ha = unrecoverable_area_ha, 
                           intact_area_ha = intact_area_ha) %>% st_set_geometry(NULL)
    write_csv(kc, path = paste0(oupdir, "temp/koala_gridded_data_",cell_area, "_lulcplit_", i, ".csv"))
  }


#join back to dataset
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="lulc", grid=k_grid)
save(k_grid, file=paste0(oupdir, "koala_gridded_data_",cell_area,"6.Rdata")) 
  
} else {
  load(paste0(oupdir, "koala_gridded_data_",cell_area,"6.Rdata"))
}  

#load and extract Commonwealth SNES map
if(file.exists(paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata"))==FALSE){
  
  snes <-   st_read(snesdir) 
  k_grid <- k_grid %>% st_transform(st_crs(snes))
  snes_likely_pol <- snes %>% filter(distribn %in% c("Known", "Likely"))
  print(paste("Starting snes likely ", Sys.time()))
  snes_likely <- st_parallel(k_grid, st_intersection, n_cores = ncore, y = snes_likely_pol) %>% 
    mutate(snes_likelyhabitat_ha = as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
    dplyr::select(snes_likelyhabitat_ha)
  save(snes_likely, paste0(oupdir, "temp/snes_likely.Rdata"))
  
  print(paste("Starting snes may ", Sys.time()))
  snes_may_pol <- snes %>% filter(distribn =="May")
  snes_may <- st_parallel(k_grid, st_intersection, n_cores = ncore, y = snes_may_pol) %>% 
    mutate(snes_maybehabitat_ha = as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
    dplyr::select(snes_maybehabitat_ha)
  save(snes_may, paste0(oupdir, "temp/snes_may.Rdata"))
  
  k_grid <- k_grid %>% bind_cols(snes_likelyhabitat_ha = snes_likely, snes_maybehabitat_ha = snes_maybe)
  save(k_grid, file= paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata"))
  
} else {
  load(paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata"))
}

  
head(k_grid)
save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,".Rdata"))
st_write(k_grid, paste0(oupdir, "koala_gridded_data_", cell_area, ".shp"))

######################
#Extract climate data
######################
if(file.exists(paste0(oupdir, "koala_gridded_clim_",cell_area,".Rdata"))==FALSE){
  print(paste0("Starting climate ", Sys.time()))
  rastlist <- paste(rep(c("2070", "Current"), each=3), c("perc99ofrecords", "perc95ofrecords", "perc90ofrecords"), sep="_")
  
for(d in rastlist) {
  currrast <- raster(list.files(climdir, pattern=d))
  k_grid <- k_grid %>% st_transform(crs(currrast))
  
  for (i  in 1:n_splits){
    print(paste0("starting split ", i, " ", Sys.time()))
    kc <- k_grid %>% filter(splits == i) %>% dplyr::select(cellid) #drop the other columns
    clim_kc <- raster::crop(currrast, kc, snap='out')
    
    print(Sys.time())
    clim_data <- raster::extract(clim_kc, kc, small=TRUE, fun=max, na.rm=TRUE)
    clim_data <- data.frame(clim_data)
    setNames(clim_data, d)
    kc <- kc %>% bind_cols(clim_data) %>% st_set_geometry(NULL)
    write_csv(kc, path = paste0(oupdir, "temp/koala_gridded_data_",cell_area, "_climsplit_", d, "_", i, ".csv"))
    rm(kc)
    rm(clim_kc)
    rm(climdata)
    
  }}
  rm(climnames)
  rm(climstack)
 
  k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="clim", grid=k_grid)
  save(k_grid, file = paste0(oupdir, "koala_gridded_clim_",cell_area,".Rdata"))
  
} else {
  load(paste0(oupdir, "koala_gridded_clim_",cell_area,".Rdata"))
}
head(k_grid)


######################
#Extract habitat data
######################
#regions <- st_read(paste0(bbdir, "IBRA7_koala_management_regions.shp"))
print(paste0("Starting habitat ", Sys.time()))
lookup <- read.csv(paste0("habitatfilelist.csv"))
k_grid <- k_grid %>% dplyr::select(cellid) #GDA94 albers and drop other columns 

#for each region, we calculate the area of koala habitat in each grid cell, and the quality of the koala habtiat in each grid cell.
#quality is not comparable across regions
#we then add those two columns for each region to k_grid.

for(i in 1:nrow(lookup)){
  curregion <- lookup$Shortname[i]
  print(paste0("Starting habitat ", curregion, " ", Sys.time()))
  
  if(lookup$state[i]=='NSW'){

    #make this faster, add a loop
    #first clip to the raster
    #or clip the shp to the KMR
    #reclassify the raster as habitat/non habitat OR make non habitat NAs
    curr_rast <- raster(list.files(paste0(datadir, lookup$Filename[i]), pattern=".tif$", recursive=TRUE, full.names=TRUE))
    k_grid <- k_grid %>% st_transform(st_crs(curr_rast))
    #area of polygon that is not classed as no data
    habitat_area_ha <- raster::extract(curr_rast, k_grid, fun=function(x, ...)length(na.omit(x))*res(curr_rast)[1]*res(curr_rast)[2]/10000) 

    #mean suitability of cells 
    habitat_rank_mean <- raster::extract(curr_rast, k_grid, fun=mean, na.rm=TRUE) 
    
    habitat <- data.frame(habitat_area_ha, habitat_rank_mean)
    habitat <- setNames(habitat, paste(names(habitat), curregion, sep="_"))
   
    #here we don't need  a left_join because raster extract keeps a row for each cellid in k_grid
    k_grid <- k_grid %>% bind_cols(habitat)
    save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".Rdata"))
    
    rm(curr_rast)
    rm(habitat)
    rm(habitat_rank_mean)
    rm(habitat_area_ha)
        
  } else if (lookup$state[i]=='SEQ'){
    
    k_grid <- k_grid %>% st_transform(3577)
    #we use the HSM categories 4:10 (low-high quality core habitat, see documentation)
    curr_shp <- st_read(paste0(datadir, lookup$Filename[i])) %>% 
                  st_transform(3577) %>% 
                  group_by(HSM_CATEGO) %>% 
                  summarise(area = sum(AREA_HA))

    #habitat <- st_intersection(k_grid, curr_shp) %>% 
    habitat <- st_parallel(k_grid, st_intersection, n_cores = ncore, y = curr_shp) %>% 
      mutate(area_ha = as.numeric(st_area(.)/10000)) %>% st_set_geometry(NULL) %>% 
      group_by(cellid) %>% 
      summarise(habitat_area_ha = sum(area_ha),
                habitat_rank_mean = sum(HSM_CATEGO*area_ha)/sum(area_ha)) %>% 
      dplyr::select(cellid, habitat_area_ha, habitat_rank_mean )
    
    habitat <- setNames(habitat, c("cellid", paste(names(habitat)[2:3], curregion, sep="_")))
    
    k_grid <- k_grid %>% left_join(habitat, by='cellid') %>% st_sf()
    save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".Rdata"))
    
    rm(curr_rast)
    rm(habitat)

  } else if (lookup$state[i]=='QLD'){
    
    
    
    
    save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".Rdata")) 
  }

save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, ".Rdata"))   
}

###END








