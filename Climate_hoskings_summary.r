library(raster)
library(sf)
library(tidyverse)

setwd("D:/Box Sync/DAWE/Climate_Hoskings/")

#dirs for habitat shps
habitatdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Habitat_harmonised/Datasets/Harmonised_koala_habitat_v1.gpkg"
centroiddir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/NIKA/NIKA_inputs/template/koala_templatehexgrid_100ha_centroids.shp"

#IBRA bioregions to match hoskings 2012
ibra_hosk <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/koala_IBRA7/IBRA7_regions_states_koala_hoskings.shp") %>% 
  st_transform(3577) %>% select("OBJECTID", "STA_CODE", "REG_Hoskin")
#range of koala in qld and nsw
krange <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/koala_IBRA7/IBRA7_regions_states_koala_dissolve.shp") %>% 
  st_transform(3577)
#State borders
ibra_states <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/IBRA7_regions_states.shp") %>% 
  st_transform(3577) %>% select("OBJECTID", "STA_CODE", "REG_NAME_7")


#Climate projections
#from Adams-Hosking C, Grantham HS, Rhodes JR, McAlpine C, Moss PT. 2011. Modelling climate-change-induced shifts in the distribution of the koala. Wildlife Research 38:122-130. CSIRO PUBLISHING.

#THRESHOLDS
#equal test sensitivity & specificity (training omission =0.205, test omission=0.224)
ess <- 0.407
#10 percentile training presence (training omission rate =0.1, test omission = 0.12)
cum10 <- 0.354
#Maximum test sensitivity plus specificity (test and training omission = 0.044)
maxss <- 0.291
thresholds <- c(0.407, 0.354, 0.291)

##############
#load rasters

k_curr <- raster(list.files("data/asciis/", full.names = TRUE)[1])
k_2030 <- raster(list.files("data/asciis/", full.names = TRUE)[2])
k_2050 <- raster(list.files("data/asciis/", full.names = TRUE)[3])
k_2070 <- raster(list.files("data/asciis/", full.names = TRUE)[4])
# writeRaster(k_2070, "Hoskings_2070_koalarange.tif", datatype='INT4S')
# writeRaster(k_2050, "Hoskings_2050_koalarange.tif", datatype='INT4S')
# writeRaster(k_2030, "Hoskings_2030_koalarange.tif", datatype='INT4S')
# writeRaster(k_curr, "Hoskings_current_koalarange.tif", datatype='INT4S')
# rpol <- rasterToPolygons(k_2070, function(x){x>=0.407}, na.rm=TRUE, dissolve = TRUE)

###############
#CALCULATE AREA OF LAND THAT WILL BE CLIMATE SUITABLE
#function to extract the climate values from each raster and summarise the area above that threshold in each polygon
#returns a table with 3 rows for each polygon, one for each of the three thresholds
thresholdfun <- function(r, shp, thresholds, scenario){
  shpdata <- shp %>% st_set_geometry(NULL)

    ext <- raster::extract(r, shp, fun=NULL, weights=TRUE, normalizeWeights=FALSE)
    
  df <- c()
  for(i in thresholds){
    area_ha <- unlist(lapply(ext, function(x) {
      if(is.null(x)){
        return(NA)  
      } else {
        b <- data.frame(x)
        b$area <- b$weight*1000
        b <- b[b$value>=i,]
        y <- sum(b$weight*1000, na.rm=TRUE) 
        return(y)
      }
    }))
    
   df <- rbind(df, data.frame(shpdata, scenario=rep(scenario, length(area_ha)), threshold=rep(i, length(area_ha)), area_ha))
  }
  return(df)
}
  
#function to extract raster values and summarise the climate suitable area
sumfun <- function(shp, oupname) {

 d1 <- thresholdfun(k_curr, shp, scenario="current", thresholds=thresholds) 
 d2 <- thresholdfun(k_2030, shp, scenario="yr2030", thresholds=thresholds) 
 d3 <- thresholdfun(k_2050, shp, scenario="yr2050", thresholds=thresholds) 
 d4 <- thresholdfun(k_2070, shp, scenario="yr2070", thresholds=thresholds) 
  
all_df <- rbind(d1,d2,d3,d4)
all_wide <- all_df %>% pivot_wider(names_from = scenario, names_prefix = "area_ha_", values_from = area_ha) %>%
                       mutate(perc_loss_2030 = round(100 - 100* area_ha_yr2030/area_ha_current, 2),
                               perc_loss_2050 = round(100 - 100* area_ha_yr2050/area_ha_current, 2),
                               perc_loss_2070 = round(100 - 100* area_ha_yr2070/area_ha_current, 2))
  
    if(grepl("bioregion",oupname)){
     states_df <- all_df %>%  select(STA_CODE, scenario, threshold, area_ha) %>%
                  group_by(threshold, STA_CODE, scenario) %>%
                  summarise(area_ha = sum(area_ha)) %>% ungroup() %>%
                  pivot_wider(names_from = scenario, names_prefix = "area_ha_", values_from = area_ha) %>%
                  mutate(perc_loss_2030 = round(100 - 100* area_ha_yr2030/area_ha_current, 2),
                         perc_loss_2050 = round(100 - 100* area_ha_yr2050/area_ha_current, 2),
                         perc_loss_2070 = round(100 - 100* area_ha_yr2070/area_ha_current, 2))
                        
       all_wide <- add_row(all_wide, states_df) 
       all_wide <- all_wide %>% filter(STA_CODE %in% c("ACT", "QLD", "NSW", "SA", "VIC", "JBT"))
       }
write_csv(all_wide, paste0("Climate_hoskings_", oupname, ".csv"))
#return(all_wide)  

}

sumfun(ibra_hosk, "bioregions_hoskings")
sumfun(ibra_states, "bioregions_ibra7")
sumfun(krange, "listed_koala")

###############
#CALCULATE AREA OF KOALA HABITAT THAT WILL BE CLIMATE SUITABLE

#koala habitat map
habitat <- st_read(habitatdir) %>% 
  st_set_geometry(NULL)
centroids <- st_read(centroiddir) %>%
  st_transform(3577)
habitat_centroids <- centroids %>% right_join(habitat, by='cellid') %>% select(habitat_ha_likely, habitat_ha_possible)
rm(habitat)

#make a raster template
e <- extent(centroids)
r_template <- raster(e, res=1000, crs=crs(centroids))

#extract the climate data each habitat centroid falls within
  clim_curr <- raster::extract(k_curr, habitat_centroids, fun=NULL)
  clim_2030 <- raster::extract(k_2030, habitat_centroids, fun=NULL)
  clim_2050 <- raster::extract(k_2050, habitat_centroids, fun=NULL)
  clim_2070 <- raster::extract(k_2070, habitat_centroids, fun=NULL)
  habitat_clim <- bind_cols(habitat_centroids, clim_curr=clim_curr, clim_2030=clim_2030, clim_2050=clim_2050, clim_2070=clim_2070)

 #rasterize the boundary shapefiles
  hosk_rast <- rasterize(ibra_hosk, r_template, field='OBJECTID')
  ibra_rast <- rasterize(ibra_states, r_template, field='OBJECTID')
  koala_rast <- rasterize(krange, r_template, field='Id')
  
  #extract the boundary ID each habitat centroid falls within
  ext_hosk <- raster::extract(hosk_rast, habitat_centroids, fun=NULL)
  ext_ibra <- raster::extract(ibra_rast, habitat_centroids, fun=NULL)
  ext_koala <- raster::extract(koala_rast, habitat_centroids, fun=NULL)
  
  #add boundary data to the habitat and climate data
  habitat_clim <- bind_cols(habitat_clim, hoskings_bioregions=ext_hosk, ibra7_bioregions=ext_ibra, koalarange=ext_koala)
  
  save(habitat_clim, file="habitat_clim.Rdata")
  load("habitat_clim.Rdata")
  
  habitat_clim <- habitat_clim %>% st_set_geometry(NULL)
  
 
habitatfun <- function(subsname, oupname){
  
  hab_long <- habitat_clim %>% pivot_longer(cols=c(clim_curr, clim_2030, clim_2050, clim_2070), names_to = "scenario", values_to = "value")
  oup <- c()
  
  for (i in thresholds) {
    df <- hab_long %>% filter(value>i)
    
    if(subsname=="ibra"){
      ibra_lookup <- ibra_states %>% st_set_geometry(NULL) %>% filter(STA_CODE %in% c("ACT", "QLD", "NSW"))
      df <- df %>%
            group_by(ibra7_bioregions, scenario) %>%
            summarise(habitat_ha = round(sum(habitat_ha_likely, na.rm=TRUE), 0)) %>% 
            ungroup() %>%
            mutate(threshold=i)
      df <- df %>% inner_join(ibra_lookup, by=c('ibra7_bioregions'='OBJECTID')) %>% select(-ibra7_bioregions)
      df_wide <- df %>% pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha, values_fill=0) %>%
        mutate(perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
    
      states_df <- df %>% group_by(threshold, STA_CODE, scenario) %>%
        summarise(habitat_ha = sum(habitat_ha)) %>% ungroup() %>%
        pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha) %>%
        mutate(perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
      
      all_wide <- add_row(df_wide, states_df) 
    
    } else if(subsname=="hosk"){
    
      ibra_lookup <- ibra_hosk %>% st_set_geometry(NULL) %>% filter(STA_CODE %in% c("ACT", "QLD", "NSW"))
      df <- df %>%
        group_by(hoskings_bioregions, scenario) %>%
        summarise(habitat_ha = round(sum(habitat_ha_likely, na.rm=TRUE), 0)) %>% ungroup() %>%
        mutate(threshold=i)
      df <- df %>% inner_join(ibra_lookup, by=c('hoskings_bioregions'='OBJECTID')) %>% select(-hoskings_bioregions)
      df_wide <- df %>% pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha, values_fill=0) %>%
        mutate(perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
      
      states_df <- df %>% group_by(threshold, STA_CODE, scenario) %>%
        summarise(habitat_ha = sum(habitat_ha)) %>% ungroup() %>%
        pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha) %>%
        mutate(perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
      
      all_wide <- add_row(df_wide, states_df) 
    
    } else if(subsname=="krange"){  
      df <- df %>%
        group_by(scenario) %>%
        summarise(habitat_ha = round(sum(habitat_ha_likely, na.rm=TRUE), 0)) %>% ungroup() %>%
        mutate(threshold=i)
      all_wide <- df %>% pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha, values_fill=0) %>%
        mutate(perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2))
  
    }
    oup <- rbind(oup, all_wide)
  }
    write_csv(oup, paste0("Climate_hoskings_habitat_", oupname, ".csv"))
    #return(oup) 
}

  habitatfun("hosk", "bioregions_hoskings")
  habitatfun("ibra", "bioregions_ibra7")
  habitatfun("krange", "listed_koala")
  
  a <- read_csv("Climate_hoskings_habitat_bioregions_hoskings.csv")
  b <- a %>% group_by(STA_CODE, REG_Hoskin) %>% summarise(perc_loss_2030 = paste0(min(perc_loss_2030), ", ", max(perc_loss_2030)),
                                                          perc_loss_2050 = paste0(min(perc_loss_2050), ", ", max(perc_loss_2050)),
                                                          perc_loss_2070 = paste0(min(perc_loss_2070), ", ", max(perc_loss_2070)))

  write_csv(b, paste0("Climate_hoskings_habitat_bioregions_hoskings_v2.csv"))

  
  a <- read_csv("Climate_hoskings_habitat_bioregions_ibra7.csv")
  b <- a %>% group_by(STA_CODE, REG_NAME_7) %>% summarise(perc_loss_2030 = paste0(min(perc_loss_2030), ", ", max(perc_loss_2030)),
                                                          perc_loss_2050 = paste0(min(perc_loss_2050), ", ", max(perc_loss_2050)),
                                                          perc_loss_2070 = paste0(min(perc_loss_2070), ", ", max(perc_loss_2070)))
  
  write_csv(b, paste0("Climate_hoskings_habitat_bioregions_ibra7_v2.csv"))  
    