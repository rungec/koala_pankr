library(raster)
library(sf)
library(tidyverse)

setwd("D:/Box Sync/DAWE/Climate_change/")

#dirs for habitat shps
habitatdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/Habitat_harmonised/Datasets/Harmonised_koala_habitat_v1.gpkg"
centroiddir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/NIKA/NIKA_inputs/template/koala_templatehexgrid_100ha_centroids.shp"

#range of koala in qld and nsw
#KLM 26=known, 36=likely, 46=may occur
krange <- st_read("D:/Box Sync/DAWE/Land_use_change/Koala_SDM/Phascolarctos_cinereus_85104.shp") %>% 
  st_transform(3577)
#IBRA bioregions to match hoskings 2012
ibra_hosk <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/koala_IBRA7/IBRA7_regions_states_koala_hoskings_2021SDM.shp") %>% 
  st_transform(3577) #%>% dplyr::select("OBJECTID", "STA_CODE", "REG_Hoskin")
#IBRA7 bioregions
ibra_states <- st_read("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Bioregions/koala_IBRA7/IBRA7_regions_states_2021SDM.shp") %>% 
  st_transform(3577) #%>% dplyr::select("OBJECTID", "STA_CODE", "REG_NAME_7")

#Climate projections
#from Adams-Hosking C, Grantham HS, Rhodes JR, McAlpine C, Moss PT. 2011. Modelling climate-change-induced shifts in the distribution of the koala. Wildlife Research 38:122-130. CSIRO PUBLISHING.
#and Briscoe, NJ, Kearney, MR, Taylor, CA & Wintle, BA 2016, 'Unpacking the mechanisms captured by a correlative species distribution model to improve predictions of climate refugia', Global Change Biology, vol. 22, no. 7, pp. 2425-2439.


###################
#BRISCOE THRESHOLDS
#load threshold table
thresholds_briscoe <- read.csv("Climate_briscoe/Climate_thresholds.csv")

###################
#HOSKINGS THRESHOLDS
#equal test sensitivity & specificity (training omission =0.205, test omission=0.224)
#ess <- 0.407
#10 percentile training presence (training omission rate =0.1, test omission = 0.12)
#cum10 <- 0.354
#Maximum test sensitivity plus specificity (test and training omission = 0.044)
#maxss <- 0.291
thresholds_hosk <- c(0.407, 0.354, 0.291)


##############
#load HOSKINGS rasters

k_curr <- raster(list.files("Climate_hoskings/data/asciis/", full.names = TRUE)[1])
k_2030 <- raster(list.files("Climate_hoskings/data/asciis/", full.names = TRUE)[2])
k_2050 <- raster(list.files("Climate_hoskings/data/asciis/", full.names = TRUE)[3])
k_2070 <- raster(list.files("Climate_hoskings/data/asciis/", full.names = TRUE)[4])
#k_2021 <- raster("Climate_hoskings/output/Interpolated_koala_climate/Hoskings_2021_koalarange_interpolated.tif")
#k_2042 <- raster("Climate_hoskings/output/Interpolated_koala_climate/Hoskings_2042_koalarange_interpolated.tif")
# writeRaster(k_2070, "Hoskings_2070_koalarange.tif", datatype='INT4S')
# writeRaster(k_2050, "Hoskings_2050_koalarange.tif", datatype='INT4S')
# writeRaster(k_2030, "Hoskings_2030_koalarange.tif", datatype='INT4S')
# writeRaster(k_curr, "Hoskings_current_koalarange.tif", datatype='INT4S')
# rpol <- rasterToPolygons(k_2070, function(x){x>=0.407}, na.rm=TRUE, dissolve = TRUE)


###############
#CALCULATE AREA OF LAND THAT WILL BE CLIMATE SUITABLE: HOSKINGS
#function to extract the climate values from each raster and summarise the area above that threshold in each polygon
#returns a table with 3 rows for each polygon, one for each of the three thresholds
thresholdfun <- function(r, shp, thresholds, scenario, climmod, currmod){
  shpdata <- shp %>% st_set_geometry(NULL)

    ext <- raster::extract(r, shp, fun=NULL, weights=TRUE, normalizeWeights=FALSE)
   
  df <- data.frame()   
  if(climmod=="Briscoe"){
    area_ha <- unlist(lapply(ext, function(x) {
      if(is.null(x)){
        return(NA)  
      } else {
        b <- data.frame(x)
        b$area <- b$weight*1000
        b <- b[b$value>=thresholds,]
        y <- sum(b$weight*1000, na.rm=TRUE) 
        return(y)
      }
    }))  

    df <- rbind(df, data.frame(shpdata, scenario=rep(scenario, length(area_ha)), model=rep(currmod, length(area_ha)), threshold=rep(thresholds, length(area_ha)), area_ha))
    
    } else if (climmod=="Hosking") { 
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
   }
  return(df)
}
  
#function to extract raster values for hoskings data and summarise the climate suitable area
sumfun <- function(shp, oupname, thresholds) {

 d1 <- thresholdfun(k_curr, shp, scenario="current", thresholds=thresholds, climmod = "Hosking") 
 #d2 <- thresholdfun(k_2021, shp, scenario="yr2021", thresholds=thresholds, climmod = "Hosking") 
 d3 <- thresholdfun(k_2030, shp, scenario="yr2030", thresholds=thresholds, climmod = "Hosking") 
 #d4 <- thresholdfun(k_2042, shp, scenario="yr2042", thresholds=thresholds, climmod = "Hosking") 
 d5 <- thresholdfun(k_2050, shp, scenario="yr2050", thresholds=thresholds, climmod = "Hosking") 
 d6 <- thresholdfun(k_2070, shp, scenario="yr2070", thresholds=thresholds, climmod = "Hosking") 
  
  #all_df <- rbind(d1,d2,d3,d4, d5, d6)
  all_df <- rbind(d1,d3,d5, d6)
  #all_df <- rbind(d1,d2,d4, d6)
  
  all_wide <- all_df %>% pivot_wider(names_from = scenario, names_prefix = "area_ha_", values_from = area_ha)
  
  write_csv(all_wide, paste0("Climate_hoskings/output/Climate_hoskings_", oupname, ".csv"))
  return(all_wide)
}

ibradf <- sumfun(ibra_hosk, "bioregions_hoskings", thresholds=thresholds_hosk)
ibra7df <- sumfun(ibra_states, "bioregions_ibra7", thresholds=thresholds_hosk)
#sumfun(krange, "listed_koala", thresholds=thresholds_hosk)

###############
#CALCULATE AREA OF LAND THAT WILL BE CLIMATE SUITABLE: BRISCOE

#Set up function
briscoe_fun <- function(inpdir, shp, scenario, oupname, tool){
  all_df <- data.frame()
  for(currfile in list.files(inpdir, full.names=TRUE)){
    r <- raster(currfile)
    r2 <- projectRaster(r, crs=crs(shp), res=10000) #project to equal area
    
    currmod <- str_split(basename(currfile), ".tif")[[1]][1]
    print(currmod)
    
    thresholds_b <- thresholds_briscoe %>% filter(Records=="No_duplicates" & Tool==tool) 
    if(grepl("_av_|average", currmod)==TRUE) {
       currthreshold <- thresholds_b[thresholds_b$Scenario=="averages", "perc95ofrecords"]
    } else if (grepl("_ExtA_|extremesA", currmod)==TRUE){
      currthreshold <- thresholds_b[thresholds_b$Scenario=="extremesA", "perc95ofrecords"]
    } else if (grepl("_ExtB_|extremesB", currmod)==TRUE){
      currthreshold <- thresholds_b[thresholds_b$Scenario=="extremesB", "perc95ofrecords"]
    } else if (grepl("_high", currmod)==TRUE){
      currthreshold <- thresholds_b[thresholds_b$Scenario=="poor_high", "perc95ofrecords"]
    } else if (grepl("_med", currmod)==TRUE){
      currthreshold <- thresholds_b[thresholds_b$Scenario=="poor_med", "perc95ofrecords"]
    } else if (grepl("_low", currmod)==TRUE){
      currthreshold <- thresholds_b[thresholds_b$Scenario=="poor_low", "perc95ofrecords"]
    } 
    
  print(currthreshold)  
  df <- thresholdfun(r2, shp, scenario=scenario, thresholds=currthreshold, climmod="Briscoe", currmod=currmod)
  
  all_df <- rbind(all_df, df)
  }

  return(all_df)
  }

#DATA FOR CA SUMMARIES
d1 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/Current", ibra_hosk, "current_maxent", "current_maxent", tool="Maxent")
d2 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/Maxent_2021", ibra_hosk, "2021_maxent", "2021_maxent", tool="Maxent")
d3 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/Maxent_2042", ibra_hosk, "2042_maxent", "2042_maxent", tool="Maxent")
d4 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/Current", ibra_hosk, "current_nichemapper", "current_nichem", tool="NicheMapper")
d5 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/NicheMapper_2021", ibra_hosk, "2021_nichemapper", "2021_nichem", tool="NicheMapper")
d6 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/NicheMapper_2042", ibra_hosk, "2042_nichemapper", "2042_nichem", tool="NicheMapper")
d7 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/2070_Access_1.3", ibra_hosk, "2070_nichemapper", "2070_nichem", tool="NicheMapper")
d8 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/2070_HadGEM2-CC", ibra_hosk, "2070_nichemapper", "2070_nichem", tool="NicheMapper")
d9 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/2070_ACCESS_1.3", ibra_hosk, "2070_maxent", "2070_maxent", tool="Maxent")
d10 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/2070_HadGEM2-CC", ibra_hosk, "2070_maxent", "2070_maxent", tool="Maxent")


all_long <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
write_csv(all_long, paste0("Climate_briscoe/output/V2_2021SDM_CA/Climate_briscoe_bioregions_hoskings_long.csv"))

all_wide <- all_long %>% select(KLM, STA_CODE, REG_Hoskin, model, area_ha) %>% pivot_wider(names_from = model, names_prefix = "area_ha_", values_from = area_ha)
write_csv(all_wide, paste0("Climate_briscoe/output/V2_2021SDM_CA/Climate_briscoe_bioregions_hoskings_wide.csv"))

#DATA FOR RP SUMMARIES
d1 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/Current", ibra_states, "current_maxent", "current_maxent", tool="Maxent")
d2 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/2070_ACCESS_1.3", ibra_states, "2070_maxent", "2070_maxent", tool="Maxent")
d3 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/Maxent/2070_HadGEM2-CC", ibra_states, "2070_maxent", "2070_maxent", tool="Maxent")
d4 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/Current", ibra_states, "current_nichemapper", "current_nichem", tool="NicheMapper")
d5 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/2070_Access_1.3", ibra_states, "2070_nichemapper", "2070_nichem", tool="NicheMapper")
d6 <- briscoe_fun("D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change/NicheMapper/2070_HadGEM2-CC", ibra_states, "2070_nichemapper", "2070_nichem", tool="NicheMapper")
d7 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/NicheMapper_2030", ibra_states, "2030_nichemapper", "2030_nichem", tool="NicheMapper")
d8 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/NicheMapper_2050", ibra_states, "2050_nichemapper", "2050_nichem", tool="NicheMapper")
d9 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/Maxent_2030", ibra_states, "2030_maxent", "2030_maxent", tool="Maxent")
d10 <- briscoe_fun("D:/Box Sync/DAWE/Climate_change/Climate_briscoe/Interpolated/Maxent_2050", ibra_states, "2050_maxent", "2050_maxent", tool="Maxent")

all_long <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
write_csv(all_long, paste0("Climate_briscoe/output/V2_2021SDM_RP/Climate_briscoe_bioregions_ibra7_long.csv"))

all_wide <- all_long %>% dplyr::select(KLM, STA_CODE, REG_NAME_7, model, area_ha) %>% pivot_wider(names_from = model, names_prefix = "area_ha_", values_from = area_ha)
write_csv(all_wide, paste0("Climate_briscoe/output/V2_2021SDM_RP/Climate_briscoe_bioregions_ibra7_wide.csv"))



###############
###############
#OLD CODE
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
  clim_2021 <- raster::extract(k_2021, habitat_centroids, fun=NULL)
  clim_2030 <- raster::extract(k_2030, habitat_centroids, fun=NULL)
  clim_2042 <- raster::extract(k_2042, habitat_centroids, fun=NULL)
  clim_2050 <- raster::extract(k_2050, habitat_centroids, fun=NULL)
  clim_2070 <- raster::extract(k_2070, habitat_centroids, fun=NULL)
  habitat_clim <- bind_cols(habitat_centroids, clim_curr=clim_curr, clim_2021=clim_2021, clim_2030=clim_2030, clim_2042=clim_2042, clim_2050=clim_2050, clim_2070=clim_2070)

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
        mutate(perc_loss_2021 = round(100 - 100* habitat_ha_clim_2021/habitat_ha_clim_curr, 2),
               perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2042 = round(100 - 100* habitat_ha_clim_2042/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
    
      states_df <- df %>% group_by(threshold, STA_CODE, scenario) %>%
        summarise(habitat_ha = sum(habitat_ha)) %>% ungroup() %>%
        pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha) %>%
        mutate(perc_loss_2021 = round(100 - 100* habitat_ha_clim_2021/habitat_ha_clim_curr, 2),
               perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2042 = round(100 - 100* habitat_ha_clim_2042/habitat_ha_clim_curr, 2),
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
        mutate(perc_loss_2021 = round(100 - 100* habitat_ha_clim_2021/habitat_ha_clim_curr, 2),
               perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2042 = round(100 - 100* habitat_ha_clim_2042/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
      
      states_df <- df %>% group_by(threshold, STA_CODE, scenario) %>%
        summarise(habitat_ha = sum(habitat_ha)) %>% ungroup() %>%
        pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha) %>%
        mutate(perc_loss_2021 = round(100 - 100* habitat_ha_clim_2021/habitat_ha_clim_curr, 2),
               perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2042 = round(100 - 100* habitat_ha_clim_2042/habitat_ha_clim_curr, 2),
               perc_loss_2050 = round(100 - 100* habitat_ha_clim_2050/habitat_ha_clim_curr, 2),
               perc_loss_2070 = round(100 - 100* habitat_ha_clim_2070/habitat_ha_clim_curr, 2)) 
      
      all_wide <- add_row(df_wide, states_df) 
    
    } else if(subsname=="krange"){  
      df <- df %>%
        group_by(scenario) %>%
        summarise(habitat_ha = round(sum(habitat_ha_likely, na.rm=TRUE), 0)) %>% ungroup() %>%
        mutate(threshold=i)
      all_wide <- df %>% pivot_wider(names_from = scenario, names_prefix = "habitat_ha_", values_from = habitat_ha, values_fill=0) %>%
        mutate(perc_loss_2021 = round(100 - 100* habitat_ha_clim_2021/habitat_ha_clim_curr, 2),
               perc_loss_2030 = round(100 - 100* habitat_ha_clim_2030/habitat_ha_clim_curr, 2),
               perc_loss_2042 = round(100 - 100* habitat_ha_clim_2042/habitat_ha_clim_curr, 2),
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
  
  
  
  
  

  
  
  
  
    