#This script clips the regional ecosystem mapping v11.0 to bioregions containing koala
#summarises the number and % of RE polygons overlapped by koala occurrences
#and makes a list of all RE containing potential koala trees (Eucalyptus or Corymbia)
#based on the main RE in each polygon (RE1)

#################
require(sf)
require(tidyverse)


#Dirs
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output")
REdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Vegetation/Qld_vege_mapping/Regional_ecosystem_v11/"
occdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Data_inp/"

#Load data
#current koala sightings
currkoala <- st_read(paste0(occdir, "Koala_Qld_noSEQ_2000on_1kmres_noDup.shp"))
st_crs(currkoala) <- CRS("+init=epsg:4283") #set proj as GDA_94 EPSG4283
#current koala sightings
histkoala <- st_read(paste0(occdir, "Koala_Qld_noSEQ_merge_1970topresent_1kmres_noDup.shp"))
st_crs(histkoala) <- CRS("+init=epsg:4283") #set proj as GDA_94 EPSG4283

#Load REDD lookup table
redd <- read_csv(paste0(REdir, 'redd-v11-1-2019.csv'))
#Add column designating any description mentioning Eucalyptus
redd <- redd %>% mutate(Euc_present = case_when(str_detect(`Description`, "Eucalyptus") ~ "Euc",
                                                TRUE ~ "Not"))
#Load regional ecosystem mapping
reveg <- st_read(dsn=paste0(REdir, 'koala.gdb'), layer='Regional_ecosystem_koala_noSEQ')
reveg$oid <- 1:nrow(reveg)
#a number of invalid geometries, lets fix them
#reveg <- st_make_valid(reveg)


############################
#buffer koala sightings to 1km, the accuracy of the dataset
#first project to GDA_94 Aust Albers EPSG 3577 which has units in m 
currkoala <- st_transform(currkoala, 3577)
currkb <- st_buffer(currkoala, 1000)
currkb <- st_transform(currkb, 4283)
overlapkv <- st_intersects(currkb, reveg, sparse=TRUE)

histkoala <- st_transform(histkoala, 3577)
histkb <- st_buffer(histkoala, 1000)
histkb <- st_transform(histkb, 4283)
overlapkvh <- st_intersects(histkb, reveg, sparse=TRUE)



#Join to occurrence and RE attribute tables
tb <- data.frame()
for(i in 1:length(overlapkv)){
  currset <- overlapkv[[i]]
  currveg <- st_drop_geometry(reveg[currset, c("oid", "RE_LABEL", "PC_LABEL", "SHAPE_Area")])
  currpt <- st_drop_geometry(currkb[i, c("OBJECTID", "START_DATE", "LATITUDE", "LONGITUDE")])
  currpt <- do.call("rbind", replicate(nrow(currveg), currpt, simplify = FALSE)) #repeat each row n times
  tb <- rbind(tb, cbind(currpt, currveg))
}

tbh <- data.frame()
for(i in 1:length(overlapkvh)){
  currset <- overlapkvh[[i]]
  currveg <- st_drop_geometry(reveg[currset, c("oid", "RE_LABEL", "PC_LABEL", "SHAPE_Area")])
  currpt <- st_drop_geometry(histkb[i, c("OBJECTID", "START_DATE", "LATITUDE", "LONGITUDE")])
  currpt <- do.call("rbind", replicate(nrow(currveg), currpt, simplify = FALSE)) #repeat each row n times
  tbh <- rbind(tbh, cbind(currpt, currveg))
}

#Split out RE1 in format matching REDD table
tb <- tb %>% mutate(RE1 = case_when(str_detect(RE_LABEL, "/") ~ str_extract(RE_LABEL, "[^//]+"),
                                     TRUE ~ RE_LABEL))
tbh <- tbh %>% mutate(RE1 = case_when(str_detect(RE_LABEL, "/") ~ str_extract(RE_LABEL, "[^//]+"),
                                     TRUE ~ RE_LABEL))

#number of polygons for each RE
Num_RE_polys <- reveg %>% st_drop_geometry() %>% 
            mutate(REfirst = case_when(str_detect(RE_LABEL, "/") ~ str_extract(RE_LABEL, "[^//]+"),
                                       TRUE ~ RE_LABEL)) %>% 
            group_by(REfirst) %>% summarise(n_RE_polys = n())


#number of RE polygons with koala records
#number of koala records in each RE
summarytb <- tb %>% group_by(RE1) %>% summarise(n_koalaocc_recent = n_distinct(OBJECTID),
                                                        n_REpolys_withkoala_recent = n_distinct(oid)) 
summarytbh <- tbh %>% group_by(RE1) %>% summarise(n_koalaocc_inclhist = n_distinct(OBJECTID),
                                                        n_REpolys_withkoala_inclhist = n_distinct(oid)) 

#join summaries to the redd table
redd_oup <- left_join(redd, Num_RE_polys, by = c("re_id" = "REfirst"))
redd_oup <- left_join(redd_oup, summarytb, by = c("re_id" = "RE1"))
redd_oup <- left_join(redd_oup, summarytbh, by = c("re_id" = "RE1"))
redd_oup <- redd_oup %>% mutate(perc_REpolys_withkoala_recent = 100*n_REpolys_withkoala_recent/n_RE_polys,
                                perc_REpolys_withkoala_inclhist = 100*n_REpolys_withkoala_inclhist/n_RE_polys) 
                          
#subset out the REs found in the study region #and that might be associated with koala
redd_oup2 <- redd_oup %>% select(re_id, Description, Euc_present, n_RE_polys, n_koalaocc_recent, n_koalaocc_inclhist, n_REpolys_withkoala_recent, perc_REpolys_withkoala_recent, n_REpolys_withkoala_inclhist, perc_REpolys_withkoala_inclhist) %>% 
                         # filter(n_RE_polys > 0 & Euc_present=='Euc' | n_koalaocc>0 )
                          filter(n_RE_polys > 0 )

#Save
write_csv(redd_oup, "REDD_QldnoSEQ_summary_of_koala_occurrence_v3.csv")
write_csv(redd_oup2, "REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts_v3.csv")

