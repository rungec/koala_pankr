library(sf)
library(tidyverse)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"
cell_area = "100ha" 

##########################################################
load(paste0(oupdir, "koala_templatehexgrid_",cell_area,".Rdata"))
df <- read.csv(file.choose())


df <- df %>% mutate(snes_may = case_when(MAX_FID_Phascolarctos_cinereus_may == -1 ~ 0,
                                            TRUE ~ 1))
df <- df %>% dplyr::select(cellid, snes_may) %>% filter(cellid > 0)
k1 <- k_grid %>% left_join(df, by='cellid', keep=FALSE)

df <- df %>% mutate(snes_likely = case_when(MAX_FID_Phascolarctos_cinereus_likel_diss == -1 ~ 0,
                                            TRUE ~ 1))
df <- df %>% dplyr::select(cellid, snes_likely) %>% filter(cellid > 0)
k2 <- k1 %>% left_join(df, by='cellid', keep=FALSE)


summary(k2)
k_grid <- k2
save(k_grid, file=paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata"))
st_write(k2, paste0(oupdir, "koala_gridded_data_", cell_area, "7.shp"))

#SEQ
df <- read.csv("Output\\test\\SEQ_habitat_area.csv")
df <- df %>% dplyr::select(cellid, SUM_Area_ha)
df <- setNames(df, c('cellid', 'habitat_area_ha_SEQ'))
write.csv(df, "Output/Gridded_data/koala_gridded_vars_100ha_SEQ.csv", row.names=FALSE)

k_grid <- left_join(k_grid, df, by='cellid')

###############################
#Regional ecosytem suitability 1 & 2
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
oupdir <- "Output/Gridded_data/clean/"
cell_area = "100ha" 

load(paste0(oupdir, "koala_gridded_vars_100ha_tidy.Rdata"))

re_1 <- st_read(dsn="C:/Users/Claire/Documents/ArcGIS/Default.gdb", layer="koala_re_qld_Union_1")

re_1 <- re_1 %>% filter(!cellid==0) %>% st_set_geometry(NULL)

re_1_group <- re_1 %>% mutate(habitat = case_when(FID_Qld_RE_utility_Dissolve_Proj1_singlepart == -1 ~ 0,
                                                  TRUE ~ 1))
re_1_group <- re_1_group %>% filter(habitat == 1) %>% 
  group_by(cellid) %>% 
  summarise(re_suitable_1_ha_qld = round(sum(Shape_Area)/10000, 0)) %>%
  ungroup()

re_1_group <- re_1_group %>% select(cellid, re_suitable_1_ha_qld)
rm(re_1)

re_12 <- st_read(dsn="C:/Users/Claire/Documents/ArcGIS/Default.gdb", layer="koala_re_qld_Union_12")

re_12 <- re_12 %>% filter(!cellid==0) %>% st_set_geometry(NULL)

re_12_group <- re_12 %>% mutate(habitat = case_when(FID_Qld_RE_utility_Dissolve_Proj12_singlepart == -1 ~ 0,
                                                    TRUE ~ 1))
re_12_group <- re_12_group %>% filter(habitat == 1) %>% 
  group_by(cellid) %>% 
  summarise(re_suitable_12_ha_qld = round(sum(Shape_Area)/10000, 0)) %>%
  ungroup()

re_12_group <- re_12_group %>% select(cellid, re_suitable_12_ha_qld)
rm(re_12)

k_fix <- left_join(k_fix, re_1_group, by='cellid')
k_fix <- left_join(k_fix, re_12_group, by='cellid')

###############################
#Regional ecosytem suitability 3
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/Gridded_data/")
oupdir <- "clean/"

load(paste0(oupdir, "koala_gridded_vars_100ha_tidy.Rdata"))

k_tmp <- k_fix %>% st_set_geometry(NULL) %>% select(cellid)

joinfun <- function(biome){
  re <- st_read(dsn="intermediate/re_biome.gdb", layer=biome)
  re <- re %>% st_set_geometry(NULL) %>% 
    filter(!cellid==0) %>% 
    filter(habitat==1) %>%
    select(cellid, habitat, "SUM_AREA_GEO")
  print(max(re$SUM_AREA_GEO, na.rm=TRUE))
  k_tmp <- k_tmp %>% left_join(re, by='cellid')
  names(k_tmp)[ncol(k_tmp)] <- paste0("re_3_", biome)
  return(k_tmp)
}


k_tmp <- joinfun("brigalow")
k_tmp <- joinfun("centralqldcoast")
k_tmp <- joinfun("desertup")
k_tmp <- joinfun("eineup")
k_tmp <- joinfun("mitchell")
k_tmp <- joinfun("mulga")
k_tmp <- joinfun("neweng")
k_tmp <- joinfun("seq")
k_tmp <- joinfun("wettrop")

k_agg <- k_tmp %>% rowwise() %>% mutate(re_suitable_3_ha_qld = sum(across(contains("re_3")), na.rm=TRUE),
                                        habitat_present_3_qld = sum(across(contains("habitat")), na.rm=TRUE))

k_fix <- k_fix %>% left_join(k_agg, by='cellid')

###################################
save(k_fix, file=paste0(oupdir, "koala_gridded_vars_100ha_tidy.Rdata"))
st_write(k_fix, paste0(oupdir, "koala_gridded_vars_100ha_tidy.gpkg")) 

