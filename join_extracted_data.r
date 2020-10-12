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
#Regional ecosytem suitability
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

k_fix <- k_fix %>% mutate(habitat_area_ha_qld = case_when(re_suitable_1_ha_qld > 0 & complexsdm_value > 0.444 ~ re_suitable_1_ha_qld))

