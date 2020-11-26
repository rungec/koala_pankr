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
#Regional ecosytem suitability 1-3
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/Output/Gridded_data/")
oupdir <- "clean/"


load(paste0(oupdir, "koala_gridded_vars_100ha_tidy.Rdata"))

k_tmp <- k_fix %>% st_set_geometry(NULL) %>% select(cellid)

joinfun <- function(biome){
  for(i in 1:3){
  filelist <- list.files("D:/Box Sync/GPEM_Postdoc/Koala_NESP/06_Analysis/Sofia/Qld_habitat/Task1/", pattern=".shp$", full.names=TRUE)
  currshp <- grep(pattern=biome, filelist, value=TRUE)
  currshp <- grep(pattern=paste0("RE",i), currshp, value=TRUE)
  re <- st_read(currshp)
  
  re <- re %>% st_set_geometry(NULL) %>% 
    filter(!cellid==0) %>% 
    filter(habitat==1) 
  names(re)[grep("AREA", names(re))] <- "Area_ha"
    re <- re %>% select(cellid, habitat, "Area_ha") %>% 
          group_by(cellid) %>% summarise(habitat = max(habitat),
                                          Area_ha = sum(Area_ha))
  print(max(re$Area_ha, na.rm=TRUE))
  k_tmp <- k_tmp %>% left_join(re, by='cellid')
  names(k_tmp)[(ncol(k_tmp)-1):ncol(k_tmp)] <- c(paste0("re", i, "_habitat_", biome), paste0("re", i, "_area_", biome))
  }
  return(k_tmp)
}


k_tmp <- joinfun("Brigalow_Belt")
k_tmp <- joinfun("Central_QC")
k_tmp <- joinfun("Desert_Uplands")
k_tmp <- joinfun("Einasleigh_Uplands")
k_tmp <- joinfun("Mitchell_Grass")
k_tmp <- joinfun("Mulga_Lands")
k_tmp <- joinfun("New_England")
k_tmp <- joinfun("Wet_trop")
k_tmp <- joinfun("SEQ_")

k_mat <- as.matrix(k_tmp)
k_mat[is.na(k_mat)] <- 0
k_oup <- k_tmp %>% select(cellid)
k_oup$re_suitable_1_ha_qld <- Rfast::rowsums(k_mat[, grep("re1_area", colnames(k_mat))])
k_oup$habitat_present_1_ha_qld <- Rfast::rowsums(k_mat[, grep("re1_habitat", colnames(k_mat))])
k_oup$re_suitable_2_ha_qld <- Rfast::rowsums(k_mat[, grep("re2_area", colnames(k_mat))])
k_oup$habitat_present_2_ha_qld <- Rfast::rowsums(k_mat[, grep("re2_habitat", colnames(k_mat))])
k_oup$re_suitable_3_ha_qld <- Rfast::rowsums(k_mat[, grep("re3_area", colnames(k_mat))])
k_oup$habitat_present_3_ha_qld <- Rfast::rowsums(k_mat[, grep("re3_habitat", colnames(k_mat))])

k_fix <- k_fix %>% select(!c(re_suitable_12_ha_qld, re_suitable_1_ha_qld, re_suitable_3_ha_qld, habitat_present_3_qld))
k_fix <- k_fix %>% left_join(k_oup, by='cellid')

###################################
save(k_fix, file=paste0(oupdir, "koala_gridded_vars_100ha_tidy_v2.Rdata"))
#st_write(k_fix, paste0(oupdir, "koala_gridded_vars_100ha_tidy.gpkg")) 

