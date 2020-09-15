library(sf)
library(tidyverse)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"
cell_area = "100ha" 

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
