#################
library(sf)
library(tidyverse)

#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"

source(paste0(getwd(), "/R_scripts/koala_pankr/fastbindfun.r"))

cell_area = "100ha" 
ncore = 1 #EDIT number of cores to use for parallel #put 1 if don't want to run in parallel
n_splits = 1000

load(paste0(oupdir, "koala_gridded_data_",cell_area,"1.Rdata"))
k_all <- k_grid


k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="pawc", grid=k_grid)
save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"2.Rdata"))

k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="soil", grid=k_grid) 
save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"3.Rdata"))

k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern="fire", grid=k_grid)
save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"4.Rdata"))

k_all <- k_grid
load(paste0(oupdir, "koala_gridded_data_",cell_area,"5.Rdata"))
k_grid <- k_grid %>% st_set_geometry(NULL)
k_all <- k_all %>% left_join(k_grid, by="cellid")
k_grid <- k_all
save(k_grid, file = paste0(oupdir, "koala_gridded_data_",cell_area,"5.Rdata"))

k6 <- fastbindfun(paste0(oupdir, "temp"), pattern="lulc", grid=k_grid)
save(k_grid, file=paste0(oupdir, "koala_gridded_data_",cell_area,"6.Rdata")) 



#k_grid <- k_grid %>% bind_cols(snes_likelyhabitat_ha = snes_likely, snes_maybehabitat_ha = snes_maybe)
load(paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata"))
k7 <- k7 %>% st_set_geometry(NULL)
k_grid <- left_join(k_grid, k7, by=c('cellid', 'splits'))
k_grid <- k_grid %>% rename(snes_maybehabitat_ha=snes_may, snes_likelyhabitat_ha=snes_likely)

save(k_grid, file=paste0(oupdir, "koala_gridded_data_",cell_area,"7.Rdata")) 


#Climate
patterns <- paste("climate", rep(c("2070", "Current"), each=3), c("perc99ofrecords", "perc95ofrecords", "perc90ofrecords"), sep="_")
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[1], grid=k_grid)
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[2], grid=k_grid)
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[3], grid=k_grid)
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[4], grid=k_grid)
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[5], grid=k_grid)
k_grid <- fastbindfun(paste0(oupdir, "temp"), pattern=patterns[6], grid=k_grid)

k_grid <- setNames(k_grid, c(names(st_set_geometry(k_grid, NULL))[1:14], patterns, "geometry"))

save(k_grid, file = paste0(oupdir, "koala_gridded_clim_",cell_area,".Rdata"))

curregion="SEQ"
habitat <- read.csv(paste0(oupdir, "koala_gridded_vars_100ha_SEQ.csv"))
k_grid <- k_grid %>% left_join(habitat, by='cellid') 
save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".Rdata"))

save(k_grid, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".Rdata"))
st_write(k_grid, paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, ".gpkg"), driver='GPKG')


kfix <- k_grid
kfix <- kfix %>% mutate(pawc_mean = replace(pawc_mean, is.nan(pawc_mean), NA),
                        pawc_max = replace(pawc_max, is.infinite(pawc_max), NA),
                        soildepth_mean = replace(soildepth_mean, is.nan(soildepth_mean), NA),
                       firefreq_88to15 = replace(firefreq_88to15, is.infinite(firefreq_88to15), NA),
                       climate_2070_perc99ofrecords = replace(climate_2070_perc99ofrecords, is.infinite(climate_2070_perc99ofrecords), NA),
                       climate_2070_perc95ofrecords = replace(climate_2070_perc95ofrecords, is.infinite(climate_2070_perc95ofrecords), NA),
                       climate_2070_perc90ofrecords = replace(climate_2070_perc90ofrecords, is.infinite(climate_2070_perc90ofrecords), NA),
                       climate_Current_perc99ofrecords = replace(climate_Current_perc99ofrecords, is.infinite(climate_Current_perc99ofrecords), NA),
                       climate_Current_perc95ofrecords = replace(climate_Current_perc95ofrecords, is.infinite(climate_Current_perc95ofrecords), NA),
                       climate_Current_perc90ofrecords = replace(climate_Current_perc90ofrecords, is.infinite(climate_Current_perc90ofrecords), NA))

save(kfix, file = paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, "_tidy.Rdata"))
st_write(kfix, paste0(oupdir, "koala_gridded_vars_", cell_area, curregion, "_tidy.gpkg"), driver='GPKG')
