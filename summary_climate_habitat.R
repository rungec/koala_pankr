#################
library(sf)
library(tidyverse)


#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
datadir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/08_Project_outputs/NIKA/NIKA_inputs/gridded_data/"
oupdir <- "Output/"

k_fix <- read_sf(paste0(datadir, "NIKA_inputs.gpkg"))
k_dat <- k_fix %>% st_set_geometry(NULL)

ibra <- read.csv("Output/Gridded_data/clean/hoskings_ibra_cellid.txt")
ibra7 <- read.csv("D:/Box Sync/DAWE/Land_use_change/output/Harmonised_Koala_Habitat_v1_bioregions.csv") %>%
            select(cellid, bioregion_id, STA_CODE, REG_NAME_7)

################BY STATE
#Area of koala habitat under climate change
a <- k_dat %>% filter(climate_2070_core==12 & !is.na(climate_2070_core)) %>% summarise(Allmodels_core_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
b <- k_dat %>% filter(climate_2070_mid==12 & !is.na(climate_2070_mid)) %>% summarise(Allmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
d <- k_dat %>% filter(climate_2070_mid>6 & !is.na(climate_2070_mid)) %>% summarise(Majmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
d2 <- k_dat %>% filter(climate_2070_mid>0 & !is.na(climate_2070_mid)) %>% summarise(Anymodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
e <- k_dat %>% summarise(Current_habitat_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
e2 <- k_dat %>% filter(climate_Current_core==6 & !is.na(climate_Current_core)) %>%  summarise(Current_habitat_core_ha = sum(habitat_ha_likely, na.rm=TRUE)) 

#Area of koala habitat under climate change, nsw
f <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% filter(climate_2070_core==12 & !is.na(climate_2070_core)) %>% summarise(Allmodels_core_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
g <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% filter(climate_2070_mid==12 & !is.na(climate_2070_mid)) %>% summarise(Allmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
h <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% filter(climate_2070_mid>6 & !is.na(climate_2070_mid)) %>% summarise(Majmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
h2 <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% filter(climate_2070_mid>0 & !is.na(climate_2070_mid)) %>% summarise(Anymodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
i <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% summarise(Current_habitat_ha = sum(habitat_ha_likely, na.rm=TRUE))
i2 <- k_dat %>% filter(nsw_eastern==1|nsw_western==1) %>% filter(climate_Current_core==6 & !is.na(climate_Current_core)) %>% summarise(Current_habitat_core_ha = sum(habitat_ha_likely, na.rm=TRUE))

#Area of koala habitat under climate change, qld
j <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% filter(climate_2070_core==12 & !is.na(climate_2070_core)) %>% summarise(Allmodels_core_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
k <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% filter(climate_2070_mid==12 & !is.na(climate_2070_mid)) %>% summarise(Allmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
l <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% filter(climate_2070_mid>6 & !is.na(climate_2070_mid)) %>% summarise(Majmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
l2 <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% filter(climate_2070_mid>0 & !is.na(climate_2070_mid)) %>% summarise(Anymodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
m <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% summarise(Current_habitat_ha = sum(habitat_ha_likely, na.rm=TRUE))
m2 <- k_dat %>% filter(qld_seq==1|qld_notseq==1) %>% filter(climate_Current_core==6 & !is.na(climate_Current_core)) %>% summarise(Current_habitat_core_ha = sum(habitat_ha_likely, na.rm=TRUE))


df <- rbind(data.frame(a, b, d,d2, e), data.frame(f, g, h,h2,i), data.frame(j,k,l,l2,m))
df$region <- c("rangewide", "nsw", "qld") 

df <- df %>% mutate(habitat_loss_allmodels_core = Current_habitat_ha - Allmodels_core_2070_ha,
                    habitat_loss_allmodels_mid = Current_habitat_ha - Allmodels_mid_2070_ha,
                    habitat_loss_anymodels_mid = Current_habitat_ha - Anymodels_mid_2070_ha,
                    habitat_loss_majmodels_mid = Current_habitat_ha - Majmodels_mid_2070_ha)

df <- df %>% mutate(percent_loss_allmodels_core = 100*habitat_loss_allmodels_core/Current_habitat_ha,
                    percent_loss_allmodels_mid = 100*habitat_loss_allmodels_mid/Current_habitat_ha,
                    percent_loss_anymodels_mid = 100*habitat_loss_anymodels_mid/Current_habitat_ha,
                    percent_loss_majmodels_mid = 100*habitat_loss_majmodels_mid/Current_habitat_ha)
df <- t(df)

write.csv(df, paste0(oupdir, "Summary_habitat_lost_to_climatechange.csv"))

################BY BIOREGION
#Bioregions drawn from IBRA7, modified to match Adams-Hosking 2016

k_ibra <- k_dat %>% left_join(ibra, by='cellid') 

#Area of koala habitat under climate change
a <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% filter(climate_2070_core==12 & !is.na(climate_2070_core)) %>% summarise(Allmodels_core_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
b <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% filter(climate_2070_mid==12 & !is.na(climate_2070_mid)) %>% summarise(Allmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
d <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% filter(climate_2070_mid>6 & !is.na(climate_2070_mid)) %>% summarise(Majmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
e <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% filter(climate_2070_mid>0 & !is.na(climate_2070_mid)) %>% summarise(Anymodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
f <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% summarise(Current_habitat_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
g <- k_ibra %>% group_by(REG_Hoskin, STA_CODE) %>% filter(climate_Current_core==6 & !is.na(climate_Current_core)) %>%  summarise(Current_habitat_core_ha = sum(habitat_ha_likely, na.rm=TRUE)) 

df <- f %>% left_join(g, by=c('REG_Hoskin', 'STA_CODE')) %>%
            left_join(a, by=c('REG_Hoskin', 'STA_CODE')) %>%
            left_join(b, by=c('REG_Hoskin', 'STA_CODE')) %>%
            left_join(d, by=c('REG_Hoskin', 'STA_CODE')) %>%
            left_join(e, by=c('REG_Hoskin', 'STA_CODE')) 
df[is.na(df), ] <- 0
write.csv(df, paste0(oupdir, "Summary_habitat_lost_to_climatechange_bioregion.csv"))

################BY BIOREGION
#Bioregions drawn from IBRA7
k_ibra <- k_dat %>% left_join(ibra7, by='cellid') 

#Area of koala habitat under climate change
a <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% filter(climate_2070_core==12 & !is.na(climate_2070_core)) %>% summarise(Allmodels_core_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
b <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% filter(climate_2070_mid==12 & !is.na(climate_2070_mid)) %>% summarise(Allmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
d <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% filter(climate_2070_mid>6 & !is.na(climate_2070_mid)) %>% summarise(Majmodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
e <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% filter(climate_2070_mid>0 & !is.na(climate_2070_mid)) %>% summarise(Anymodels_mid_2070_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
f <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% summarise(Current_habitat_ha = sum(habitat_ha_likely, na.rm=TRUE)) 
g <- k_ibra %>% group_by(REG_NAME_7, STA_CODE) %>% filter(climate_Current_core==6 & !is.na(climate_Current_core)) %>%  summarise(Current_habitat_core_ha = sum(habitat_ha_likely, na.rm=TRUE)) 

df <- f %>% left_join(g, by=c('REG_NAME_7', 'STA_CODE')) %>%
  left_join(a, by=c('REG_NAME_7', 'STA_CODE')) %>%
  left_join(b, by=c('REG_NAME_7', 'STA_CODE')) %>%
  left_join(d, by=c('REG_NAME_7', 'STA_CODE')) %>%
  left_join(e, by=c('REG_NAME_7', 'STA_CODE')) %>% 
  filter(STA_CODE %in% c("QLD", "NSW", "ACT", "JBT")) %>%
  replace_na(list(Current_habitat_ha=0, Current_habitat_core_ha=0, Allmodels_core_2070_ha=0, Allmodels_mid_2070_ha=0, Majmodels_mid_2070_ha=0, Anymodels_mid_2070_ha=0))


write.csv(df, paste0(oupdir, "Summary_habitat_lost_to_climatechange_bioregion_ibra7.csv"))

