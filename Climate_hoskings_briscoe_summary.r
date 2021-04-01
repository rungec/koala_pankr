library(raster)
library(sf)
library(tidyverse)

setwd("D:/Box Sync/DAWE/Climate_change/")

###########################
###SUMMARISE CLIMATE PROJECTIONS FOR RP (2070)
#Using 2021 SDM and IBRA7 bioregions
hosk <- read_csv(paste0("Climate_hoskings/output/V2_2021SDM/Climate_hoskings_bioregions_ibra7.csv"))
brisc_long <- read_csv(paste0("Climate_briscoe/output/V2_2021SDM_RP/Climate_briscoe_bioregions_ibra7_long.csv"))

###summarise hoskings climate projection
#reformat
hosk <- hosk %>% filter(threshold==0.407 & KLM %in% c(26, 36)) %>% select(!threshold) %>%
                group_by(STA_CODE, REG_NAME_7) %>%
                summarise(area_ha_current = sum(area_ha_current),
                          area_ha_2070 = sum(area_ha_yr2070)) %>% ungroup()

#add state and whole range summaries
hosk2 <- hosk %>% group_by(STA_CODE) %>%
  summarise(area_ha_current = sum(area_ha_current),
            area_ha_2070 = sum(area_ha_2070)) %>%
  mutate(REG_NAME_7="WHOLE_STATE")
hosk3 <- hosk2 %>% 
  summarise(area_ha_current = sum(area_ha_current),
            area_ha_2070 = sum(area_ha_2070)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_NAME_7="WHOLE_RANGE")
hoskall <- rbind(hosk, hosk2, hosk3) %>% 
            mutate(model_1 = "Adams_hoskings")

###summarise briscoe climate projection
#reformat
brisc_long <- brisc_long %>% filter(KLM %in% c(26, 36)) %>% #select known & likely range
              mutate(year = str_extract(scenario, "[^_]+"),
                     GCM = case_when(str_detect(model, "ACC")==TRUE ~ "ACC",
                                     str_detect(model, "Acc")==TRUE ~ "ACC",
                                     str_detect(model, "Had")==TRUE ~ "HAD"),
                     model_1 = case_when(str_detect(model, "averages")==TRUE ~ "averages",
                                         str_detect(model, "extremesA")==TRUE ~ "extremesA",
                                         str_detect(model, "extremesB")==TRUE ~ "extremesB",
                                         str_detect(model, "high")==TRUE ~ "high",
                                         str_detect(model, "med")==TRUE ~ "med",
                                         str_detect(model, "low")==TRUE ~ "low")) %>%
              select(STA_CODE, REG_NAME_7, area_ha, model_1, year, GCM) 
#add state and whole range summaries   
b1 <- brisc_long %>%  
  group_by(STA_CODE, REG_NAME_7, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() 
b1a <- b1 %>% group_by(STA_CODE, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(REG_NAME_7="WHOLE_STATE")
b1b <- b1 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_NAME_7="WHOLE_RANGE")
b1all <- rbind(b1, b1a, b1b) 
b1curr <- b1all %>% filter(year=="current") %>% rename(area_ha_current = area_ha) %>%
            select(!c(GCM, year)) %>% 
            mutate(model_1 = paste(model_1, "ACC", sep="_"))
b1curr2 <- b1all %>% filter(year=="current") %>% rename(area_ha_current = area_ha) %>%
            select(!c(GCM, year)) %>% 
            mutate(model_1 = paste(model_1, "HAD", sep="_"))
b1curr <- rbind(b1curr, b1curr2)
b12070 <- b1all %>% filter(year==2070) %>%
          mutate(model_1 = paste(model_1, GCM, sep="_")) %>% rename(area_ha_2070 = area_ha) %>% 
          select(!c(GCM, year))
 
###Join briscoe & hoskings climate estimates
alldf <- left_join(b1curr, b12070, by=c("STA_CODE", "REG_NAME_7", "model_1")) %>% 
          bind_rows(hoskall) %>% 
          mutate(perc_loss_historical_to_2070 = round(100 - 100* area_ha_2070/area_ha_current, 2)) %>%
          mutate(perc_loss_historical_to_2070 = case_when(is.na(perc_loss_historical_to_2070) ~ 0, 
                                                          !is.na(perc_loss_historical_to_2070) ~ perc_loss_historical_to_2070))

write_csv(alldf, "Aggregated_summaries/V2_2021SDM_RP/Climate_all_bioregions_ibra7_merged.csv")

###Summarise across all 13 models

alldfsumm <- alldf %>% group_by(STA_CODE, REG_NAME_7) %>%
                      summarise(min_perc_loss_historical_to_2070 = min(perc_loss_historical_to_2070, na.rm=TRUE),
                                max_perc_loss_historical_to_2070 = max(perc_loss_historical_to_2070, na.rm=TRUE),
                                median_perc_loss_historical_to_2070 = median(perc_loss_historical_to_2070, na.rm=TRUE))

write_csv(alldfsumm, "Aggregated_summaries/V2_2021SDM_RP/Climate_all_bioregions_ibra7_summary.csv")




###########################
###SUMMARISE CLIMATE PROJECTIONS FOR CA (2021, 2042)
#Using 2021 SDM & IBRA7 bioregions adjusted to match Hoskings 2016
hosk <- read_csv(paste0("Climate_hoskings/output/V2_2021SDM/Climate_hoskings_bioregions_hoskings.csv"))
brisc_long <- read_csv(paste0("Climate_briscoe/output/V2_2021SDM_CA/Climate_briscoe_bioregions_hoskings_long.csv"))

###summarise hoskings climate projection
  #reformat
  hosk <- hosk %>% filter(threshold==0.407 & KLM %in% c(26, 36)) %>% select(!threshold) %>%
    group_by(STA_CODE, REG_Hoskin) %>%
    summarise(area_ha_current = sum(area_ha_current),
              area_ha_2021 = sum(area_ha_yr2021),
              area_ha_2042 = sum(area_ha_yr2042),
              area_ha_2070 = sum(area_ha_yr2070)) %>% ungroup()
  
  #add state and whole range summaries
  hosk2 <- hosk %>% group_by(STA_CODE) %>%
    summarise(area_ha_current = sum(area_ha_current),
              area_ha_2021 = sum(area_ha_2021),
              area_ha_2042 = sum(area_ha_2042),
              area_ha_2070 = sum(area_ha_2070)) %>%
    mutate(REG_Hoskin="WHOLE_STATE")
  hosk3 <- hosk2 %>% 
    summarise(area_ha_current = sum(area_ha_current),
              area_ha_2021 = sum(area_ha_2021),
              area_ha_2042 = sum(area_ha_2042),
              area_ha_2070 = sum(area_ha_2070)) %>%
    mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
  hoskall <- rbind(hosk, hosk2, hosk3) %>% 
    mutate(model_1 = "Adams_hoskings")
  
  ###summarise briscoe climate projection
  #reformat
  brisc_long <- brisc_long %>% filter(KLM %in% c(26, 36)) %>% #select known & likely range
    mutate(year = str_extract(scenario, "[^_]+"),
           GCM = case_when(str_detect(model, "ACC")==TRUE ~ "ACC",
                           str_detect(model, "Acc")==TRUE ~ "ACC",
                           str_detect(model, "Had")==TRUE ~ "HAD",
                           str_detect(model, "HAD")==TRUE ~ "HAD"),
           model_1 = case_when(str_detect(model, "averages")==TRUE ~ "averages",
                               str_detect(model, "_av_")==TRUE ~ "averages",
                               str_detect(model, "ExtA")==TRUE ~ "extremesA",
                               str_detect(model, "extremesA")==TRUE ~ "extremesA",
                               str_detect(model, "ExtB")==TRUE ~ "extremesB",
                               str_detect(model, "extremesB")==TRUE ~ "extremesB",
                               str_detect(model, "high")==TRUE ~ "high",
                               str_detect(model, "med")==TRUE ~ "med",
                               str_detect(model, "low")==TRUE ~ "low")) %>%
    select(STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) 
  #add state and whole range summaries   
  b1 <- brisc_long %>%  
    group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
    summarise(area_ha = sum(area_ha)) %>%
    ungroup() 
  b1a <- b1 %>% group_by(STA_CODE, model_1, year, GCM) %>%
    summarise(area_ha = sum(area_ha)) %>%
    mutate(REG_Hoskin="WHOLE_STATE")
  b1b <- b1 %>% group_by(model_1, year, GCM) %>%
    summarise(area_ha = sum(area_ha)) %>%
    mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
  b1all <- rbind(b1, b1a, b1b) 
  b1curr <- b1all %>% filter(year=="current") %>% rename(area_ha_current = area_ha) %>%
    select(!c(GCM, year)) %>% 
    mutate(model_1 = paste(model_1, "ACC", sep="_"))
  b1curr2 <- b1all %>% filter(year=="current") %>% rename(area_ha_current = area_ha) %>%
    select(!c(GCM, year)) %>% 
    mutate(model_1 = paste(model_1, "HAD", sep="_"))
  b1curr <- rbind(b1curr, b1curr2)
  b12021 <- b1all %>% filter(year==2021) %>%
    mutate(model_1 = paste(model_1, GCM, sep="_")) %>% rename(area_ha_2021 = area_ha) %>% 
    select(!c(GCM, year))
  b12042 <- b1all %>% filter(year==2042) %>%
    mutate(model_1 = paste(model_1, GCM, sep="_")) %>% rename(area_ha_2042 = area_ha) %>% 
    select(!c(GCM, year))
  b12070 <- b1all %>% filter(year==2070) %>%
    mutate(model_1 = paste(model_1, GCM, sep="_")) %>% rename(area_ha_2070 = area_ha) %>% 
    select(!c(GCM, year))
  
  ###Join briscoe & hoskings climate estimates
  alldf <- left_join(b1curr, b12021, by=c("STA_CODE", "REG_Hoskin", "model_1")) %>% 
            left_join(b12042, by=c("STA_CODE", "REG_Hoskin", "model_1")) %>% 
            left_join(b12070, by=c("STA_CODE", "REG_Hoskin", "model_1")) %>% 
    bind_rows(hoskall) %>% #replace(.==0, 0.001) %>% 
    mutate(perc_loss_historical_to_2021 = round(100 - 100* area_ha_2021/area_ha_current, 2),
           perc_loss_historical_to_2042 = round(100 - 100* area_ha_2042/area_ha_current, 2),
           perc_loss_historical_to_2070 = round(100 - 100* area_ha_2070/area_ha_current, 2),
           perc_loss_2021_to_2042 = round(100 - 100* area_ha_2042/area_ha_2021, 2)) %>%
    mutate(perc_loss_historical_to_2070 = case_when(is.na(perc_loss_historical_to_2021) ~ 0, 
                                                    !is.na(perc_loss_historical_to_2021) ~ perc_loss_historical_to_2021),
           perc_loss_historical_to_2070 = case_when(is.na(perc_loss_historical_to_2042) ~ 0, 
                                                    !is.na(perc_loss_historical_to_2042) ~ perc_loss_historical_to_2042),
           perc_loss_historical_to_2070 = case_when(is.na(perc_loss_historical_to_2070) ~ 0, 
                                                    !is.na(perc_loss_historical_to_2070) ~ perc_loss_historical_to_2070))
  
  write_csv(alldf, "Aggregated_summaries/V2_2021SDM_CA/Climate_all_bioregions_hosk_merged.csv")
  
  ###Summarise across all 13 models
  
  alldfsumm <- alldf %>% group_by(STA_CODE, REG_Hoskin) %>%
    summarise(min_perc_loss_historical_to_2021 = min(perc_loss_historical_to_2021, na.rm=TRUE),
              max_perc_loss_historical_to_2021 = max(perc_loss_historical_to_2021, na.rm=TRUE),
              median_perc_loss_historical_to_2021 = median(perc_loss_historical_to_2021, na.rm=TRUE),
              min_perc_loss_historical_to_2042 = min(perc_loss_historical_to_2042, na.rm=TRUE),
              max_perc_loss_historical_to_2042 = max(perc_loss_historical_to_2042, na.rm=TRUE),
              median_perc_loss_historical_to_2042 = median(perc_loss_historical_to_2042, na.rm=TRUE),
              min_perc_loss_historical_to_2070 = min(perc_loss_historical_to_2070, na.rm=TRUE),
              max_perc_loss_historical_to_2070 = max(perc_loss_historical_to_2070, na.rm=TRUE),
              median_perc_loss_historical_to_2070 = median(perc_loss_historical_to_2070, na.rm=TRUE),
              min_perc_loss_2021_to_2042 = min(perc_loss_2021_to_2042, na.rm=TRUE),
              max_perc_loss_2021_to_2042 = max(perc_loss_2021_to_2042, na.rm=TRUE),
              median_perc_loss_2021_to_2042 = median(perc_loss_2021_to_2042, na.rm=TRUE))
  
  write_csv(alldfsumm, "Aggregated_summaries/V2_2021SDM_CA/Climate_all_bioregions_hosk_summary.csv")
  
########################


