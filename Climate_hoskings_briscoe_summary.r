library(raster)
library(sf)
library(tidyverse)

setwd("D:/Box Sync/DAWE/Climate_change/")


hosk <- read_csv(paste0("Climate_hoskings/output/Climate_hoskings_bioregions_hoskings.csv"))
brisc_long <- read_csv(paste0("Climate_briscoe/output/Climate_briscoe_bioregions_hoskings_long.csv"))
brisc_wide <- read_csv(paste0("Climate_briscoe/output/Climate_briscoe_bioregions_hoskings_wide.csv"))

hosk <- hosk %>% filter(threshold==0.407)
#alldat <- dplyr::left_join(hosk, brisc, by=c("KLM", "STA_CODE", "REG_Hoskin"))
#write_csv(alldat, paste0("Climate_all_bioregions_hoskings_merged.csv"))


b1 <- brisc_long %>% filter(model_1=="averages") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
  filter(KLM %in% c(26, 36)) %>%
  group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() 
b1a <- b1 %>% group_by(STA_CODE, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(REG_Hoskin="WHOLE_STATE")
b1b <- b1 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b1all <- rbind(b1, b1a, b1b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))
    

b2 <- brisc_long %>% filter(model_1=="extremesA") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
      filter(KLM %in% c(26, 36)) %>%
      group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
      summarise(area_ha = sum(area_ha)) %>%
      ungroup() 
b2a <- b2 %>% group_by(STA_CODE, model_1, year, GCM) %>%
      summarise(area_ha = sum(area_ha)) %>%
      mutate(REG_Hoskin="WHOLE_STATE")
b2b <- b2 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b2all <- rbind(b2, b2a, b2b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))

b3 <- brisc_long %>% filter(model_1=="extremesB") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
      filter(KLM %in% c(26, 36)) %>%
      group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
      summarise(area_ha = sum(area_ha)) %>%
      ungroup() 
b3a <- b3 %>% group_by(STA_CODE, model_1, year, GCM) %>%
      summarise(area_ha = sum(area_ha)) %>%
      mutate(REG_Hoskin="WHOLE_STATE")
b3b <- b3 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b3all <- rbind(b3, b3a, b3b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))

b4 <- brisc_long %>% filter(model_1=="high") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
  filter(KLM %in% c(26, 36)) %>%
  group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() 
b4a <- b4 %>% group_by(STA_CODE, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(REG_Hoskin="WHOLE_STATE")
b4b <- b4 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b4all <- rbind(b4, b4a, b4b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))

b5 <- brisc_long %>% filter(model_1=="med") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
  filter(KLM %in% c(26, 36)) %>%
  group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() 
b5a <- b5 %>% group_by(STA_CODE, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(REG_Hoskin="WHOLE_STATE")
b5b <- b5 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b5all <- rbind(b5, b5a, b5b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))

b6 <- brisc_long %>% filter(model_1=="low") %>% select(KLM, STA_CODE, REG_Hoskin, area_ha, model_1, year, GCM) %>%
  filter(KLM %in% c(26, 36)) %>%
  group_by(STA_CODE, REG_Hoskin, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() 
b6a <- b6 %>% group_by(STA_CODE, model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(REG_Hoskin="WHOLE_STATE")
b6b <- b6 %>% group_by(model_1, year, GCM) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
b6all <- rbind(b6, b6a, b6b) %>% 
  pivot_wider(names_from = c(year, GCM), names_prefix = "area_ha_", values_from = area_ha) %>%
  mutate(perc_loss_2021_ACC = round(100 - 100* area_ha_2021_ACC/area_ha_current_NA, 2),
         perc_loss_2021_HAD = round(100 - 100* area_ha_2021_HAD/area_ha_current_NA, 2),
         perc_loss_2042_ACC = round(100 - 100* area_ha_2042_ACC/area_ha_current_NA, 2),
         perc_loss_2042_HAD = round(100 - 100* area_ha_2042_HAD/area_ha_current_NA, 2))

ball <- b1all %>% left_join(b2all, by=c("STA_CODE", "REG_Hoskin")) %>%
            left_join(b3all, by=c("STA_CODE", "REG_Hoskin")) %>%
            left_join(b4all, by=c("STA_CODE", "REG_Hoskin")) %>%
            left_join(b5all, by=c("STA_CODE", "REG_Hoskin")) %>%
            left_join(b6all, by=c("STA_CODE", "REG_Hoskin")) 


# boup <- ball %>% mutate(min_perc_2021 = min(across(starts_with("perc_loss_2021")), na.rm=TRUE),
#                      max_perc_2021 = max(c_across(starts_with("perc_loss_2021")), na.rm=TRUE),
#                      median_perc_2021 = median(c_across(starts_with("perc_loss_2021")), na.rm=TRUE),
#                      min_perc_2042 = min(c_across(starts_with("perc_loss_2042")), na.rm=TRUE),
#                      max_perc_2042 = max(c_across(starts_with("perc_loss_2042")), na.rm=TRUE),
#                       median_perc_2042 = median(c_across(starts_with("perc_loss_2042")), na.rm=TRUE))                        


hosk1 <- hosk %>% filter(KLM %in% c(26, 36) & threshold==0.407) %>%
  select(!threshold) %>%
  group_by(STA_CODE, REG_Hoskin) %>%
  summarise(area_ha_current = sum(area_ha_current),
            area_ha_2021 = sum(area_ha_yr2021),
            area_ha_2042 = sum(area_ha_yr2042)) %>%
  ungroup() 
hosk2 <- hosk1 %>% group_by(STA_CODE) %>%
  summarise(area_ha_current = sum(area_ha_current),
            area_ha_2021 = sum(area_ha_2021),
            area_ha_2042 = sum(area_ha_2042)) %>%
  mutate(REG_Hoskin="WHOLE_STATE")
hosk3 <- hosk2 %>% 
  summarise(area_ha_current = sum(area_ha_current),
            area_ha_2021 = sum(area_ha_2021),
            area_ha_2042 = sum(area_ha_2042)) %>%
  mutate(STA_CODE="WHOLE_RANGE", REG_Hoskin="WHOLE_RANGE")
hoskall <- rbind(hosk1, hosk2, hosk3) %>% 
  mutate(perc_loss_2021 = round(100 - 100* area_ha_2021/area_ha_current, 2),
         perc_loss_2042 = round(100 - 100* area_ha_2042/area_ha_current, 2))



ball <- left_join(ball, hoskall, by=c("STA_CODE", "REG_Hoskin"))

write_csv(ball, "Climate_all_bioregions_hoskings_merged.csv")



