#TIDY the variables
#drop planning units where not in any region
#output as GDA Albers
#round habtiat_area_seq
#round pawc_mean
#round soil depth and change to mm
#round intact, recoverable and intact areas
#summarise the area of habitat into a single column

#################
library(sf)
library(tidyverse)

setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
oupdir <- "Output/Gridded_data/"
cell_area = "100ha" 

#Load data
load(paste0(oupdir, "clean/koala_gridded_vars_100ha_tidy_v2.Rdata"))

#Drop cells not falling within one of the 4 regions
k_fix2 <- k_fix %>% filter(!(qld_seq==0 & qld_notseq==0 & nsw_eastern==0 & nsw_western==0))

#round columns 
k_fix <- k_fix2 %>% st_transform(3577) %>%
          mutate(habitat_area_ha_seq = round(habitat_area_ha_seq, 0),
                 pawc_mean=round(pawc_mean, 0),
                 intact_area_ha=round(intact_area_ha, 0),
                 recoverable_area_ha=round(recoverable_area_ha, 0),
                 unrecoverable_area_ha=round(unrecoverable_area_ha,0),
                 soildepth_mean=round(soildepth_mean*1000, 0))
k_fix <- k_fix %>%
          mutate(snes_likelyhabitat_ha = round(snes_likelyhabitat_ha*100, 0),
                 snes_maybehabitat_ha = round(snes_maybehabitat_ha*100, 0))

k_fix <- k_fix %>% mutate(intact_area_ha = case_when(intact_area_ha>100 ~ 100, TRUE ~ intact_area_ha),
                          recoverable_area_ha = case_when(recoverable_area_ha>100 ~ 100, TRUE ~ recoverable_area_ha),
                          unrecoverable_area_ha = case_when(unrecoverable_area_ha>100 ~ 100, TRUE ~ unrecoverable_area_ha))

#add column nsw western habitat
k_fix <- k_fix %>% mutate(habitat_area_ha_nsw_west_ess = case_when(nsw_western==1 & complexsdm_value >=0.444 ~ 100),
                          habitat_area_ha_nsw_west_maxk = case_when(nsw_western==1 & complexsdm_value >=0.3925 ~ 100))

#rename columns
k_fix <- k_fix %>% dplyr::rename(habitat_area_ha_seq = habitat_area_ha_SEQ)      

#fix overlapping regions in seq
#table listing cellids for areas in seq not greater qld
seq_fix <- read.csv(paste0(oupdir, "intermediate/Qld_seq_notgreaterqld.csv")) %>% select(cellid)
k_fix <- k_fix %>% mutate(qld_notseq = case_when(cellid %in% seq_fix$cellid ~ 0,
                                                 TRUE ~ qld_notseq))

##################################
#Calculate habitat ranking and area for greater Qld
k_fix <- k_fix %>% 
  mutate(habitat_rank_qld = case_when(re_suitable_1_ha_qld > 0 & (complexsdm_value > 0.444 | snes_likelyhabitat_ha > 0 | climate_Current_perc95ofrecords > 3) ~ 10,
                                      re_suitable_2_ha_qld > 0 & (complexsdm_value > 0.444 | snes_likelyhabitat_ha > 0 | climate_Current_perc95ofrecords > 3) ~ 9,
                                      re_suitable_3_ha_qld > 0 & (complexsdm_value > 0.444 | snes_likelyhabitat_ha > 0 | climate_Current_perc95ofrecords > 3) ~ 8,
                                      re_suitable_1_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala > 0) ~ 7,
                                      re_suitable_2_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala > 0) ~ 6,
                                      re_suitable_3_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala > 0) ~ 5,
                                      re_suitable_1_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala == 0) ~ 4,
                                      re_suitable_2_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala == 0) ~ 4,
                                      re_suitable_3_ha_qld > 0 & (complexsdm_value > 0.3925 | snes_maybehabitat_ha > 0 | climate_Current_perc99ofrecords > 3) & (historic_koala|current_koala == 0) ~ 4,
                                      TRUE ~ 0))

k_fix <- k_fix %>% mutate(habitat_area_ha_qld = case_when(habitat_rank_qld %in% 8:10 ~ re_suitable_1_ha_qld + re_suitable_2_ha_qld + re_suitable_3_ha_qld,
                                         TRUE ~ 0),
         habitat_area_ha_qld_s2 = case_when(habitat_rank_qld > 3 ~ re_suitable_1_ha_qld + re_suitable_2_ha_qld + re_suitable_3_ha_qld,
                                            TRUE ~ 0))

##################################
#Drop eastern nsw habitat smaller than 30ha
k_fix <- k_fix %>% mutate(habitat_area_ha_nswe = case_when(habitat_area_ha_nsw >= 30 ~ habitat_area_ha_nsw,
                                                           habitat_area_ha_nsw < 30 ~ 0),
                          habitat_area_ha_nswe_123 = case_when(habitat_area_ha_nsw_123 >= 30 ~ habitat_area_ha_nsw_123,
                                                           habitat_area_ha_nsw_123 < 30 ~ 0))

##################################
##add column habitat_present
k_fix <- k_fix %>% mutate(habitat_present = case_when(habitat_area_ha_seq > 0 | habitat_area_ha_nsw >= 30 | habitat_area_ha_qld > 0 | habitat_area_ha_nsw_west_ess > 0 ~ 1,
                                                      TRUE ~ 0),
                          habitat_present_s2 = case_when(habitat_area_ha_seq > 0 | habitat_area_ha_nsw_123 >= 30 | habitat_area_ha_qld_s2 > 0 | habitat_area_ha_nsw_west_maxk > 0 ~ 1,
                                                         TRUE ~ 0))
  
  
##add column habitat_area_total_ha
#where seq area>0 & nsw area>0 add area
k_fix <- k_fix %>% rowwise() %>%
  mutate(habitat_area_total = case_when(habitat_present==1 & nsw_western==0 ~ sum(habitat_area_ha_seq, habitat_area_ha_nsw, habitat_area_ha_qld, na.rm=TRUE),
                                        habitat_present==1 & nsw_western==1 ~ habitat_area_ha_nsw_west_ess),
         habitat_area_total_s2 = case_when(habitat_present_s2==1 & nsw_western==0 ~ sum(habitat_area_ha_seq, habitat_area_ha_nsw_123, habitat_area_ha_qld_s2, na.rm=TRUE),
                                            habitat_present_s2==1 & nsw_western==1 ~ habitat_area_ha_nsw_west_maxk))

k_fix <- k_fix %>% replace_na(list(habitat_area_total=0, habitat_area_total_s2=0))
k_fix <- k_fix %>% mutate(habitat_area_total = case_when(habitat_area_total > 100 ~ 100, 
                                                         TRUE ~ habitat_area_total),
                          habitat_area_total_s2 = case_when(habitat_area_total_s2 > 100 ~ 100, 
                                                         TRUE ~ habitat_area_total_s2))

###########################
#add column ranking environmental suitablility
k_fix <- k_fix %>% mutate(env_suitable = case_when(snes_likelyhabitat_ha > 0 | complexsdm_value > 0.444 | climate_Current_perc95ofrecords > 3 ~ "likely",
                                                   (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) | complexsdm_value > 0.3925 | climate_Current_perc99ofrecords > 3 ~ "possible",
                                                   TRUE ~ "not suitable"))

###########################
#change the order of columns
k_fix <- k_fix %>% relocate(pawc_mean:permanent_water_area_ha, .after=habitat_present_3_ha_qld)
k_fix <- k_fix %>% ungroup()

k_fix <- st_sf(k_fix)

###########################

save(k_fix, file=paste0(oupdir, "clean/koala_gridded_vars_100ha_tidy_v3.Rdata"))
st_write(k_fix, paste0(oupdir, "clean/koala_gridded_vars_100ha_tidy_v3.gpkg"), append=FALSE) 

