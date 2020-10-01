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
load(paste0(oupdir, "/intermediate/koala_gridded_vars_", cell_area, "SEQNSW_tidy4.Rdata"))

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
                          

##add column habitat_area_total_ha
#where seq area>0 & nsw area>0 add area
k_fix <- k_fix %>% rowwise() %>%
            mutate(habitat_area_total = case_when(nsw_eastern==1|qld_seq==1 ~ sum(habitat_area_ha_seq, habitat_area_ha_nsw, na.rm=TRUE),
                                                         nsw_western==1 ~ habitat_area_ha_nsw_west_ess),
                                      habitat_area_total_s2 = case_when(nsw_eastern==1|qld_seq==1 ~ sum(habitat_area_ha_seq, habitat_area_ha_nsw_123, na.rm=TRUE),
                                                            nsw_western==1 ~ habitat_area_ha_nsw_west_maxk))
k_fix <- k_fix %>% replace_na(list(habitat_area_total=0, habitat_area_total_s2=0))


#change the order of columns
k_fix <- k_fix %>% relocate(complexsdm_value:complexsdm_interpolatedvalue, .after=snes_likelyhabitat_ha)
k_fix <- k_fix %>% relocate(nsw_eastern, .after=qld_notseq)
k_fix <- k_fix %>% relocate(nsw_western, .after=nsw_eastern)
k_fix <- k_fix %>% relocate(habitat_area_ha_nsw_west_ess:habitat_area_ha_nsw_west_maxk, .after=habitat_area_ha_nsw_123)
k_fix <- k_fix %>% relocate(habitat_area_total:habitat_area_total_s2, .after=nsw_western)


#rename columns
k_fix <- k_fix %>% dplyr::rename(habitat_area_ha_seq = habitat_area_ha_SEQ)


save(k_fix, file=paste0(oupdir, "clean/koala_gridded_vars_100ha_tidy.Rdata"))
st_write(k_fix, paste0(oupdir, "clean/koala_gridded_vars_100ha_tidy.gpkg")) 

