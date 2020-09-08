#################
library(sf)
library(tidyverse)
library(MASS)
library(tmap)
library(ggpubr)

#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/"

cell_area = "100ha" 

load(paste0(oupdir, "koala_gridded_vars_", cell_area, "SEQ_tidy.Rdata"))

################
#Summarise the variables
s <- kfix %>% st_set_geometry(NULL) %>%
  summarise(across(.cols=everything(), list(mean = ~ mean(.x, na.rm = TRUE), 
                                            min = ~ min(.x, na.rm = TRUE), 
                                            max = ~ max(.x, na.rm = TRUE), 
                                            numNAs = ~ sum(is.na(.x)) ) ) ) %>% as_tibble()
stat <-  str_extract(names(s), "(?<=_)[^_]*$")
colid <- names(s)
s1 <- s %>% pivot_longer(cols = everything()) %>% 
  mutate(stat=str_extract(name, "(?<=_)[^_]*$")) %>%
  mutate(colid=gsub("[^_]*$", "", name)) %>% dplyr::select(!name)
s2 <- s1 %>% pivot_wider(names_from = stat, values_from = value)

write.csv(s2, paste0(oupdir, "Summary_koala_gridded_vars.csv"))

#######################################
#Lets look at cutpoints and how much landscape is covered by each variable

k_dat <- kfix %>% st_set_geometry(NULL)
k_dat <- k_dat %>% mutate(all_koala = current_koala + historic_koala,
                          log_all_koala = log(all_koala+1),
                          all_koala_binomial = case_when(all_koala>0 ~ 1,TRUE ~ 0))


#number of cells with 12 future climate models (90% threshold)
k_dat %>% filter(climate_2070_perc90ofrecords==12 & !is.na(climate_2070_perc90ofrecords)) %>% nrow() 
k_dat %>% filter(climate_Current_perc90ofrecords==6 & !is.na(climate_Current_perc90ofrecords)) %>% nrow() 
k_dat %>% filter(climate_2070_perc90ofrecords==12 & climate_Current_perc90ofrecords==6) %>% nrow() 

#number of cells with habitat 
k_dat %>% filter(snes_likelyhabitat_ha>0) %>% nrow()
k_dat %>% filter(snes_maybehabitat_ha>0) %>% nrow()
k_dat %>% filter(intact_area_ha>0) %>% nrow()
k_dat %>% filter(recoverable_area_ha>0) %>% nrow()
k_dat %>% filter(unrecoverable_area_ha>0) %>% nrow()

#plot vars
p4 <- ggplot(k_dat) +
  geom_bin2d(aes(x=firefreq_88to15, y=log_all_koala))
p1 <- ggplot(k_dat) +
  geom_bin2d(aes(x=pawc_mean, y=log_all_koala))
p3 <- ggplot(k_dat) +
  geom_bin2d(aes(x=soildepth_mean, y=log_all_koala))
p5 <- ggplot(k_dat) +
  geom_bin2d(aes(x=permanent_water_area_ha, y=log_all_koala))

g <- ggarrange(p4, p1, p3, p5, ncol=2, nrow=2)
ggsave("PANKR_variables_vs_nkoala.png", g)

#chose cutoff points
n_koala <- k_dat %>% summarise(sum(current_koala))
n_koala_historic <- k_dat %>% summarise(sum(historic_koala))
k_dat %>% filter(firefreq_88to15<3) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(firefreq_88to15>2) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(permanent_water_area_ha==0) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(permanent_water_area_ha>0) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.8) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.8) %>% summarise(100*sum(historic_koala)/n_koala_historic)
k_dat %>% filter(soildepth_mean>0.8) %>% summarise(100*sum(historic_koala)/n_koala_historic)


#Model of koala refugia


#need a binomial model with probability of current koala vs probability of not koala?
library(MASS)
m1 <- with(k_dat, glm.nb(all_koala ~ pawc_mean + soildepth_mean + permanent_water_area_ha, link=log))
#m3 <- with(k_dat, glm.nb(all_koala ~ pawc_mean + permanent_water_area_ha, link=log)) #does not converge

m2 <- with(k_dat, glm(all_koala ~ pawc_mean + soildepth_mean + permanent_water_area_ha, family='poisson'))

k_dat <- k_dat %>% 
  
  