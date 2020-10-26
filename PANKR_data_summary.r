#################
library(sf)
library(tidyverse)
library(tmap)
library(ggpubr)

#Dirs & file locations
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing/")
#setwd("M:/Users/uqcrung1/Documents/Koala_pankr/")
datadir <- "Data_inp/"
oupdir <- "Output/Gridded_data/clean/"

cell_area = "100ha" 

load(paste0(oupdir, "koala_gridded_vars_", cell_area, "_tidy.Rdata"))

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
#number of cells with majority climate models (90% threshold)
k_dat %>% filter(climate_2070_perc90ofrecords>6 & !is.na(climate_2070_perc90ofrecords)) %>% nrow() 
k_dat %>% filter(climate_Current_perc90ofrecords>3 & !is.na(climate_Current_perc90ofrecords)) %>% nrow() 
k_dat %>% filter(climate_2070_perc90ofrecords>6 & climate_Current_perc90ofrecords>3) %>% nrow() 


#number of cells with habitat 
k_dat %>% filter(snes_likelyhabitat_ha>0) %>% nrow()
k_dat %>% filter(snes_maybehabitat_ha>0) %>% nrow()
k_dat %>% filter(intact_area_ha>0) %>% nrow()
k_dat %>% filter(recoverable_area_ha>0) %>% nrow()
k_dat %>% filter(unrecoverable_area_ha>0) %>% nrow()

#number of cells with koala
k_dat %>% filter(current_koala>0) %>% nrow()
k_dat %>% filter(historic_koala>0) %>% nrow()

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
k_dat %>% filter(firefreq_88to15<3 | is.na(firefreq_88to15)) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(firefreq_88to15==1 | is.na(firefreq_88to15)) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(is.na(firefreq_88to15)) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(permanent_water_area_ha==0) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(permanent_water_area_ha>0) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.8) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.9) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.958) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(soildepth_mean>0.8) %>% summarise(100*sum(historic_koala)/n_koala_historic)
k_dat %>% filter(pawc_mean>30) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(pawc_mean>40) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(pawc_mean>50) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(pawc_mean>100) %>% summarise(100*sum(current_koala)/n_koala)
k_dat %>% filter(pawc_mean>77) %>% summarise(100*sum(current_koala)/n_koala)

#area under cutoff points
k_dat %>% filter(firefreq_88to15<3 | is.na(firefreq_88to15)) %>% nrow()
k_dat %>% filter(firefreq_88to15==1 | is.na(firefreq_88to15) ) %>% nrow()
k_dat %>% filter(is.na(firefreq_88to15) ) %>% nrow()
k_dat %>% filter(permanent_water_area_ha>0 ) %>% nrow()
k_dat %>% filter(soildepth_mean>0.8 ) %>% nrow()
k_dat %>% filter(soildepth_mean>0.9 ) %>% nrow()
k_dat %>% filter(soildepth_mean>1.0 ) %>% nrow()
k_dat %>% filter(soildepth_mean>0.958 ) %>% nrow()
k_dat %>% filter(pawc_mean>30 ) %>% nrow()
k_dat %>% filter(pawc_mean>40 ) %>% nrow()
k_dat %>% filter(pawc_mean>50 ) %>% nrow()
k_dat %>% filter(pawc_mean>100 ) %>% nrow()
k_dat %>% filter(pawc_mean>77 ) %>% nrow()


################################
##Plot the variables
region <- st_read(paste0(datadir, "IBRA7_regions_states_koala_dissolve.shp"))
oupdir <- "Output/figures/variables/"

plotfun <- function(data, colid, plottitle, ...) {
    p <- tm_shape(data) +
      tm_fill(col=colid, title=plottitle, legend.position=c("top", "right"), colorNA="grey90", ...) +
      tm_shape(region) + tm_borders() +
      tm_layout(frame=FALSE)
  tmap_save(p, paste0(oupdir, colid, ".png"), height=1920, width=1080)
}
greypal <- c("grey90", RColorBrewer::brewer.pal(5, "YlGnBu")[2:5])

plotfun(kfix, colid="current_koala", plottitle="Recent koala", breaks=c(0, 1, 10, 500, 3100), palette=greypal)
plotfun(kfix, colid="historic_koala", plottitle="Historic koala", breaks=c(0, 1, 10, 200, 1400), palette=greypal)
plotfun(kfix, colid="pawc_mean", plottitle="Mean PAWC (mm)", style='quantile', palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="soildepth_mean", plottitle="Soil depth (m)", n=5, style='quantile', palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="firefreq_88to15", plottitle="Fire frequency", breaks=c(1, 2, 3, 6, 18), labels = c("1", "2", "3 to 5", "> 5"), palette='YlGnBu', colorNA="grey90", textNA="No fires")
plotfun(kfix, colid="permanent_water_area_ha", plottitle="Permanent water (ha)", breaks=c(0, 1, 10, 50, 100),  labels = c("0", "1 to 10", "11 to 50", "> 50"), palette='YlGnBu')
plotfun(kfix, colid="recoverable_area_ha", plottitle="Restorable area (ha)", breaks=c(0, 1, 25, 50, 100), labels = c("0", "1 to 25", "26 to 50", ">50"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="unrecoverable_area_ha", plottitle="Unrestorable area (ha)", breaks=c(0, 1, 25, 50, 100), labels = c("0", "1 to 25", "26 to 50", ">50"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="intact_area_ha", plottitle="Intact area (ha)", breaks=c(0, 1, 25, 50, 100), labels = c("0", "1 to 25", "26 to 50", ">50"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="snes_maybehabitat_ha", plottitle="SNES (may occur, ha)", style='cat', palette='YlGnBu')
plotfun(kfix, colid="snes_likelyhabitat_ha", plottitle="SNES (likely to occur, ha)", style='cat', palette='YlGnBu')
plotfun(kfix, colid="climate_2070_perc99ofrecords", plottitle="Climate refugia 2070 (99)", breaks=c(0, 1, 6, 10, 12, 12), labels = c("0", "1 to 5", "6 to 9", "10 to 11", "12"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="climate_2070_perc95ofrecords", plottitle="Climate refugia 2070 (95)", breaks=c(0, 1, 6, 10, 12, 12), labels = c("0", "1 to 5", "6 to 9", "10 to 11", "12"),palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="climate_2070_perc90ofrecords", plottitle="Climate refugia 2070 (90)", breaks=c(0, 1, 6, 10, 12, 12), labels = c("0", "1 to 5", "6 to 9", "10 to 11", "12"),palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="climate_Current_perc99ofrecords", plottitle="Current climate refugia (99)", breaks=c(0, 1, 4, 6, 6), labels = c("0", "1 to 3", "4 to 5", "6"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="climate_Current_perc95ofrecords", plottitle="Current climate refugia (95)", breaks=c(0, 1, 4, 6, 6), labels = c("0", "1 to 3", "4 to 5", "6"), palette='YlGnBu', showNA=FALSE)
plotfun(kfix, colid="climate_Current_perc90ofrecords", plottitle="Current climate refugia (90)", breaks=c(0, 1, 4, 6, 6), labels = c("0", "1 to 3", "4 to 5", "6"), palette='YlGnBu', showNA=FALSE)
plotfun(k_fix, colid="habitat_area_ha_SEQ", plottitle="Habitat area SEQ (ha)", breaks=c(0, 0.1, 10, 50, 102), labels = c("0", "< 10", "10 to 50", "50 to 100"), palette=greypal)
plotfun(k_fix, colid="habitat_area_ha_nsw", plottitle="Habitat (ha)", breaks=c(0, 5, 30, 60, 100), palette='YlGnBu', showNA=FALSE)
plotfun(k_fix, colid="nswcomplexsdm_interpolatedvalue", plottitle="Habitat suitability (complex sdm)", breaks=c(0, 0.3925, 0.444, 1), palette='YlGnBu', showNA=FALSE)
plotfun(k_fix, colid="nswcomplexsdm_value", plottitle="Habitat suitability (complex sdm)", breaks=c(0, 0.3925, 0.444, 1), palette='YlGnBu', showNA=FALSE)
plotfun(k_fix, colid="habitat_area_total", plottitle="Habitat (ha)", breaks=c(0, 50, 100), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)
plotfun(k_fix, colid="habitat_area_total_s2", plottitle="Habitat (ha)", breaks=c(0, 50, 100), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)

###############
#Comparing the habitat models
k_fix <- k_fix %>% mutate(complex_diff = case_when(complexsdm_value < 0.444 & complexsdm_interpolatedvalue >= 0.444 ~ 1,
                                                   complexsdm_value >= 0.444 & complexsdm_interpolatedvalue < 0.444 ~ 2,
                                                   complexsdm_value >= 0.444 & complexsdm_interpolatedvalue >= 0.444 ~ 3))
table(k_fix$complex_diff)
diffpal <- c("red", "yellow", RColorBrewer::brewer.pal(5, "YlGnBu")[3])
plotfun(k_fix, colid="complex_diff", plottitle="Difference between complex sdm summaries", breaks=c(0,1.1,2.1,3.1), labels=c("interpolated not centroid", "centroid not interpolated", "both"), palette=diffpal, showNA=FALSE)

k_fix <- k_fix %>% mutate(snes_complex_diff = case_when((snes_likelyhabitat_ha == 0 & snes_maybehabitat_ha == 0) & complexsdm_value >= 0.444 ~ 1,
                                                        (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & complexsdm_value < 0.444 ~ 2,
                                                        (snes_likelyhabitat_ha > 0 | snes_maybehabitat_ha > 0) & complexsdm_value >= 0.444 ~ 3))
table(k_fix$snes_complex_diff)
k_fix <- k_fix %>% mutate(snes_complex_diff2 = case_when(snes_likelyhabitat_ha == 0 & complexsdm_value >= 0.444 ~ 1,
                                                        snes_likelyhabitat_ha > 0 & complexsdm_value < 0.444 ~ 2,
                                                        snes_likelyhabitat_ha > 0 & complexsdm_value >= 0.444 ~ 3))
table(k_fix$snes_complex_diff2)
k_fix <- k_fix %>% mutate(snes_complex_diff3 = case_when(snes_likelyhabitat_ha == 0 & complexsdm_value >= 0.3925 ~ 1,
                                                        snes_likelyhabitat_ha > 0 & complexsdm_value < 0.3925 ~ 2,
                                                        snes_likelyhabitat_ha > 0 & complexsdm_value >= 0.3925 ~ 3))
table(k_fix$snes_complex_diff3)
plotfun(k_fix, colid="snes_complex_diff", plottitle="Difference", breaks=c(0,1.1,2.1,3.1), labels=c("complex not snes", "snes not complex", "both"), palette=diffpal, showNA=FALSE)
plotfun(k_fix, colid="snes_complex_diff3", plottitle="Difference", breaks=c(0,1.1,2.1,3.1), labels=c("complex not snes", "snes not complex", "both"), palette=diffpal, showNA=FALSE)
plotfun(k_fix, colid="snes_complex_diff2", plottitle="Difference", breaks=c(0,1.1,2.1,3.1), labels=c("complex not snes", "snes not complex", "both"), palette=diffpal, showNA=FALSE)

#################################
#Map habitat by region
nsw_eastern <- st_read(paste0(datadir, "Habitat_maps/NSW/KMRs_eastern.shp"))
nsw_western <- st_read(paste0(datadir, "Habitat_maps/NSW/KMRs_western.shp"))
seq_region <- st_read(paste0(datadir, "Habitat_maps/SEQ/SEQRP_study_area.shp"))
glob <- st_read("D:/Box Sync/Data_software/Datasets/General maps and shapefiles/GlobalBorders/Global_borders/Borders_naturalearth/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
act_region <- glob %>% filter(admin=="Australia" & name=="Australian Capital Territory")
oupdir <- "Output/figures/variables/"

plotfun <- function(data, colid, plottitle, oupname, region, ...) {
  currbox <- st_bbox(region, crs=st_crs(region))
  p <- tm_shape(data, bbox=currbox) +
    tm_fill(col=colid, title=plottitle, colorNA="grey90", ...) +
   # tm_shape(nsw_eastern) +
   # tm_fill(palette="grey90") +
    tm_shape(region) + tm_borders(lwd=2) + 
    #tm_layout(legend.position=c("top", "left"), frame=FALSE) 
    tm_layout(legend.outside=TRUE, legend.outside.position="right", frame=FALSE)
  tmap_save(p, paste0(oupdir, colid, oupname,".png"), width=1080)
}
greypal <- c("grey90", RColorBrewer::brewer.pal(4, "YlGnBu"))

#Plot area of habitat by region
plotfun(k_fix, colid="habitat_area_total", plottitle="Habitat (ha)", oupname="_SEQ", region=seq_region,
        breaks=c(0, 0.1, 10, 50, 102), labels = c("0", "< 10", "10 to 50", "50 to 100"), palette=greypal, showNA=FALSE)

plotfun(k_fix, colid="habitat_area_ha_nsw", plottitle="Habitat (ha)", oupname="_ACT_likely", region=act_region, 
        breaks=c(0, 5, 30, 50, 100), labels = c("< 5", "5 to 30", "30 to 50", "50 to 100"), palette='YlGnBu', showNA=FALSE)
plotfun(k_fix, colid="habitat_area_ha_nsw_123", plottitle="Habitat (ha)", oupname="_ACT_possible", region=act_region, 
        breaks=c(0, 5, 30, 50, 100), labels = c("< 5", "5 to 30", "30 to 50", "50 to 100"), palette=greypal, showNA=FALSE)

plotfun(k_fix, colid="habitat_area_ha_nsw", plottitle="Habitat (ha)", oupname="_NSWE_likely",  region=nsw_eastern, 
        breaks=c(0, 5, 30, 50, 100), labels = c("< 5", "5 to 30", "30 to 50", "50 to 100"), palette=greypal, showNA=FALSE)
plotfun(k_fix, colid="habitat_area_ha_nsw_123", plottitle="Habitat (ha)", oupname="_NSWE_possible", region=nsw_eastern, 
        breaks=c(0, 5, 30, 50, 100), labels = c("< 5", "5 to 30", "30 to 50", "50 to 100"), palette=greypal, showNA=FALSE)

#Plot presence of habitat by region
plotfun(k_fix, colid="habitat_present", plottitle="Habitat", oupname="_NSWW_likely", region=nsw_western, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)
plotfun(k_fix, colid="habitat_present_s2", plottitle="Habitat", oupname="_NSWW_posible", region=nsw_western, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)

plotfun(k_fix, colid="habitat_present", plottitle="Habitat", oupname="_SEQ", region=seq_region, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)

plotfun(k_fix, colid="habitat_present", plottitle="Habitat", oupname="_ACT_likely", region=act_region, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)
plotfun(k_fix, colid="habitat_present_s2", plottitle="Habitat", oupname="_ACT_possible", region=act_region, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)

plotfun(k_fix, colid="habitat_present", plottitle="Habitat", oupname="_NSWE_likely", region=nsw_eastern, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)
plotfun(k_fix, colid="habitat_present_s2", plottitle="Habitat", oupname="_NSWE_posible", region=nsw_eastern, 
        breaks=c(0, 0.1, 1), labels = c("unsuitable", "suitable"), palette=greypal, showNA=FALSE)


##################################
#Plot comparing the climate cutoffs
k_long <- k_dat %>% dplyr::select(climate_2070_perc99ofrecords:climate_Current_perc90ofrecords) %>% 
  pivot_longer(cols=climate_2070_perc99ofrecords:climate_Current_perc90ofrecords, names_to = "clim_scenario") %>%
  filter(!is.na(value))
k_plot <- k_long %>% group_by(clim_scenario, value) %>% tally(name='ncells') %>%
  group_by(clim_scenario) %>% 
  summarise(n_allmods = sum(ncells[value==max(value)]),
            n_nminus1 = sum(ncells[value>=max(value)-1]),
            n_nminus2 = sum(ncells[value>=max(value)-2]),
            n_majority = sum(ncells[value>6]),
            n_majority2 = sum(ncells[value>3])) %>%
  mutate(threshold = rep(c(90, 95, 99), times=2),
         time_period = rep(c("2070", "Current"), each=3)) %>%
  mutate(n_majority_fix = case_when(time_period=="Current" ~ n_majority2,
                                    TRUE ~ n_majority),
         n_minus = case_when(time_period=="Current" ~ n_nminus1,
                              TRUE ~ n_nminus2)) %>%
  dplyr::select(clim_scenario, time_period, threshold, n_allmods, n_minus, n_majority_fix) %>% 
  pivot_longer(cols=n_allmods:n_majority_fix) %>%
  mutate(area = value/10000)


p <- ggplot(k_plot, aes(x=threshold, y=area, col=time_period, shape=name)) +
  geom_point(size=3) +
  geom_line() +
  xlab("Threshold (% of records)") + ylab("Climatically suitable (million ha)") +
  scale_x_reverse(breaks=c(100, 99, 95, 90, 85)) +
  theme_classic() + #theme(text = element_text(size = 18)) +
  scale_shape_discrete(name  ="Criteria",
                       breaks=c("n_allmods", "n_minus", "n_majority_fix"),
                       labels=c("All models", "All minus one model", "Majority of models")) +
  scale_colour_discrete(name  ="Time period")
ggsave(paste0(oupdir, "Climate_thresholds_area_comparison2.png"), p, scale=0.6, width=10, height=7)


###END