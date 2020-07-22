#This script thresholds spatial predictions of koala climate niche into high, medium and low suitability.
#Climate data obtained from Briscoe 2016.
#Koala occurrence records from Wildnet (Qld) and ALA (NSW)

#################
require(sf)
require(tidyverse)
require(raster)

#Dirs
setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/07_Processing")
occdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Occurrence records"
climdir <- "D:/Box Sync/GPEM_Postdoc/Koala_NESP/04_Datasets/Refugia/Climate_change"

#Load data
#current koala sightings
currkoala <- st_read(occdir, "Koala_Qld_NSW_merge_2000on_1kmres_noDup")
st_crs(currkoala) <- CRS("+init=epsg:4283")

#maxent and nichemapper predictions have different extents, so we load separately
#maxent current rasters
#rastlist <- list.files(paste0(climdir, "/Maxent/Current"), ".tif", recursive = TRUE, full.names = TRUE)
maxestack <- stack(list.files(paste0(climdir, "/Maxent/Current"), ".tif", recursive = TRUE, full.names = TRUE))
crs(maxestack) <- CRS("+init=epsg:4283")

#nichemapper current rasters
#rastlist <- list.files(paste0(climdir, "/NicheMapper/Current"), ".tif", recursive = TRUE, full.names = TRUE)
nichestack <- stack(list.files(paste0(climdir, "/NicheMapper/Current"), ".tif", recursive = TRUE, full.names = TRUE))
crs(nichestack) <- CRS("+init=epsg:4283")

#return a df listing the climate suitability of each occurrence record for each climate projection 
maxedf <- extract(maxestack, currkoala, cellnumbers=TRUE, df=TRUE)
nichedf <- extract(nichestack, currkoala, cellnumbers=TRUE, df=TRUE)

#remove duplicates (where multiple points fall in a cell)
maxenodup <- maxedf[!duplicated(maxedf$cells), ]
nichenodup <- nichedf[!duplicated(nichedf$cells), ]

#merge and reformat
climdf <- bind_rows(list("All records" = maxedf, "All records" = nichedf, "No duplicates" = maxenodup, "No duplicates" = nichenodup), .id="Records")
climdflong <- pivot_longer(climdf, 4:ncol(climdf), names_to="Model", values_to="Suitability") %>% na.omit()

#Plot
ggplot(climdflong, aes(x=Suitability)) +
  geom_histogram(binwidth = 0.01) +
  facet_grid(Records ~ Model, scales="free") +
  theme_light() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5),
        strip.text.x = element_text(size=7))
ggsave("Output/Model_occurrence_histograms.png", width=21, height=9, units=c("cm"))

#Estimate quantiles for the different models
thresholds <- climdflong %>% group_by(Model, Records) %>% summarise(
  min= min(Suitability),
  max= max(Suitability),
  perc99ofrecords = quantile(Suitability, prob=0.01),
  perc95ofrecords = quantile(Suitability, prob=0.05),
  perc90ofrecords = quantile(Suitability, prob=0.1),
  perc80ofrecords = quantile(Suitability, prob=0.2),
  perc50ofrecords = quantile(Suitability, prob=0.5),
  perc10ofrecords = quantile(Suitability, prob=0.9))
  
write.csv(thresholds, "Output/Climate_thresholds.csv", row.names=FALSE)

#Threshold rasters




