#################
library(sf)
library(tidyverse)


setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/06_Analysis/Sofia/Sofia_processed")

re_shp <- st_read("Maps_graphs/Shapefiles/biome_re_tree_uti_area/biome_re_tree_uti_area.shp") 
re_shp <- mutate(re_shp, RE1_dup = as.character(RE1))

kocc_df <- read.csv("REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts.csv", header=TRUE) 
kocc_df <- mutate(kocc_df, re_id_dup = as.character(re_id))

#############################
#Write preliminary koala tree map
re_oup <- left_join(re_shp, kocc_df, by=c("RE1"="re_id"))
re_oup <- re_oup %>% rename("Tree_utility" = "Spcs_tl")
re_oup <- re_oup %>% rename("n_ktreesp" = "Spcs_t_")
re_oup <- re_oup %>% mutate(RE_suitability = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala > 1.0 ~ "Very high", 
                                                       Tree_utility =="Medium" & perc_REpolys_withkoala > 1.0 ~ "High",
                                                       Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala > 0 ~ "Medium",
                                                       Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc) ~ "Low",
                                                       TRUE ~ "None"))

re_oup <- re_oup %>% select(!REG_NAM)
re_oup <- re_oup %>% select(!Description)
#st_write(re_oup, "Shapefiles/biome_re_utility/Qld_RE_utility.gpkg", append=FALSE)
st_write(re_oup, "Shapefiles/biome_re_utility/Qld_RE_utility.shp")

##############################
#Write merged .csv for experts to evaluate
re_df <- re_shp %>% st_set_geometry(NULL) 
re_df <- re_df %>% group_by(RE1) %>% summarise(Biome = first(Biome), 
                                            Tree_utility = first(Spcs_tl),
                                            Num_utilised_trees = first(Spcs_t_))

re_merge <- full_join(kocc_df, re_df, by=c("re_id" = "RE1"))
re_merge <- re_merge %>% mutate(RE_suitability = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala > 1.0 ~ "Very high", 
                                                       Tree_utility =="Medium" & perc_REpolys_withkoala > 1.0 ~ "High",
                                                       Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala > 0 ~ "Medium",
                                                       Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc) ~ "Low",
                                                       TRUE ~ "None"))


write.csv(re_merge, "REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts_c2.csv", row.names=FALSE)









