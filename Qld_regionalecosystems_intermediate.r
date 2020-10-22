#################
library(sf)
library(tidyverse)


setwd("D:/Box Sync/GPEM_Postdoc/Koala_NESP/06_Analysis/Sofia/")

re_shp <- st_read("Shapefiles/biome_re_tree_uti_area/biome_re_tree_uti_area.shp") 

kocc_df <- read.csv("Expert elicitation/REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts.csv", header=TRUE) 
kocc_df_hist <- read.csv("Expert elicitation/REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts_v3.csv", header=TRUE) 

#############################
#Fix text in shp

re_shp <- re_shp %>% mutate(Biome = case_when(Biome == "Nandewar" ~ "New England Tablelands",
                                              Biome == "Central Mackay Coast" ~ "Central Queensland Coast",
                                              Biome == "Darling Riverine Plains" ~ "Mulga Lands",
                                              Biome == "Brigalow Belt South" ~ "Brigalow Belt",
                                              Biome == "Brigalow Belt North" ~ "Brigalow Belt",
                                              Biome == "South Eastern Queensland" ~ "South East Queensland",
                                              TRUE ~ Biome),
                            Spcs_tl = case_when(Spcs_tl=="Other" ~ "NA",
                                                TRUE ~ Spcs_tl),
                            RE1 = case_when(RE1 == "on-rem" ~ "non-rem",
                                            RE1 =="stuary" ~ "estuary",
                                            RE1 =="anal" ~ "canal",
                                            RE1 =="cean" ~ "ocean",
                                            RE1 =="cean" ~ "ocean",
                                            RE1 =="snd" ~ "sand",
                                            RE1 =="wter" ~ "water",
                                            RE1 =="sallow" ~ "shallow",
                                            TRUE ~ RE1))
re_shp <- re_shp %>% rename("Tree_utility" = "Spcs_tl", "n_ktreesp" = "Spcs_t_")

re_shp <- re_shp %>% select(!OBJECTI:SQ_KM)

#############################
#Write preliminary koala tree map
re_oup <- left_join(re_shp, kocc_df, by=c("RE1"="re_id"))
re_oup <- re_oup %>% mutate(RE_suitability = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala > 1.0 ~ "Very high", 
                                                       Tree_utility =="Medium" & perc_REpolys_withkoala > 1.0 ~ "High",
                                                       Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala > 0 ~ "Medium",
                                                       Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc) ~ "Low",
                                                       TRUE ~ "None"))
re_oup <- re_oup %>% mutate(RE_suitability_index = as.integer(case_when(RE_suitability =="Very high" ~ 1, 
                                                             RE_suitability =="High" ~ 2,
                                                             RE_suitability =="Medium" ~ 3,
                                                             RE_suitability =="Low" ~ 4,
                                                            TRUE ~ 5)))

re_oup <- re_oup %>% select(!Description)
st_write(re_oup, "Shapefiles/biome_re_utility/Qld_RE_utility.gpkg", append=FALSE)
st_write(re_oup, "Shapefiles/biome_re_utility/Qld_RE_utility.shp")

##Make additional file mapping habitat considering historical as well as recent records
re_hist <- left_join(re_shp, kocc_df_hist, by=c("RE1"="re_id"))
re_hist <- re_hist %>% mutate(RE_suitability_hist = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala_inclhist > 1.0 ~ "Very high", 
                                                       Tree_utility =="Medium" & perc_REpolys_withkoala_inclhist > 1.0 ~ "High",
                                                       Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala_inclhist > 0 ~ "Medium",
                                                       Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc_inclhist) ~ "Low",
                                                       TRUE ~ "None"))
re_hist <- re_hist %>% mutate(RE_suitability_index_hist = as.integer(case_when(RE_suitability_hist =="Very high" ~ 1, 
                                                                        RE_suitability_hist =="High" ~ 2,
                                                                        RE_suitability_hist =="Medium" ~ 3,
                                                                        RE_suitability_hist =="Low" ~ 4,
                                                                        TRUE ~ 5)))

st_write(re_hist, "Shapefiles/biome_re_utility/Qld_RE_utility_historical.gpkg", append=FALSE)  
st_write(re_hist, "Shapefiles/biome_re_utility/Qld_RE_utility_historical.shp")

##############################
#Write merged .csv for experts to evaluate
re_df <- re_shp %>% st_set_geometry(NULL) 
re_df <- re_df %>% group_by(RE1) %>% summarise(Biome = first(Biome), 
                                            Tree_utility = first(Tree_utility),
                                            n_ktreesp = first(n_ktreesp))

re_merge <- full_join(kocc_df, re_df, by=c("re_id" = "RE1"))
re_merge <- re_merge %>% mutate(RE_suitability = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala > 1.0 ~ "Very high", 
                                                       Tree_utility =="Medium" & perc_REpolys_withkoala > 1.0 ~ "High",
                                                       Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala > 0 ~ "Medium",
                                                       Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc) ~ "Low",
                                                       TRUE ~ "None"))


write.csv(re_merge, "REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts_c2.csv", row.names=FALSE)

##############################

re_merge_hist <- full_join(kocc_df_hist, re_df, by=c("re_id" = "RE1"))
re_merge_hist <- re_merge_hist %>% mutate(RE_suitability_recent = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala_recent > 1.0 ~ "Very high", 
                                                                Tree_utility =="Medium" & perc_REpolys_withkoala_recent > 1.0 ~ "High",
                                                                Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala_recent > 0 ~ "Medium",
                                                                Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc_recent) ~ "Low",
                                                                TRUE ~ "None"),
                                      RE_suitability_hist = case_when(Tree_utility=='Higher' & perc_REpolys_withkoala_inclhist > 1.0 ~ "Very high", 
                                                           Tree_utility =="Medium" & perc_REpolys_withkoala_inclhist > 1.0 ~ "High",
                                                           Tree_utility %in% c('Higher', "Medium") & perc_REpolys_withkoala_inclhist > 0 ~ "Medium",
                                                           Tree_utility %in% c('Higher', "Medium") & is.na(n_koalaocc_inclhist) ~ "Low",
                                                           TRUE ~ "None"))


write.csv(re_merge_hist, "REDD_QldnoSEQ_summary_of_koala_occurrence_forexperts_historical.csv", row.names=FALSE)









