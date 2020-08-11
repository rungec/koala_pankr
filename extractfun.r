#This script contains spatial functions to extract the landscape values which will be used for clustering

######################
##Set up overlap functions
######################

# create a function to measure proportion of elements of sf1 are covered by sf2
prop_overlap <- function(sf1,sf2){
  area <- as.numeric(st_area(sf1))
  dist <- map(st_geometry(sf1),
              function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                st_intersection(sf2) %>% 
                st_area() %>% 
                as.numeric()) %>% 
    lapply(function(x) ifelse(is.null(x), NA, x)) %>% 
    lapply(function(x) ifelse(is.na(x), 0, x)) %>%
    unlist()
  return(dist/area)
}

# create a function to measure which elements of sf1 are most covered by elements of sf2
most_overlap <- function(sf1,sf2,column,missing){
  most <- suppressWarnings(map(st_geometry(sf1),
                               function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                                 st_sf() %>% 
                                 st_intersection(.,sf2) %>% 
                                 mutate(area=st_area(.)) %>%
                                 filter(area==max(area)) %>% 
                                 data.frame() %>% 
                                 select(column))) %>% 
    lapply(function(x) ifelse(nrow(x)==0, missing, x)) %>% 
    unlist()
  return(most)
}

# create a function to measure which elements of sf1 are most covered by elements of sf2
most_overlap <- function(sf1,sf2,column,missing){
  most <- suppressWarnings(map(st_geometry(sf1),
                               function(x) st_sfc(x,crs=st_crs(sf2)) %>% 
                                 st_sf() %>% 
                                 st_intersection(.,sf2) %>% 
                                 mutate(area=st_area(.)) %>%
                                 filter(area==max(area)) %>% 
                                 data.frame() %>% 
                                 select(column))) %>% 
    lapply(function(x) ifelse(nrow(x)==0, missing, x)) %>% 
    unlist()
  return(most)
}

######################
#Split hexagons by the region and extract the habitat values using the models for each region
######################
extracthabitat <- function(k_grid, currarea, currhab){

#subset hexagons by current region 
#need to find a way to deal with the hexagons that overlap the borders

#load and extract habitat model

}

##END