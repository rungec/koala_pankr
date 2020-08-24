######################
##Make grid templates
######################
#function to make hexagonal grids
makegridfun <- function(bb, currdiameter){
  
  bb_buff <- st_buffer(bb, currdiameter)
  
  currgrid <- st_make_grid(bb, cellsize=currdiameter, square=FALSE, flat_topped = TRUE) %>% st_as_sf()
  currgrid$cellid <- 1:nrow(currgrid)
 
  return(currgrid)
}

