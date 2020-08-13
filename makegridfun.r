######################
##Make grid templates
######################
#function to make hexagonal grids
makegridfun <- function(bb, currdiameter){
  
  bb_buff <- st_buffer(bb, currdiameter)
  
  currgrid <- st_make_grid(bb, cellsize=cell_diameter, square=FALSE, flat_topped = TRUE)
  
  return(currgrid)
}

