
fastbindfun <- function(tempdir, pattern, grid){
	fileNames = list.files(tempdir, pattern=pattern, full.names=TRUE)
	dflist <- lapply(fileNames, data.table::fread)
	df <- data.table::rbindlist(dflist, use.names=TRUE)
	df <- df %>% dplyr::select(!splits)
	grid_oup <- grid %>% left_join(df, by='cellid')
	return(grid_oup)
	}