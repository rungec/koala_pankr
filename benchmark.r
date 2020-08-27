# Make test data
# RasterStack
r <- raster(ncol=36, nrow=18)
r[] <- 1:ncell(r)
s <- stack(r, sqrt(r), r/r)

f0 <- function(pawc_kc, kc) {raster::extract(pawc_kc, kc, fun=mean, na.rm=TRUE)}
f1 <- function(pawc_kc, kc) {raster::extract(pawc_kc, kc, fun=max, na.rm=TRUE)}

# SpatialPolygons
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- spPolygons(cds1, cds2)

microbenchmark(f0(r, polys), f1(r, polys), times=100)
