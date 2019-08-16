library(prism)
library(raster) 
library(sp) 
library(pbdMPI)

init()
finalpoints_sp <- readRDS(file = "finalpoints_sp.rds")
options(prism.path = "/lustre/projects/css/csas/albenson/prismtmp/tminJan")

# Create raster stack for only the years of PRISM data needed
RS <- prism_stack(ls_prism_data()) #raster data
proj4string(RS)<-CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")

# Run 1km, 5km, 10km, and 25km buffers for each point in the species shapefile and extract PRISM data within
# those buffers
bufferSize_ls <- c(1000, 5000, 10000, 20000)
myextractfun <- function(b){
  result <- raster::extract(RS, finalpoints_sp, buffer = b, fun= mean, na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE, sp = TRUE)
}

result <- pbdLapply(bufferSize_ls, myextractfun)
finalize()

# Save result as an RDS
saveRDS(result, file = "tminJan_buffers.rds")