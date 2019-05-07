library(prism)
library(raster) 
library(sp) 
library(pbdMPI)

init()
finalpoints_sp <- readRDS(file = "finalpoints_sp.rds")
options(prism.path = "~/lustre/projects/css/csas/albenson/prismtmp/tmaxAug")

# Extacts PRISM data to areas of occupancy described by buffers
RS <- prism_stack(ls_prism_data()) #raster data
proj4string(RS)<-CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")
prism_years <- t(ls_prism_data())

bufferSize_ls <- c(1000, 5000, 10000, 20000)
myextractfun <- function(b){
  result <- raster::extract(RS, finalpoints_sp, buffer = b, fun= mean, na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
}
result <- pbdLapply(bufferSize_ls, myextractfun)

finalize()

saveRDS(result, file = "tmaxAug_species_buffers.rds")