library(prism)
library(raster) 
library(sp) 
library(pbdMPI)

init()
finalpoints_sp <- readRDS(file = "finalpoints_sp.rds")
options(prism.path = "/lustre/projects/css/csas/albenson/prismtmp/tminJan")

finalpoints_sp$year <- substr(finalpoints_sp$eventDate, start = 1, stop = 4)
finalpoints_sp$year <- paste0("_", finalpoints_sp$year)

# Filter out the years first needed from the PRISM data
RS_sub <- vector('list', length(finalpoints_sp))
for (i in 1:nrow(finalpoints_sp)){
  RS_sub[[i]] <- (grep(finalpoints_sp$year[i], ls_prism_data()$files, value=TRUE))
}
RS_sub_unq <- unlist(unique(RS_sub))

# Create raster stack for only the years of PRISM data needed
RS <- prism_stack(RS_sub_unq) #raster data
proj4string(RS)<-CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")

# Run 1km, 5km, 10km, and 25km buffers for each point in the species shapefile and extract PRISM data within
# those buffers
bufferSize_ls <- c(1000, 5000, 10000, 20000)
myextractfun <- function(b){
  result <- raster::extract(RS, finalpoints_sp, buffer = b, fun= mean, na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
}

result <- pbdLapply(bufferSize_ls, myextractfun)
finalize()

# Save result as an RDS
saveRDS(result, file = "tminJan_species_buffers.rds")