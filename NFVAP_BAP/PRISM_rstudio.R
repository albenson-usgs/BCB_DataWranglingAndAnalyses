# Abby's version of the National Fishes Vulnerability Assessment "PRISM data extraction from buffers_11_13_2018.R" and
# "Post extraction PRISM processing_pointbuffers_11_13_2018.R"
# Abby Benson
# 2019-08-26

library(prism)
library(tidyverse)

options(prism.path = "~/prismtmp")
get_prism_annual(type = "tmax", year = 1895:2014, keepZip = FALSE)
get_prism_annual(type = "tmin", year = 1895:2014, keepZip = FALSE)
# get_prism_annual(type = "tmean", year = 1895:2014, keepZip = FALSE) # Decided 2019-05-14 that it didn't make sense to
# include the annual mean as it wouldn't tell us anything that the min and max aren't already saying.
get_prism_annual(type = "ppt", year = 1895:2014, keepZip = FALSE)
get_prism_monthlys(type = "tmax", year = 1895:2014, mon = 8, keepZip = FALSE)
get_prism_monthlys(type = "tmin", year = 1895:2014, mon = 1, keepZip = FALSE)

# Extract PRISM values for each species observation point. Buffers are added during the extract function. This code
# needs to be run on a high performance computing cluster like USGS Yeti

###### Run PRISM_variable_yeti.R on a HPC - each variable has its own R script #####
# Couldn't figure out how to make a for loop to make this work so repetition it is!
# Import RDS file from the Yeti HPC procedure
# RDSfiles <- list.files(path = ".", pattern = ".rds", full.names = F)
# RDSfiles <- RDSfiles[2:6] #remove the spatial points rds file used as an input to HPC work and isn't an output
# 
# 
# for (f in length(RDSfiles)){
#   var_buffer <- readRDS(RDSfiles[f])
#   for (v in length(var_buffer)){
#     var_1km <- as.data.frame(var_buffer[1])
#     var_5km <- as.data.frame(var_buffer[2])
#     var_10km <- as.data.frame(var_buffer[3])
#     var_20km <- as.data.frame(var_buffer[4])
#   }
# }
tmax_Buffer <- readRDS("tmax_buffers.rds")
tmax_1km <- as.data.frame(tmax_Buffer[[1]])
tmax_1km$year <- substr(tmax_1km$eventDate, start = 1, stop = 4)
tmax_1km$year <- paste0("_", tmax_1km$year)
tmax_1km_final <- tmax_1km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tmax_10km <- as.data.frame(tmax_Buffer[[3]])
tmax_10km$year <- substr(tmax_10km$eventDate, start = 1, stop = 4)
tmax_10km$year <- paste0("_", tmax_10km$year)
tmax_10km_final <- tmax_10km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tmaxAug_Buffer <- readRDS("tmaxAug_buffers.rds")
tmaxAug_1km <- as.data.frame(tmaxAug_Buffer[[1]])
tmaxAug_1km$year <- substr(tmaxAug_1km$eventDate, start = 1, stop = 4)
tmaxAug_1km$year <- paste0("_", tmaxAug_1km$year)
tmaxAug_1km_final <- tmaxAug_1km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tmaxAug_10km <- as.data.frame(tmaxAug_Buffer[[3]])
tmaxAug_10km$year <- substr(tmaxAug_10km$eventDate, start = 1, stop = 4)
tmaxAug_10km$year <- paste0("_", tmaxAug_10km$year)
tmaxAug_10km_final <- tmaxAug_10km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

ppt_Buffer <- readRDS("ppt_buffers.rds")
ppt_1km <- as.data.frame(ppt_Buffer[[1]])
ppt_1km$year <- substr(ppt_1km$eventDate, start = 1, stop = 4)
ppt_1km$year <- paste0("_", ppt_1km$year)
ppt_1km_final <- ppt_1km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

ppt_10km <- as.data.frame(ppt_Buffer[[3]])
ppt_10km$year <- substr(ppt_10km$eventDate, start = 1, stop = 4)
ppt_10km$year <- paste0("_", ppt_10km$year)
ppt_10km_final <- ppt_10km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tmin_Buffer <- readRDS("tmin_buffers.rds")
tmin_1km <- as.data.frame(tmin_Buffer[[1]])
tmin_1km$year <- substr(tmin_1km$eventDate, start = 1, stop = 4)
tmin_1km$year <- paste0("_", tmin_1km$year)
tmin_1km_final <- tmin_1km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tmin_10km <- as.data.frame(tmin_Buffer[[3]])
tmin_10km$year <- substr(tmin_10km$eventDate, start = 1, stop = 4)
tmin_10km$year <- paste0("_", tmin_10km$year)
tmin_10km_final <- tmin_10km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tminJan_Buffer <- readRDS("tminJan_buffers.rds")
tminJan_1km <- as.data.frame(tminJan_Buffer[[1]])
tminJan_1km$year <- substr(tminJan_1km$eventDate, start = 1, stop = 4)
tminJan_1km$year <- paste0("_", tminJan_1km$year)
tminJan_1km_final <- tminJan_1km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

tminJan_10km <- as.data.frame(tminJan_Buffer[[3]])
tminJan_10km$year <- substr(tminJan_10km$eventDate, start = 1, stop = 4)
tminJan_10km$year <- paste0("_", tminJan_10km$year)
tminJan_10km_final <- tminJan_10km %>%
  gather(key = "prism_name", value = "value", 15:134) %>%
  rowwise() %>%
  filter(grepl(pattern = year, x = prism_name))

#### Now we need to calculate standard deviation
st_dev <- as.data.frame(tmin_10km_final[1,]$scientificName)
st_dev$tmax_1km <- sd(tmax_1km_final$value)
st_dev$tmax_10km <- sd(tmax_10km_final$value)
st_dev$ppt_1km <- sd(ppt_1km_final$value)
st_dev$ppt_10km <- sd(ppt_10km_final$value)
st_dev$tmaxAug_1km <- sd(tmaxAug_1km_final$value)
st_dev$tmaxAug_10km <- sd(tmaxAug_10km_final$value)
st_dev$tmin_1km <- sd(tmin_1km_final$value)
st_dev$tmin_10km <- sd(tmin_10km_final$value)
st_dev$tminJan_1km <- sd(tminJan_1km_final$value)
st_dev$tminJan_10km <- sd(tminJan_10km_final$value)

