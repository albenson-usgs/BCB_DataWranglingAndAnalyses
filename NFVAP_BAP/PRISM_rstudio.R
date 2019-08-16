# Abby's version of the National Fishes Vulnerability Assessment "PRISM data extraction from buffers_11_13_2018.R" and
# "Post extraction PRISM processing_pointbuffers_11_13_2018.R"
# Abby Benson
# 2019-07-03

library(prism)
library(zoo)
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

# Import RDS file from the Yeti HPC procedure
# Apply a 30 year rolling window to all four buffer sizes for the maximum annual temperature PRISM variable
# Since the variable is maximum temp we use the max function in the rolling window.
tmax_Buffer <- readRDS("tmax_buffers.rds")
tmax_1kmBuffer <- tmax_Buffer[[1]]
tmax_1kmBuffer <- as.data.frame(tmax_1kmBuffer)
tmax_1kmBuffer <- tmax_1kmBuffer[c(5,11,15:134)]
zoo_tmax_1kmbuffer <- zoo(tmax_1kmBuffer[c(5,15:134)], order.by = tmax_1kmBuffer$eventDate)
#occ_data <- tmax_1kmBuffer[c(1:14)]
#rownames(tmax_1kmBuffer) <- tmax_1kmBuffer$occurrenceID
RW_tmax_1kmbuffer <- as.data.frame(rollapply(tmax_1kmBuffer_gather[c(4)], width = 30, by = 1, FUN = max))



tmax_5kmBuffer <- as.data.frame(tmax_Buffer[[2]])
RW_tmax_5kmbuffer <- as.data.frame(rollapply(tmax_5kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))
tmax_10kmBuffer <- as.data.frame(tmax_Buffer[[3]])
RW_tmax_10kmbuffer <- as.data.frame(rollapply(tmax_10kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))
tmax_20kmBuffer <- as.data.frame(tmax_Buffer[[4]])
RW_tmax_20kmbuffer <- as.data.frame(rollapply(tmax_20kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))

# Apply a 30 year rolling window to all four buffer sizes for the maximum August temperature PRISM variable
# Since the variable is maximum temp in August we use the max function in the rolling window.
tmaxAug_Buffer <- readRDS("tmaxAug_buffers.rds")
tmaxAug_1kmBuffer <- as.data.frame(tmaxAug_Buffer[[1]])
RW_tmaxAug_1kmbuffer <- as.data.frame(rollapply(tmaxAug_1kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))
tmaxAug_5kmBuffer <- as.data.frame(tmaxAug_Buffer[[2]])
RW_tmaxAug_5kmbuffer <- as.data.frame(rollapply(tmaxAug_5kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))
tmaxAug_10kmBuffer <- as.data.frame(tmaxAug_Buffer[[3]])
RW_tmaxAug_10kmbuffer <- as.data.frame(rollapply(tmaxAug_10kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))
tmaxAug_20kmBuffer <- as.data.frame(tmaxAug_Buffer[[4]])
RW_tmaxAug_20kmbuffer <- as.data.frame(rollapply(tmaxAug_20kmBuffer, width = 30, by = 1, FUN = max, na.rm = TRUE))

# Apply a 30 year rolling window to all four buffer sizes for the annual precipitation PRISM variable
# For this rolling window we chose to use the min function as we thought minimum precipitation would 
# have a greater affect for fish than max.
ppt_Buffer <- readRDS("ppt_buffers.rds")
ppt_1kmBuffer <- as.data.frame(ppt_Buffer[[1]])
RW_ppt_1kmbuffer <- as.data.frame(rollapply(ppt_1kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
ppt_5kmBuffer <- as.data.frame(ppt_Buffer[[2]])
RW_ppt_5kmbuffer <- as.data.frame(rollapply(ppt_5kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
ppt_10kmBuffer <- as.data.frame(ppt_Buffer[[3]])
RW_ppt_10kmbuffer <- as.data.frame(rollapply(ppt_10kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
ppt_20kmBuffer <- as.data.frame(ppt_Buffer[[4]])
RW_ppt_20kmbuffer <- as.data.frame(rollapply(ppt_20kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))

# Apply a 30 year rolling window to all four buffer sizes for the minimum annual temperature PRISM variable
# Since the variable is minimum temperature we chose the min function for the rolling window.
tmin_Buffer <- readRDS("tmin_buffers.rds")
tmin_1kmBuffer <- as.data.frame(tmin_Buffer[[1]])
RW_tmin_1kmbuffer <- as.data.frame(rollapply(tmin_1kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tmin_5kmBuffer <- as.data.frame(tmin_Buffer[[2]])
RW_tmin_5kmbuffer <- as.data.frame(rollapply(tmin_5kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tmin_10kmBuffer <- as.data.frame(tmin_Buffer[[3]])
RW_tmin_10kmbuffer <- as.data.frame(rollapply(tmin_10kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tmin_20kmBuffer <- as.data.frame(tmin_Buffer[[4]])
RW_tmin_20kmbuffer <- as.data.frame(rollapply(tmin_20kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))

# Apply a 30 year rolling window to all four buffer sizes for the minimum annual temperature in January 
# PRISM variable. Since the variable is minimum January temperature we chose the min function for the 
# rolling window.
tminJan_Buffer <- readRDS("tminJan_buffers.rds")
tminJan_1kmBuffer <- as.data.frame(tminJan_Buffer[[1]])
RW_tminJan_1kmbuffer <- as.data.frame(rollapply(tminJan_1kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tminJan_5kmBuffer <- as.data.frame(tminJan_Buffer[[2]])
RW_tminJan_5kmbuffer <- as.data.frame(rollapply(tminJan_5kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tminJan_10kmBuffer <- as.data.frame(tminJan_Buffer[[3]])
RW_tminJan_10kmbuffer <- as.data.frame(rollapply(tminJan_10kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))
tminJan_20kmBuffer <- as.data.frame(tminJan_Buffer[[4]])
RW_tminJan_20kmbuffer <- as.data.frame(rollapply(tminJan_20kmBuffer, width = 30, by = 1, FUN = min, na.rm = TRUE))

#### Next up figure out how to limit to the years which match with eventDate
# First let's see if we can limit for one dataframe
# Grab the year of data for each occurrence
finalpoints_sp$year <- substr(finalpoints_sp$eventDate, start = 1, stop = 4)
finalpoints_sp$year <- paste0("_", finalpoints_sp$year)

data_years <- unique(finalpoints_sp$year)
PRISM_data_list = do.call("list", mget(grep("RW", ls(), value=T)))

datalist <- list()
set <- list()
for (d in 1:length(PRISM_data_list)){
  for (i in data_years){
    dat <- PRISM_data_list[[d]][grepl(i, colnames(PRISM_data_list[[d]]))]
    if (ncol(dat) > 0){
      datalist[[i]] <- dat
    }
  }
  df <- do.call(cbind,datalist)
  PRISM_data_list[d] <- list(df)
}

# Now we need to limit to only the data point for the year for the occurrence (instead of PRISM data for
# every occurrence for every year)
final_species_PRISM_data <- as.data.frame(finalpoints_sp[c("occurrenceID", "scientificName", "eventDate", "year")])
final_species_PRISM_data <- final_species_PRISM_data[which(!is.na(final_species_PRISM_data$eventDate)),]
final_species_PRISM_data$ppt <- ifelse(final_species_PRISM_data$year %in% colnames(PRISM_data_list$RW_ppt_10kmbuffer)) 


#### Now we need to calculate standard deviation
