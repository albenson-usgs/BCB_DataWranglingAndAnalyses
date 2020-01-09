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

# Wrap cleaning lines into a function to reuse
clean_dataset <- function(df) {
  df <- as.data.frame(df)
  df$year <- substr(df$eventDate, start = 1, stop = 4)
  df$year <- paste0("_", df$year)
  df_final <- df %>%
    gather(key = "prism_name", value = "value", 15:134) %>%
    rowwise() %>%
    filter(grepl(pattern = year, x = prism_name))
  return(df_final)
}

# Read in .rds files from yeti output and limit the data to only the instances
# where a point occurs in a particular year, eliminating data that do no follow that rule
PATH_YetiResults <- "~/BCB/MultipleSpeciesVulnerabilityNA/NFVAP_BAP/yeti_results"
filenames <- list.files(path = PATH_YetiResults, full.names = T)
ldf <- lapply(filenames, readRDS)
res <- lapply(ldf, clean_dataset)


# Now we need to calculate standard deviation on the values for each species and climate
# variable combinations
stdev <- lapply(res[[i]]$value, sd)
stdev <- c()
for (i in 1:length(res)){
  stdev[[i]] <- sd(res[[i]]$value, na.rm = T)# unsure why there are some NAs, not all sp-clim_var have them
  stdev <- list.append(stdev, res[[i]]$name[1])
  stdev <- list.append(stdev, res[[i]]$prism_name[1])
} #need to add in sp and clim_var to list somehow



