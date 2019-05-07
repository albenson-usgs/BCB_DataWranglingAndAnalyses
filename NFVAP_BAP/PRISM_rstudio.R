library(prism)

options(prism.path = "~/prismtmp")
get_prism_annual(type = "tmax", year = 1895:2014, keepZip = FALSE)
get_prism_annual(type = "tmin", year = 1895:2014, keepZip = FALSE)
get_prism_annual(type = "tmean", year = 1895:2014, keepZip = FALSE)
get_prism_annual(type = "ppt", year = 1895:2014, keepZip = FALSE)
get_prism_monthlys(type = "tmax", year = 1895:2014, mon = 8, keepZip = FALSE)
get_prism_monthlys(type = "tmin", year = 1895:2014, mon = 1, keepZip = FALSE)

# Extract PRISM values for each species observation point. Buffers are added during the extract function. This code
# needs to be run on a high performance computing cluster like USGS Yeti
### Run PRISM_variable_yeti.R on a HPC - each variable has its own R script

RW_min <- as.data.frame(rollapply(ext_ID1, width = 30, by = 1, FUN = min, na.rm = TRUE))
