# Abby's version of the National Fishes Vulnerability Assessment Compiled Code 6_11_2018.R
# Abby Benson
# 2018-10-23

# install necessary libraries
library(readr)
library(tidyverse)
library(rgbif)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(adehabitatHR)

#1-- First we need the final species list. The final species list was dervied using the following steps (...). The final species list
# can be found in the National Fishes Vulnerability Assessment Project Google Drive Space in the Species Selection Folder. It is in the
# "Species Selection by Taxonomy and Geography" spreadsheet, tab 3.

SpeciesList <- read_csv("~/BCB/MultipleSpeciesVulnerabilityNA/NFVAP_BAP/SpeciesSelectionbyTaxonomyandGeography_FinalList.csv")

# Want to limit the responses from GBIF to only those locations within the contiguous United States so I used to the OBIS WKT Tool
# (http://iobis.org/maptool/) to generate a very rough WKT for the US:
# GEOMETRYCOLLECTION(POLYGON ((-125.15625 48.48749, -125.20020 42.71473, -124.89258 40.27953, 
# -123.26660 37.30028, -120.62988 33.72434, -117.42188 32.54681, -114.87305 32.50976, -111.04980 31.31610, -108.28125 31.35364, 
# -108.23730 31.76554, -106.56738 31.84023, -105.38086 30.78904, -104.72168 29.99300, -103.13965 28.76766, -102.87598 29.30556, 
# -102.08496 29.80252, -101.33789 29.64987, -100.19531 28.11075, -99.31641 26.54922, -96.98730 25.79989, -97.03125 26.90248, 
# -95.00977 28.80617, -92.54883 29.03696, -89.16504 28.22697, -88.06641 29.38218, -85.38574 29.19053, -83.80371 27.91677, 
# -83.62793 24.64702, -81.78223 23.76524, -79.93652 24.04646, -79.05762 28.07198, -80.24414 30.41078, -79.93652 31.91487, 
# -76.55273 33.90690, -74.66309 35.03000, -75.19043 37.02010, -72.99316 40.11169, -70.83984 40.64730, -69.34570 40.97990, 
# -69.56543 42.84375, -68.86230 43.26121, -67.28027 43.80282, -66.66504 44.52784, -67.45605 45.58329, -67.80762 46.98025, 
# -68.51074 47.51720, -69.12598 47.45781, -70.22461 46.37725, -71.27930 45.21300, -72.46582 45.33670, -74.31152 45.27489, 
# -76.68457 43.96119, -77.56348 43.64403, -79.01367 43.51669, -78.88184 42.74701, -80.72754 42.48830, -82.57324 41.86956, 
# -83.01270 42.71473, -82.13379 43.48481, -82.57324 45.30580, -83.54004 45.82880, -84.85840 45.89001, -84.72656 46.83013, 
# -85.86914 47.33882, -88.15430 48.13677, -89.64844 48.01932, -90.92285 48.25394, -92.76855 48.48749, -94.79004 48.86471, 
# -95.05371 49.49667, -95.36133 49.00905, -106.87500 49.06667, -115.22461 49.03787, -122.69531 48.95137, -123.31055 48.22467, 
# -125.15625 48.48749)))

#2-- Go out to GBIF to grab the records for a species
### Couldn't get the API to work. JSON reading error. Will have to rely on rgbif. Keeping for now in case we need to look
# at the API work
# GBIFSpeciesQuery <- "http://api.gbif.org/v1/occurrence/search?geometry=POLYGON((
#-125.15625%2048.48749,%20-125.20020%2042.71473,%20-124.89258%2040.27953,%20-123.26660%2037.30028,%20-120.62988%2033.72434,
#%20-117.42188%2032.54681,%20-114.87305%2032.50976,%20-111.04980%2031.31610,%20-108.28125%2031.35364,%20-108.23730%2031.76554,
#%20-106.56738%2031.84023,%20-105.38086%2030.78904,%20-104.72168%2029.99300,%20-103.13965%2028.76766,%20-102.87598%2029.30556,
#%20-102.08496%2029.80252,%20-101.33789%2029.64987,%20-100.19531%2028.11075,%20-99.31641%2026.54922,%20-96.98730%2025.79989,
#%20-97.03125%2026.90248,%20-95.00977%2028.80617,%20-92.54883%2029.03696,%20-89.16504%2028.22697,%20-88.06641%2029.38218,
#%20-85.38574%2029.19053,%20-83.80371%2027.91677,%20-83.62793%2024.64702,%20-81.78223%2023.76524,%20-79.93652%2024.04646,
#%20-79.05762%2028.07198,%20-80.24414%2030.41078,%20-79.93652%2031.91487,%20-76.55273%2033.90690,%20-74.66309%2035.03000,
#%20-75.19043%2037.02010,%20-72.99316%2040.11169,%20-70.83984%2040.64730,%20-69.34570%2040.97990,%20-69.56543%2042.84375,
#%20-68.86230%2043.26121,%20-67.28027%2043.80282,%20-66.66504%2044.52784,%20-67.45605%2045.58329,%20-67.80762%2046.98025,
#%20-68.51074%2047.51720,%20-69.12598%2047.45781,%20-70.22461%2046.37725,%20-71.27930%2045.21300,%20-72.46582%2045.33670,
#%20-74.31152%2045.27489,%20-76.68457%2043.96119,%20-77.56348%2043.64403,%20-79.01367%2043.51669,%20-78.88184%2042.74701,
#%20-80.72754%2042.48830,%20-82.57324%2041.86956,%20-83.01270%2042.71473,%20-82.13379%2043.48481,%20-82.57324%2045.30580,
#%20-83.54004%2045.82880,%20-84.85840%2045.89001,%20-84.72656%2046.83013,%20-85.86914%2047.33882,%20-88.15430%2048.13677,
#%20-89.64844%2048.01932,%20-90.92285%2048.25394,%20-92.76855%2048.48749,%20-94.79004%2048.86471,%20-95.05371%2049.49667,
#%20-95.36133%2049.00905,%20-106.87500%2049.06667,%20-115.22461%2049.03787,%20-122.69531%2048.95137,%20-123.31055%2048.22467,
#%20-125.15625%2048.48749))&scientificName=%22Acipenser%20medirostris%22"
# GBIFSpeciesText <- readLines(curl(GBIFSpeciesQuery))
# GBIFSpecies <- fromJSON(GBIFSpeciesText)
# Error trying to parse the JSON. Loading the JSON into validator- looks like the JSON is not valid

# Using the scientificName as the search parameter means we'll get back any records for synonyms (ex: searching "Ameiurus 
# serracanthus" brings back records for that as well as "Ictalurus serracanthus")
GBIFSpecies <- occ_search(scientificName = "Ameiurus serracanthus", hasCoordinate = T, geometry = "POLYGON ((-125.15625 48.48749, 
-125.20020 42.71473, -124.89258 40.27953, -123.26660 37.30028, -120.62988 33.72434, -117.42188 32.54681, -114.87305 32.50976, 
-111.04980 31.31610, -108.28125 31.35364, -108.23730 31.76554, -106.56738 31.84023, -105.38086 30.78904, -104.72168 29.99300, 
-103.13965 28.76766, -102.87598 29.30556, -102.08496 29.80252, -101.33789 29.64987, -100.19531 28.11075, -99.31641 26.54922, 
-96.98730 25.79989, -97.03125 26.90248, -95.00977 28.80617, -92.54883 29.03696, -89.16504 28.22697, -88.06641 29.38218, 
-85.38574 29.19053, -83.80371 27.91677, -83.62793 24.64702, -81.78223 23.76524, -79.93652 24.04646, -79.05762 28.07198, 
-80.24414 30.41078, -79.93652 31.91487, -76.55273 33.90690, -74.66309 35.03000, -75.19043 37.02010, -72.99316 40.11169, 
-70.83984 40.64730, -69.34570 40.97990, -69.56543 42.84375, -68.86230 43.26121, -67.28027 43.80282, -66.66504 44.52784, 
-67.45605 45.58329, -67.80762 46.98025, -68.51074 47.51720, -69.12598 47.45781, -70.22461 46.37725, -71.27930 45.21300, 
-72.46582 45.33670, -74.31152 45.27489, -76.68457 43.96119, -77.56348 43.64403, -79.01367 43.51669, -78.88184 42.74701, 
-80.72754 42.48830, -82.57324 41.86956, -83.01270 42.71473, -82.13379 43.48481, -82.57324 45.30580, -83.54004 45.82880, 
-84.85840 45.89001, -84.72656 46.83013, -85.86914 47.33882, -88.15430 48.13677, -89.64844 48.01932, -90.92285 48.25394, 
-92.76855 48.48749, -94.79004 48.86471, -95.05371 49.49667, -95.36133 49.00905, -106.87500 49.06667, -115.22461 49.03787, 
-122.69531 48.95137, -123.31055 48.22467, -125.15625 48.48749))", limit = 500, fields = c("name","decimalLatitude",
                                                                                         "decimalLongitude","issues","eventDate",
                                                                                         "geodeticDatum","datasetName",
                                                                                         "occurrenceID", "occurrenceStatus",
                                                                                         "country", "countryCode","scientificName",
                                                                                         "locality","dataset"))



GBIFSpeciesData <- as.data.frame(GBIFSpecies$data)

#3-- Filter records that have the gbif issues COUNTRY_COORDINATE_MISMATCH, RECORDED_DATE_MISMATCH, ZERO_COORDINATE, and
# IDENTIFIED_DATE_UNLIKELY
dat <- GBIFSpeciesData %>%
  occ_issues(-cucdmis, -rdatm, -zerocd, -iddatunl)

dat_coord <- dat[c("decimalLongitude", "decimalLatitude")] #data frame of longitude and latitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
datfsp <- SpatialPointsDataFrame(dat_coord, dat, proj4string = crs.geo)#spatial data frame with correct projection

#4-- Filter out occurrences that are outside the native range for the species as determined by NatureServe. Data were downloaded
# from here http://www.natureserve.org/conservation-tools/data-maps-tools/digital-distribution-native-us-fishes-watershed
PATH_NatureServeFishRanges <- "F:/SpatialData/NS_Fish_Range_Shapefiles"
dat_sp_range <- readOGR(dsn = PATH_NatureServeFishRanges, layer = "Ameiurus_serracanthus") #NatureServe species range
proj4string(dat_sp_range) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
reproj_dat_sp_range <- spTransform(dat_sp_range, crs.geo)
clipped_occurrences_by_range <- datfsp[reproj_dat_sp_range,]

#5-- Filter out estuarine occurrence points since we only want to assess freshwater data specifically
# Estuary shapefile was downloaded from the EPA website https://www.epa.gov/hesc/estuary-data-mapper-edm and the Atlantic, Pacific,
# and Gulf of Mexico estuaries were merged into a single layer by the Mims team. Abby accessed the merged file in the Mims Lab google docs
# folder.
PATH_ESTUARY <- "F:/SpatialData/merged_estuary"
estuary <- readOGR(dsn = PATH_ESTUARY, layer="Merged_estuaries") # read in the estuary shapefile
proj4string(estuary) <- crs.geo
  
#subset estuarine records out from the occurrences
sub_est <- clipped_occurrences_by_range[estuary,] #subsets occurrence points to those of estuary
est_pts <- as.data.frame(sub_est) #changes spatial points to a dataframe for mapping with ggplot2
finalpoints_sp <- clipped_occurrences_by_range[(!clipped_occurrences_by_range$occurrenceID %in% est_pts$occurrenceID),] #takes out occurrence records in estuaries

#6-- Generate buffers for 1km, 5km, 10km, 20km
# Have to project data into a projected CRS because gBuffer will not work on geographic data. 
# Mims team chose USA Contiguous albers equal area
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")
finalpoints_Albers <- spTransform(finalpoints_sp, crs.albers)
AOOs <- data.frame(scientific_name = character(), buffer_1km = numeric(), buffer_5km = numeric(), buffer_10km = numeric(),
                   buffer_20km = numeric(), stringsAsFactors = F)
AOOs[1,]$scientific_name <- dat[1,]$name

### create 1 km buffer and add the total area to the AOO dataframe
sp_buffer_1km <- gBuffer(finalpoints_Albers, width = 1, byid= FALSE) #create a buffer of 1 km 
AOOs[1,]$buffer_1km <- gArea(sp_buffer_1km, byid = FALSE)

### create 5 km buffer
sp_buffer_5km <- gBuffer(finalpoints_Albers, width = 5, byid = F)
AOOs[1,]$buffer_5km <- gArea(sp_buffer_5km, byid = F)

### create 10 km buffer
sp_buffer_10km <- gBuffer(finalpoints_Albers, width = 10, byid = F)
AOOs[1,]$buffer_10km <- gArea(sp_buffer_10km, byid = F)

### create 20 km buffer
sp_buffer_20km <- gBuffer(finalpoints_Albers, width = 20, byid = F)
AOOs[1,]$buffer_20km <- gArea(sp_buffer_20km, byid = F)

#7-- Generate minimum convex polygon
### need only the lat/lon, can't have other information for the mcp functions in adeHabitatHR and can't seem to
### subset the spatial data same as a normal data frame so need to convert to a normal data frame, subset out
### lat/lon then turn it back into a spatial data frame
fp_sp_xy <- as.data.frame(finalpoints_sp)
fp_sp_xy <- fp_sp_xy[c("decimalLatitude", "decimalLongitude")] 
fp_sp_xy <- SpatialPoints(fp_sp_xy)
proj4string(fp_sp_xy) <- crs.geo
fp_sp_xy_albers <- spTransform(fp_sp_xy, crs.albers)
mcp <- mcp(fp_sp_xy_albers, percent = 100)
mcp_area <- mcp.area(fp_sp_xy_albers ,unout  = "km2", percent = 100, plotit = F) # this doesn't seem to be working
