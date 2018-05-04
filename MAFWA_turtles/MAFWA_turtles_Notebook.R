#' Midwestern Association of Fish and Wildlife Agencies (MAFWA) SGCN Turtle Analysis
#' Abby Benson  
#' April 30, 2018  


# Data for this notebook was brought together and managed using the R script MAFWA_SGCN_Analysis.R

#' ## General information about the data
#' The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in the
#' Midwestern Association of Fish and Wildlife Agences (MAFWA). SGCN lists were collated by USGS and matched to the Integrated 
#' Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
#' and the NatureServe database. In this notebook we are looking specifically at turtles identified as SGCN in the MAFWA states and the
#' information available for those turtle species in BISON.
#' List of states included in this analysis: Illinois, Indiana, Iowa, Kansas, Kentucky, Michigan, Minnesota, Missouri, Nebraska, North Dakota,
#' Ohio, South Dakota, Wisconsin

#+ Load libraries, include=F
library(scales)
library(tidyverse)
library(ggplot2)
library(knitr)
library(gridExtra)
library(kableExtra)
library(gtable)
library(reshape2)
library(plotly)
library(jsonlite)
library(obistools)


#' Note: In order to have MAFWA_list and MAFWA_list_species, you need to first run the R script MAFWA_SGCN_Analysis.R and MAFWA_turtles_analysis.R

#+ SGCN_turtle_number, include=FALSE
#' ### Number of Species of Greatest Conservation Need turtles listed in MAFWA states:
nrow(MAFWA_turtlesOcc)

#+ FWS_listing, include=FALSE
#' ### General information about the twenty turtles from SGCN, Fish and Wildlife Service, and NatureServe

MAFWA_turtlesGenInfo <- MAFWA_turtlesGenInfo[c("ScientificName","Common Name","statelist2015","ListingStatus","NS_NationalStatus",
                                               "NS_NationalReviewDate")]
MAFWA_turtlesGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))

#' ### Let's examine the available data from BISON
bisonTurtleGenInfo <- bisonTurtleData %>%
  group_by(ScientificName) %>%
  summarise(Total = n(), `Minimum Date`=min(eventDate, na.rm = T), `Maximum Date`= max(eventDate, na.rm = T))
bisonTurtleGenInfo <- merge(bisonTurtleGenInfo, turtleCommonNames, by="ScientificName")
bisonTurtleGenInfo <- bisonTurtleGenInfo[c("ScientificName", "Common Name", "Total", "Minimum Date", "Maximum Date")]

bisonTurtleGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))

#' Some of the observations were missing dates
sum(is.na(bisonTurtleData$eventDate))

#' Plot a map of Blanding's, Spotted, and Wood Turtles with NatureServe Centroid locations removed
bisonTurtleData$decimalLatitude <- as.numeric(bisonTurtleData$decimalLatitude)
bisonTurtleData$decimalLongitude <- as.numeric(bisonTurtleData$decimalLongitude)
bisonBlandingsTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Emydoidea blandingii" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]
bisonSpottedTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Clemmys guttata" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]
bisonWoodTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Glyptemys insculpta" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]

#' #### Blanding's Turtle map
plot_map_leaflet(bisonBlandingsTurtles)
#' #### Spotted Turtle map
plot_map_leaflet(bisonSpottedTurtles)
#' #### Wood Turtle map
plot_map_leaflet(bisonWoodTurtles)

#' We might also want to know which institutions are supplying the data for these species
ds <- count(bisonTurtleData, dataSource)
ds_plot <- ggplot(ds, aes(dataSource, n))
ds_plot + geom_bar(aes(fill = dataSource), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  guides(fill = F)

#' Limit the previous figure to only the occurrences from the last 20 years
bisonTurtleData_recent <- bisonTurtleData[which(bisonTurtleData$eventDate >= "1998-01-01"),]
ds2 <- count(bisonTurtleData_recent, dataSource)
ds2_plot <- ggplot(ds2, aes(dataSource, n))
ds2_plot + geom_bar(aes(fill = dataSource), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  guides(fill = F)
