# Script to align Ben Letcher's vB.csv data to Darwin Core
# Abby Benson
# 2019-08-29

# From an email from Ben the columns of data that are collected data (as opposed to derived data)
# are row numbers through "sex" so we can ignore the other columns of data for now.

library(readr)
library(tidyverse)
library(uuid)

# Read in dataset
vB <- read_csv("vB.csv")


vB_occurrence <- vB[,2:25]

## Record Level https://dwc.tdwg.org/terms/#record-level
vB_occurrence$type <- "event"
vB_occurrence$language <- "en"
vB_occurrence$license <- "http://creativecommons.org/publicdomain/zero/1.0/legalcode"
vB_occurrence$datasetID <- "USGS_PIT_Fish_Daily_Survival_Model"
vB_occurrence$institutionCode <- "USGS"
vB_occurrence$datasetName <- "USGS Passive Integrated Transponder data for stream fish daily survival model"
vB_occurrence$basisOfRecord <- "HumanObservation"

## Occurrence https://dwc.tdwg.org/terms/#occurrence
vB_occurrence$occurrenceID <- ""
vB_occurrence$occurrenceID <- sapply(vB_occurrence$occurrenceID, function(x) UUIDgenerate(use.time = TRUE))
vB_occurrence$individualCount <- 1
vB_occurrence$occurrenceStatus <- "present"

## Organism https://dwc.tdwg.org/terms/#organism
vB_occurrence$organismID <- vB_occurrence$tag

## Event https://dwc.tdwg.org/terms/#event
vB_occurrence$eventDate <- vB_occurrence$detectionDate
vB_occurrence$samplingProtocol <- vB_occurrence$survey

## Location https://dwc.tdwg.org/terms/#location
vB_occurrence$river <- paste0("river: ", vB_occurrence$river)
vB_occurrence$section<- paste0("section: ", vB_occurrence$section)
vB_occurrence$area <- paste0("area: ", vB_occurrence$area)
vB_occurrence$locality <- paste(vB_occurrence$river, vB_occurrence$section, vB_occurrence$area, sep = "; ")
# need to add latitude and longitude (or maybe footprintWKT instead?), geodetic datum, and coordinate uncertainty in meters

## Taxon https://dwc.tdwg.org/terms/#taxon
vB_occurrence$scientificName <- vB_occurrence$species
vB_occurrence$scientificName <- str_replace(vB_occurrence$scientificName, "bkt", "Salvelinus fontinalis")
vB_occurrence$scientificName <- str_replace(vB_occurrence$scientificName, "bnt", "Salmo trutta")
vB_occurrence$scientificName <- str_replace(vB_occurrence$scientificName, "ats", "Salmo salar")
# add scientific name ID? Taxonomic hierarchy?

vB_occurrence <- vB_occurrence[,c(25:39, 14, 19)]
vB_occurrence_snapshot <- vB_occurrence[1:"1000",]

write.csv(vB_occurrence_snapshot, file = "vB_to_DwC_subset_20190829.csv", fileEncoding = "UTF-8", row.names = F)

## Measurement or Fact https://dwc.tdwg.org/terms/#measurementorfact
observedLength <- vB_occurrence[c("occurrenceID","observedLength")]
observedLength <- observedLength[which(!is.na(observedLength$observedLength)),]
observedLength$measurementType <- "observed length of the fish"
observedLength$measurementTypeID <- ""
observedLength$measurementValue <- observedLength$observedLength
observedLength$measurementUnit <- "millimeter"
observedLength$measurementUnitID <- ""
observedLength$measurementMethod <- ""
observedLength$observedLength <- NULL

observedWeight <- vB_occurrence[c("occurrenceID","observedWeight")]
observedWeight <- observedWeight[which(!is.na(observedWeight$observedWeight)),]
observedWeight$measurementType <- "observed weight of the fish"
observedWeight$measurementTypeID <- ""
observedWeight$measurementValue <- observedWeight$observedWeight
observedWeight$measurementUnit <- "grams"
observedWeight$measurementUnitID <- ""
observedWeight$measurementMethod <- ""
observedWeight$observedWeight <- NULL

meanTemperature <- vB_occurrence[c("occurrenceID","meanTemperature")]
meanTemperature <- meanTemperature[which(!is.na(meanTemperature$meanTemperature)),]
meanTemperature$measurementType <- "mean temperature"
meanTemperature$measurementTypeID <- ""
meanTemperature$measurementValue <- meanTemperature$meanTemperature
meanTemperature$measurementUnit <- "degrees Celsius"
meanTemperature$measurementUnitID <- ""
meanTemperature$measurementMethod <- ""
meanTemperature$meanTemperature <- NULL

meanFlow<- vB_occurrence[c("occurrenceID","meanFlow")]
meanFlow <- meanFlow[which(!is.na(meanFlow$meanFlow)),]
meanFlow$measurementType <- "mean water flow"
meanFlow$measurementTypeID <- ""
meanFlow$measurementValue <- meanFlow$meanFlow
meanFlow$measurementUnit <- "meters cubed per second"
meanFlow$measurementUnitID <- ""
meanFlow$measurementMethod <- ""
meanFlow$meanFlow <- NULL

measurementorfact <- rbind(observedLength, observedWeight, meanTemperature, meanFlow)

observedLength_ss <- observedLength[1:"25",]
observedWeight_ss <- observedWeight[1:"100",]
meanTemperature_ss <- meanTemperature[1:"100",]
meanFlow_ss <- meanFlow[1:"100",]

measurementorfact_snapshot <- rbind(observedLength_ss, observedWeight_ss, meanTemperature_ss, meanFlow_ss)
write.csv(measurementorfact_snapshot, file = "vB_to_DwC_MoF_20190829.csv", fileEncoding = "UTF-8", row.names = F)

