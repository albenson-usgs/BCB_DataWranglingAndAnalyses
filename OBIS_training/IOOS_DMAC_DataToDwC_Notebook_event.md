Aligning Data to Darwin Core - Sampling Event with Measurement or Fact
Abby Benson  
October 8,2019 
## General information about this notebook
This notebook was created for the IOOS DMAC Code Sprint Biological Data Session
The data in this notebook were created specifically as an example and meant solely to be
illustrative of the process for aligning data to the biological data standard - Darwin Core.
These data should not be considered actually occurrences of species and any measurements
are also contrived. This notebook is meant to provide a step by step process for taking
original data and aligning it to Darwin Core.


```r
library(readr)
library(uuid)
library(dplyr)

MadeUpDataForBiologicalDataTraining <- read_csv("~/OBIS/Reference Documentation/Presentations/IOOS DMAC Code Sprint/MadeUpDataForBiologicalDataTraining.csv")
```

```
## Parsed with column specification:
## cols(
##   date = col_character(),
##   lat = col_double(),
##   lon = col_double(),
##   region = col_character(),
##   station = col_double(),
##   transect = col_double(),
##   `scientific name` = col_character(),
##   `percent cover` = col_double(),
##   depth = col_double(),
##   `bottom type` = col_character(),
##   rugosity = col_double(),
##   temperature = col_double()
## )
```

First we need to to decide if we will provide an occurrence only version of the data or 
a sampling event with measurement or facts version of the data. Occurrence only is easier 
to create. It's only one file to produce. However, several pieces of information will be
left out if we choose that option. If we choose to do sampling event with measurement or
fact we'll be able to capture all of the data in the file creating a lossless version. 
Here we decide to use the sampling event option to include as much information as we can.
First let's create the eventID and occurrenceID in the original file so that information
can be reused for all necessary files down the line.


```r
MadeUpDataForBiologicalDataTraining$eventID <- paste(MadeUpDataForBiologicalDataTraining$region, 
                                                     MadeUpDataForBiologicalDataTraining$station, 
                                                     MadeUpDataForBiologicalDataTraining$transect, sep = "_") 
MadeUpDataForBiologicalDataTraining$occurrenceID <- ""
MadeUpDataForBiologicalDataTraining$occurrenceID <- sapply(MadeUpDataForBiologicalDataTraining$occurrenceID, 
                                       function(x) UUIDgenerate(use.time = TRUE))
```

We will need to create three separate files to comply with the sampling event format.
We'll start with the event file but we only need to include the columns that are relevant
to the event file.


```r
event <- MadeUpDataForBiologicalDataTraining[c("date", "lat", "lon", "region", "station",
                                               "transect", "depth", "bottom type", "eventID")]
```

Next we need to rename any columns of data that match directly to Darwin Core. We know
this based on our crosswalk spreadsheet CrosswalkToDarwinCore.csv


```r
event$decimalLatitude <- event$lat
event$decimalLongitude <- event$lon
event$minimumDepthInMeters <- event$depth
event$maximumDepthInMeters <- event$depth
event$habitat <- event$`bottom type`
event$island <- event$region
head(event, n = 10)
```

```
## # A tibble: 10 x 15
##    date    lat   lon region station transect depth `bottom type` eventID
##    <chr> <dbl> <dbl> <chr>    <dbl>    <dbl> <dbl> <chr>         <chr>  
##  1 7/16~  18.3 -64.8 St. J~     250        1    25 shallow reef~ St. Jo~
##  2 7/16~  18.3 -64.8 St. J~     250        1    25 shallow reef~ St. Jo~
##  3 7/16~  18.3 -64.8 St. J~     250        1    25 shallow reef~ St. Jo~
##  4 7/16~  18.3 -64.8 St. J~     250        1    25 shallow reef~ St. Jo~
##  5 7/16~  18.3 -64.8 St. J~     250        2    35 complex back~ St. Jo~
##  6 7/16~  18.3 -64.8 St. J~     250        2    35 complex back~ St. Jo~
##  7 7/16~  18.3 -64.8 St. J~     250        2    35 complex back~ St. Jo~
##  8 7/16~  18.3 -64.8 St. J~     250        2    35 complex back~ St. Jo~
##  9 7/16~  18.3 -64.8 St. J~     250        3    85 deep reef     St. Jo~
## 10 7/16~  18.3 -64.8 St. J~     250        3    85 deep reef     St. Jo~
## # ... with 6 more variables: decimalLatitude <dbl>,
## #   decimalLongitude <dbl>, minimumDepthInMeters <dbl>,
## #   maximumDepthInMeters <dbl>, habitat <chr>, island <chr>
```

We need to convert the date to ISO format


```r
event$eventDate <- as.Date(event$date, format = "%m/%d/%Y")
```

We will also have to add any missing required fields


```r
event$basisOfRecord <- "HumanObservation"
event$geodeticDatum <- "EPSG:4326 WGS84"
```

Then we'll remove any columns that we no longer need to clean things up a bit.


```r
event$date <- NULL
event$lat <- NULL
event$lon <- NULL
event$region <- NULL
event$station <- NULL
event$transect <- NULL
event$depth <- NULL
event$`bottom type` <- NULL
```

We have too many repeating rows of information. We can pare this down using eventID which
is a unique identifier for each sampling event in the data- which is six, three transects
per site.


```r
event <- event[which(!duplicated(event$eventID)),]
head(event, n = 6)
```

```
## # A tibble: 6 x 10
##   eventID decimalLatitude decimalLongitude minimumDepthInM~
##   <chr>             <dbl>            <dbl>            <dbl>
## 1 St. Jo~            18.3            -64.8               25
## 2 St. Jo~            18.3            -64.8               35
## 3 St. Jo~            18.3            -64.8               85
## 4 St. Jo~            18.3            -64.8               28
## 5 St. Jo~            18.3            -64.8               16
## 6 St. Jo~            18.3            -64.8               90
## # ... with 6 more variables: maximumDepthInMeters <dbl>, habitat <chr>,
## #   island <chr>, eventDate <date>, basisOfRecord <chr>,
## #   geodeticDatum <chr>
```

Finally we write out the event file


```r
write.csv(event, file="MadeUpData_event.csv", row.names=FALSE, fileEncoding="UTF-8", quote=TRUE)
```

Next we need to create the occurrence file.


```r
occurrence <- MadeUpDataForBiologicalDataTraining[c("scientific name", "eventID", "occurrenceID", "percent cover")] 

occurrence$scientificName <- occurrence$`scientific name`

occurrence$occurrenceStatus <-  ifelse (occurrence$`percent cover` == 0, "absent", "present")
```

## Taxonomic Name Matching
A requirement for OBIS is that all scientific names match to the World Register of 
Marine Species (WoRMS) and a scientificNameID is included. A scientificNameID looks
like this "urn:lsid:marinespecies.org:taxname:275730" with the last digits after
the colon being the WoRMS aphia ID. We'll need to go out to WoRMS to grab this
information.
Create a lookup table of unique scientific names


```r
lut_worms <- as.data.frame(unique(occurrence_only$scientificName))
lut_worms$scientificName <- as.character(lut_worms$`unique(occurrence_only$scientificName)`)
lut_worms$`unique(occurrence_only$scientificName)` <- NULL
lut_worms$scientificName <- as.character(lut_worms$scientificName)
lut_worms$acceptedname <- ""
lut_worms$acceptedID <- ""
lut_worms$scientificNameID <- ""
lut_worms$kingdom <- ""
lut_worms$phylum <- ""
lut_worms$class <- ""
lut_worms$order <- ""
lut_worms$family <- ""
lut_worms$genus <- ""
lut_worms$scientificNameAuthorship <- ""
lut_worms$taxonRank <- ""
```

Taxonomic lookup using the library taxizesoap


```r
for (i in 1:nrow(lut_worms)){
  df <- worms_records(scientific = lut_worms$scientificName[i])
  lut_worms[i,]$scientificNameID <- df$lsid[1]
  lut_worms[i,]$acceptedname <- df$valid_name[1]
  lut_worms[i,]$acceptedID <- df$valid_AphiaID[1]
  lut_worms[i,]$kingdom <- df$kingdom[1]
  lut_worms[i,]$phylum <- df$phylum[1]
  lut_worms[i,]$class <- df$class[1]
  lut_worms[i,]$order <- df$order[1]
  lut_worms[i,]$family <- df$family[1]
  lut_worms[i,]$genus <- df$genus[1]
  lut_worms[i,]$scientificNameAuthorship <- df$authority[1]
  lut_worms[i,]$taxonRank <- df$rank[1]
  message(paste("Looking up information for species:", lut_worms[i,]$scientificName))
}
```

```
## Looking up information for species: Acropora cervicornis
```

```
## Looking up information for species: Madracis auretenra
```

```
## Looking up information for species: Mussa angulosa
```

```
## Looking up information for species: Siderastrea radians
```

```r
occurrence <- merge(occurrence, lut_worms, by = "scientificName")
```

We're going to remove any unnecessary columns to clean up the file


```r
occurrence$`scientific name` <- NULL
occurrence$`percent cover` <- NULL


write.csv(occurrence, file="MadeUpData_Occurrence.csv", row.names=FALSE, fileEncoding="UTF-8", quote=TRUE)
```

The last file we need to create is the measurement or fact file


```r
temperature <- MadeUpDataForBiologicalDataTraining[c("eventID", "temperature", "date")]
temperature$occurrenceID <- ""
temperature$measurementType <- "temperature"
temperature$measurementTypeID <- ""
temperature$measurementValue <- temperature$temperature
temperature$measurementUnit <- "Celsius"
temperature$measurementUnitID <- ""
temperature$measurementDeterminedDate <- as.Date(temperature$date, format = "%m/%d/%Y")
temperature$measurementMethod <- ""
temperature$temperature <- NULL
temperature$date <- NULL
rugosity <- MadeUpDataForBiologicalDataTraining[c("eventID", "rugosity", "date")]
rugosity$occurrenceID <- ""
rugosity$measurementType <- "rugosity"
rugosity$measurementTypeID <- ""
rugosity$measurementValue <- rugosity$rugosity
rugosity$measurementUnit <- ""
rugosity$measurementUnitID <- ""
rugosity$measurementDeterminedDate <- as.Date(rugosity$date, format = "%m/%d/%Y")
rugosity$measurementMethod <- ""
rugosity$rugosity <- NULL
rugosity$date <- NULL
percentcover <- MadeUpDataForBiologicalDataTraining[c("eventID", "occurrenceID", "percent cover",
                                                      "date")]
percentcover$measurementType <- "Percent Cover"
percentcover$measurementTypeID <- ""
percentcover$measurementValue <- percentcover$`percent cover`
percentcover$measurementUnit <- ""
percentcover$measurementUnitID <- ""
percentcover$measurementDeterminedDate <- as.Date(percentcover$date, format = "%m/%d/%Y")
percentcover$measurementMethod <- ""
percentcover$`percent cover` <- NULL
percentcover$date <- NULL
measurementOrFact <- rbind(temperature, rugosity, percentcover)

write.csv(measurementOrFact, file="MadeUpData_mof.csv", row.names=FALSE, fileEncoding="UTF-8", quote=TRUE)
```

