library(jsonlite)
library(tidyverse)
library(mongolite)
library(dplyr)
library(kableExtra)
library(plyr)


# Pulls the SGCN National List from MongoDB
bis_sgcnsynthesis2 <- mongo(collection = "xxxxxxx", db = "xxxxxxx", url = "xxxxxxx")
mongo_sgcn2 <- bis_sgcnsynthesis2$find('{}', fields = '{"_id":1, "Common Name":1, "Taxonomic Group":1, "Match Method":1, "Taxonomy":1, "TESS":1, "NatureServe":1, "Source Data Summary":1}')


sgcn_natlist <- mongo_sgcn2
sgcn_natlist$ScientificName <- sgcn_natlist$`_id`

# Let's break this down into manageable chunks first starting with FWS WSFR Region 3. But in order to do that we need the state list.
# This bit of code is extremely slow. Would be good to figure out how to speed this up. Takes hours to run.
sgcn_natlist$statelist2015 <- NA
sgcn_natlist$statelist2005 <- NA
for (i in 1:nrow(sgcn_natlist)){
  if(!is.null(sgcn_natlist$`Source Data Summary`[[i]]$`2015`$States)){
    staterow <- unlist(sgcn_natlist$`Source Data Summary`[[i]]$`2015`$States[[1]])
    sgcn_natlist[i,]$statelist2015 <- paste(staterow, collapse = ",")
  }
}

for (i in 1:nrow(sgcn_natlist)){
  if(!is.null(sgcn_natlist$`Source Data Summary`[[i]]$`2005`$States)){
    staterow <- unlist(sgcn_natlist$`Source Data Summary`[[i]]$`2005`$States)
    sgcn_natlist[i,]$statelist2005 <- paste(staterow, collapse = ",")
  }
}

### Subset the data to just the national lists species for a particular region
toMatchFWSRegion1 <- c("Idaho","Oregon","Washington","Hawaii","Northern Mariana Islands","Puerto Rico","U.S. Virgin Islands","Guam","American Samoa") 
FWSRegion1_list <- sgcn_natlist[grep(paste(toMatchFWSRegion1,collapse = "|"),sgcn_natlist$statelist2015),]


# Extract out the FWS listing status from the document structure coming back from Mongodb2.0
FWSRegion1_list$ListingStatus <- NA
FWSRegion1_list$ListingStatus2 <- NA
FWSRegion1_list$ListingStatus3 <- NA
FWSRegion1_list$ListingStatus4 <- NA
for (i in 1:nrow (FWSRegion1_list)) {
  if (!is.null(FWSRegion1_list[i,]$TESS$listingStatus[[1]])){
    tessrow <- as.data.frame(FWSRegion1_list[i,]$TESS$listingStatus, simplifyDataFrame=T)
    if (nrow(tessrow) == 1) {
      FWSRegion1_list[i,]$ListingStatus <- tessrow$STATUS[[1]]
    }
    if (nrow(tessrow) == 2){
    FWSRegion1_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
    FWSRegion1_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
    }
    if (nrow(tessrow) == 3){
      FWSRegion1_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      FWSRegion1_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      FWSRegion1_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
    }
    if (nrow(tessrow) == 4){
      FWSRegion1_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      FWSRegion1_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      FWSRegion1_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
      FWSRegion1_list[i,]$ListingStatus4 <- tessrow$STATUS[[4]]
    }
  } else {
    FWSRegion1_list[i,]$ListingStatus <- NA
  }
}

FWSRegion1_list$TESS <- NULL

# Swap higher priority listing status for lower priority ones
for (i in 1:nrow(FWSRegion1_list)) {
  if (!is.na(FWSRegion1_list[i,]$ListingStatus2) & FWSRegion1_list[i,]$ListingStatus2 == "Endangered") {
    FWSRegion1_list[i,]$ListingStatus <- paste(FWSRegion1_list[i,]$ListingStatus,FWSRegion1_list[i,]$ListingStatus2, sep = "")
    FWSRegion1_list[i,]$ListingStatus2 <- substr(FWSRegion1_list[i,]$ListingStatus,0,nchar(FWSRegion1_list[i,]$ListingStatus) - nchar(FWSRegion1_list[i,]$ListingStatus2))
    FWSRegion1_list[i,]$ListingStatus <- substr(FWSRegion1_list[i,]$ListingStatus,nchar(FWSRegion1_list[i,]$ListingStatus2) + 1, nchar(FWSRegion1_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(FWSRegion1_list)) {
  if (!is.na(FWSRegion1_list[i,]$ListingStatus3) & FWSRegion1_list[i,]$ListingStatus3 == "Endangered") {
    FWSRegion1_list[i,]$ListingStatus <- paste(FWSRegion1_list[i,]$ListingStatus,FWSRegion1_list[i,]$ListingStatus3, sep = "")
    FWSRegion1_list[i,]$ListingStatus3 <- substr(FWSRegion1_list[i,]$ListingStatus,0,nchar(FWSRegion1_list[i,]$ListingStatus) - nchar(FWSRegion1_list[i,]$ListingStatus3))
    FWSRegion1_list[i,]$ListingStatus <- substr(FWSRegion1_list[i,]$ListingStatus,nchar(FWSRegion1_list[i,]$ListingStatus3) + 1, nchar(FWSRegion1_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(FWSRegion1_list)) {
  if (!is.na(FWSRegion1_list[i,]$ListingStatus3) & FWSRegion1_list[i,]$ListingStatus3 == "Recovery") {
    FWSRegion1_list[i,]$ListingStatus <- paste(FWSRegion1_list[i,]$ListingStatus,FWSRegion1_list[i,]$ListingStatus3, sep = "")
    FWSRegion1_list[i,]$ListingStatus3 <- substr(FWSRegion1_list[i,]$ListingStatus,0,nchar(FWSRegion1_list[i,]$ListingStatus) - nchar(FWSRegion1_list[i,]$ListingStatus3))
    FWSRegion1_list[i,]$ListingStatus <- substr(FWSRegion1_list[i,]$ListingStatus,nchar(FWSRegion1_list[i,]$ListingStatus3) + 1, nchar(FWSRegion1_list[i,]$ListingStatus))
  }
}

# Now we want to grab what we need from the NatureServe rank data
FWSRegion1_list$NSGlobalDescription <- FWSRegion1_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$roundedRank$description
FWSRegion1_list$NSGlobalReviewDate <- FWSRegion1_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$statusLastReviewed

# Not grabbing the NS state ranks for now. Try and come back to that later.
FWSRegion1_list$NatureServe <- NULL

# Populating the taxonomic hierarchy from the Taxonomy information from Mongodb
FWSRegion1_list$kingdom <- NA
FWSRegion1_list$phylum <- NA
FWSRegion1_list$class <- NA
FWSRegion1_list$order <- NA
FWSRegion1_list$family <- NA
FWSRegion1_list$genus <- NA
for (i in 1: nrow(FWSRegion1_list)) {
  if (!is.null(FWSRegion1_list$Taxonomy[[i]])) {
  taxonomy_row <- FWSRegion1_list[i,]$Taxonomy
  df <- ldply(taxonomy_row, data.frame)
  if (is.na(FWSRegion1_list[i,]$kingdom)) {
    FWSRegion1_list[i,]$kingdom <- df$name[which(df$rank == "Kingdom")]
    FWSRegion1_list[i,]$phylum <- df$name[which(df$rank == "Phylum" | df$rank == "Division")]
    if (nrow(df[which(df$rank == "Class"),]) >0) { 
      FWSRegion1_list[i,]$class <- df$name[which(df$rank == "Class")]
    } 
    if (nrow(df[which(df$rank == "Order"),]) >0) {
      FWSRegion1_list[i,]$order <- df$name[which(df$rank == "Order")]
    } 
    if (nrow(df[which(df$rank == "Family"),]) >0) {
      FWSRegion1_list[i,]$family <- df$name[which(df$rank == "Family")]
    } 
    if (nrow(df[which(df$rank == "Genus"),]) >0) {
      FWSRegion1_list[i,]$genus <- df$name[which(df$rank == "Genus")]
    }
    }
  }
}

# Need to unload the package "plyr" here because it causes problems for summarizing the data below
detach(package:plyr)

### See what BISON has for these species
FWSRegion1_list$bisonQuery <- NA
for(i in 1:nrow(FWSRegion1_list)){
  if (is.na(FWSRegion1_list[i,]$bisonQuery)){
      FWSRegion1_list[i,]$bisonQuery <- paste("https://bison.usgs.gov/api/search.json?count=1&type=scientific_name&species=", (URLencode(FWSRegion1_list[i,]$ScientificName)), sep='')
    }
  }

FWSRegion1_list$bisontotal <- NA
FWSRegion1_list$bisonOccLiterature <- NA
FWSRegion1_list$bisonOccObservation <- NA
FWSRegion1_list$bisonOccCentroid <- NA
FWSRegion1_list$bisonOccSpecimen <- NA
FWSRegion1_list$bisonOccFossil <- NA
FWSRegion1_list$bisonOccUnknown <- NA
FWSRegion1_list$bisonIdaho <- NA
FWSRegion1_list$bisonOregon <- NA
FWSRegion1_list$bisonWashington <- NA
FWSRegion1_list$bisonHawaii <- NA
FWSRegion1_list$bisonAmericanSamoa <- NA
FWSRegion1_list$bisonGuam <- NA
FWSRegion1_list$bisonNortherMarianaIslands <- NA
FWSRegion1_list$bisonUSVirginIslands <- NA
FWSRegion1_list$bisonPuertoRico <- NA
bisonrow <- list()
for(i in 1:nrow(FWSRegion1_list)){
  if (is.na(FWSRegion1_list[i,]$bisontotal)){
    bisonrow <- fromJSON(FWSRegion1_list[i,]$bisonQuery)
    FWSRegion1_list[i,]$bisontotal <- bisonrow$total
    FWSRegion1_list[i,]$bisonOccLiterature <- bisonrow$occurrences$legend$literature
    FWSRegion1_list[i,]$bisonOccFossil <- bisonrow$occurrences$legend$fossil
    FWSRegion1_list[i,]$bisonOccCentroid <- bisonrow$occurrences$legend$centroid
    FWSRegion1_list[i,]$bisonOccObservation <- bisonrow$occurrences$legend$observation
    FWSRegion1_list[i,]$bisonOccSpecimen <- bisonrow$occurrences$legend$specimen
    FWSRegion1_list[i,]$bisonOccUnknown <- bisonrow$occurrences$legend$unknown
    FWSRegion1_list[i,]$bisonIdaho <- bisonrow$states$data$Idaho$total
    FWSRegion1_list[i,]$bisonOregon <- bisonrow$states$data$Oregon$total
    FWSRegion1_list[i,]$bisonWashington <- bisonrow$states$data$Washington$total
    FWSRegion1_list[i,]$bisonHawaii <- bisonrow$states$data$Hawaii$total
    FWSRegion1_list[i,]$bisonGuam <- bisonrow$states$data$Guam$total
    FWSRegion1_list[i,]$bisonAmericanSamoa <- bisonrow$states$data$`American Samoa`$total
    FWSRegion1_list[i,]$bisonUSVirginIslands <- bisonrow$states$data$`United States Virgin Islands`$total
    FWSRegion1_list[i,]$bisonNortherMarianaIslands <- bisonrow$states$data$`Commonwealth of the Northern Mariana Islands`$total
    FWSRegion1_list[i,]$bisonPuertoRico <- bisonrow$states$data$`Puerto Rico`$total
  }
}

FWSRegion1_list$bisonIdaho <- ifelse(FWSRegion1_list$bisonIdaho == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonIdaho)
FWSRegion1_list$bisonOregon <- ifelse(FWSRegion1_list$bisonOregon == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonOregon)
FWSRegion1_list$bisonWashington <- ifelse(FWSRegion1_list$bisonWashington == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonWashington)
FWSRegion1_list$bisonHawaii <- ifelse(FWSRegion1_list$bisonHawaii == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonHawaii)
FWSRegion1_list$bisonGuam <- ifelse(FWSRegion1_list$bisonGuam == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonGuam)
FWSRegion1_list$bisonAmericanSamoa <- ifelse(FWSRegion1_list$bisonAmericanSamoa == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonAmericanSamoa)
FWSRegion1_list$bisonNortherMarianaIslands <- ifelse(FWSRegion1_list$bisonNortherMarianaIslands == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonNortherMarianaIslands)
FWSRegion1_list$bisonUSVirginIslands <- ifelse(FWSRegion1_list$bisonUSVirginIslands == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonUSVirginIslands)
FWSRegion1_list$bisonPuertoRico <- ifelse(FWSRegion1_list$bisonPuertoRico == FWSRegion1_list$ScientificName, NA, FWSRegion1_list$bisonPuertoRico)

FWSRegion1_list[,24:38] <- lapply(FWSRegion1_list[,24:38], function(x) as.numeric(x))

#bisonAndStateListsSaved <- FWSRegion1_list[c(7:34)]
#bisonAndStateListsSaved <- bisonAndStateListsSaved[c(-8:-10)]

# Group by class

r1 <- FWSRegion1_list %>%
  group_by(class) %>%
  summarize(Idaho = sum(bisonIdaho, na.rm = T), Oregon = sum(bisonOregon, na.rm = T), 
            Washington = sum(bisonWashington, na.rm = T), Hawaii = sum(bisonHawaii, na.rm = T), Guam = sum(bisonGuam, na.rm = T), 
            AmericanSamoa = sum(bisonAmericanSamoa, na.rm = T), VirginIslands = sum(bisonUSVirginIslands, na.rm = T), 
            PuertoRico = sum(bisonPuertoRico, na.rm = T), NMariananIslands = sum(bisonNortherMarianaIslands, na.rm = T))
r1$Total <- NA
for (i in 1:nrow(r1)){
  r1[i,]$Total <- (r1[i,]$Idaho + r1[i,]$Oregon + r1[i,]$Washington + r1[i,]$Hawaii + r1[i,]$Guam + r1[i,]$NMariananIslands + r1[i,]$PuertoRico + r1[i,]$AmericanSamoa + r1[i,]$VirginIslands)
}
r1$class <- factor(r1$class, levels = r1$class[order(r1$Total)])
r1 <- r1[order(r1$Total),] 
r1 <- r1[-41,]

# Create a two-way table
r1_twoway <- r1[order(r1$Total),]

# # Create a log version of the data to prep for a dot plot since Aves makes it impossible to see the other classes
# r_log <- r
# r_log$logTotal <- log10(r_log$FWSRegionTotal)
# r_log <- r_log[order(r_log$logTotal),]

ls_r1 <- FWSRegion1_list %>%
  group_by(class, ListingStatus) %>%
  summarize(count = n())
ls_r1 <- ls_r1[which(!is.na(ls_r1$ListingStatus)),]
ls_r1 <- ls_r1[which(ls_r1$ListingStatus == "Candidate" | ls_r1$ListingStatus == "Endangered" | 
                                             ls_r1$ListingStatus == "Threatened" | 
                                             ls_r1$ListingStatus == "Under Review in the Candidate or Petition Process"),]

ListingStatus2 <- FWSRegion1_list[which(!is.na(FWSRegion1_list$ListingStatus2)),]

EndangeredList <- FWSRegion1_list[which(FWSRegion1_list$ListingStatus == "Endangered"),]

# What can we say about the distribution of observation types at the class level?

o1 <- FWSRegion1_list %>%
  group_by(class) %>%
  summarize(Count = n(), Literature = sum(bisonOccLiterature, na.rm = T), Observation = sum(bisonOccObservation, na.rm = T), 
            Specimen = sum(bisonOccSpecimen, na.rm = T), Fossil = sum(bisonOccFossil, na.rm = T), Unknown = sum(bisonOccUnknown, na.rm = T))
o1$class <- paste(o1$class, o1$Count, sep = " (")
o1$class <- paste(o1$class, ")", sep = "")
o1 <- o1[,-2]
o1_gather <- gather(o1, key = "type", value = "Count", -class)



