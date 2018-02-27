library(jsonlite)
library(tidyverse)
library(mongolite)
library(dplyr)
library(kableExtra)

# Pulls the SGCN National List from MongoDB
bis_sgcnsynthesis2 <- mongo(collection = "xxxxxxxxx", db = "xxxxxxxxxxxxx", url = "xxxxxxxxxxxxxxxxx")
mongo_sgcn2 <- bis_sgcnsynthesis2$find('{}', fields = '{"_id":1, "Common Name":1, "Taxonomic Group":1, "Match Method":1, "Taxonomy":1, "TESS":1, "NatureServe":1, "Source Data Summary":1}')
mongo_sgcn2$`Scientific Name` <- mongo_sgcn2$`_id`
mongo_sgcn2$CommonName <- mongo_sgcn2$`Common Name`
mongo_compact <- mongo_sgcn2[c("CommonName", "Scientific Name")]

sgcn_complist <- mongo_sgcn2

# Bring in the FWS National Listing Workplan (https://www.fws.gov/endangered/esa-library/pdf/Listing%207-Year%20Workplan%20Sept%202016.pdf)
#tabula_Listing_7_Year_Workplan_Sept_2016 <- read_csv("~/SWAPs/DataAnalysis/FWSNationalListingWorkplan/tabula-Listing 7-Year Workplan Sept 2016.csv")
Prelisting_Datcall_Species_List_FINAL_November_9 <- read_csv("~/SWAPs/DataAnalysis/FWSNationalListingWorkplan/Prelisting Datcall Species List FINAL November 9.csv")
Prelisting_Datcall_Species_List_FINAL_November_9 <- Prelisting_Datcall_Species_List_FINAL_November_9[-364:-999,-7:-14]

sgcn_fwsListingWP <- merge(Prelisting_Datcall_Species_List_FINAL_November_9, mongo_compact, by = "Scientific Name", all.x = T)
unmatchedFWSnames <- sgcn_fwsListingWP[which(is.na(sgcn_fwsListingWP$CommonName)),]
unmatchedFWSnames <- unmatchedFWSnames[,-7]
sgcn_fwsListingWP <- sgcn_fwsListingWP[which(!is.na(sgcn_fwsListingWP$ScientificName)),]

# Merge with information we have in the SGCN Summary table for these species
sgcn_fwsListingWP <- merge(Prelisting_Datcall_Species_List_FINAL_November_9, mongo_sgcn2, by = "Scientific Name")
sgcn_fwsListingWP$TESS <- NULL
sgcn_fwsListingWP$NatureServe <- NULL

# Figure out which states have listed these species
sgcn_fwsListingWP$statelist2015 <- NA
sgcn_fwsListingWP$statelist2005 <- NA
for (i in 1:nrow(sgcn_fwsListingWP)){
  if(!is.null(sgcn_fwsListingWP$`Source Data Summary`[[i]]$`2015`$States)){
    staterow <- unlist(sgcn_fwsListingWP$`Source Data Summary`[[i]]$`2015`$States[[1]])
    sgcn_fwsListingWP[i,]$statelist2015 <- paste(staterow, collapse = ",")
  }
}

for (i in 1:nrow(sgcn_fwsListingWP)){
  if(!is.null(sgcn_fwsListingWP$`Source Data Summary`[[i]]$`2005`$States)){
    staterow <- unlist(sgcn_fwsListingWP$`Source Data Summary`[[i]]$`2005`$States)
    sgcn_fwsListingWP[i,]$statelist2005 <- paste(staterow, collapse = ",")
  }
}

### See what BISON has for these species
sgcn_fwsListingWP$bisonQuery <- NA
for(i in 1:nrow(sgcn_fwsListingWP)){
  if (is.na(sgcn_fwsListingWP[i,]$bisonQuery)){
    sgcn_fwsListingWP[i,]$bisonQuery <- paste("https://bison.usgs.gov/solr/occurrences/select?q=scientificName:(%22", (URLencode(sgcn_fwsListingWP[i,]$`Scientific Name`)), sep='')
  }
}
sgcn_fwsListingWP$bisonQuery <- paste(sgcn_fwsListingWP$bisonQuery, "%22)&facet.mincount=1&rows=0&facet=true&facet.missing=true&facet.limit=-1&wt=json&indent=true&facet.field=basisOfRecord", sep = '')

sgcn_fwsListingWP$bisontotal <- NA
# sgcn_fwsListingWP$bisonOccLiterature <- NA
# sgcn_fwsListingWP$bisonOccObservation <- NA
# sgcn_fwsListingWP$bisonOccCentroid <- NA
# sgcn_fwsListingWP$bisonOccSpecimen <- NA
# sgcn_fwsListingWP$bisonOccFossil <- NA
# sgcn_fwsListingWP$bisonOccUnknown <- NA

for(i in 1:nrow(sgcn_fwsListingWP)){
  if (is.na(sgcn_fwsListingWP[i,]$bisontotal)){
    bisonrow <- fromJSON(sgcn_fwsListingWP[i,]$bisonQuery)
    sgcn_fwsListingWP[i,]$bisontotal <- bisonrow$response$numFound
  }
}

# I'm not able to grab the basis of record right now because SOLR sends back the data as a one combined string. I could parse it if each
# element was always accounted for but for some returns there are observations and specimens only while others might have literature, specimens,
# and observations so the elements all fall in different places within the string

# sgcn_fwsListingWP[i,]$bisonOccLiterature <- bisonrow$occurrences$legend$literature
# sgcn_fwsListingWP[i,]$bisonOccFossil <- bisonrow$occurrences$legend$fossil
# sgcn_fwsListingWP[i,]$bisonOccCentroid <- bisonrow$occurrences$legend$centroid
# sgcn_fwsListingWP[i,]$bisonOccObservation <- bisonrow$occurrences$legend$observation
# sgcn_fwsListingWP[i,]$bisonOccSpecimen <- bisonrow$occurrences$legend$specimen
# sgcn_fwsListingWP[i,]$bisonOccUnknown <- bisonrow$occurrences$legend$unknown

# sgcn_fwsListingWP$bisonOccLiterature <- ifelse(sgcn_fwsListingWP$bisonOccLiterature == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccLiterature)
# sgcn_fwsListingWP$bisonOccFossil<- ifelse(sgcn_fwsListingWP$bisonOccFossil == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccFossil)
# sgcn_fwsListingWP$bisonOccCentroid <- ifelse(sgcn_fwsListingWP$bisonOccCentroid == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccCentroid)
# sgcn_fwsListingWP$bisonOccObservation<- ifelse(sgcn_fwsListingWP$bisonOccObservation == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccObservation)
# sgcn_fwsListingWP$bisonOccSpecimen <- ifelse(sgcn_fwsListingWP$bisonOccSpecimen == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccSpecimen)
# sgcn_fwsListingWP$bisonOccUnknown <- ifelse(sgcn_fwsListingWP$bisonOccUnknown == sgcn_fwsListingWP$ScientificName, NA, sgcn_fwsListingWP$bisonOccUnknown)
# 
# sgcn_fwsListingWP[,22:27] <- lapply(sgcn_fwsListingWP[,22:27], function(x) as.numeric(x))

# Populating the taxonomic hierarchy from the Taxonomy information from Mongodb
library(plyr)
sgcn_fwsListingWP$kingdom <- NA
sgcn_fwsListingWP$phylum <- NA
sgcn_fwsListingWP$class <- NA
sgcn_fwsListingWP$order <- NA
sgcn_fwsListingWP$family <- NA
sgcn_fwsListingWP$genus <- NA
for (i in 1: nrow(sgcn_fwsListingWP)) {
  if (!is.null(sgcn_fwsListingWP$Taxonomy[[i]])) {
    taxonomy_row <- sgcn_fwsListingWP[i,]$Taxonomy
    df <- ldply(taxonomy_row, data.frame)
    if (is.na(sgcn_fwsListingWP[i,]$kingdom)) {
      sgcn_fwsListingWP[i,]$kingdom <- df$name[which(df$rank == "Kingdom")]
      sgcn_fwsListingWP[i,]$phylum <- df$name[which(df$rank == "Phylum" | df$rank == "Division")]
      if (nrow(df[which(df$rank == "Class"),]) >0) { 
        sgcn_fwsListingWP[i,]$class <- df$name[which(df$rank == "Class")]
      } 
      if (nrow(df[which(df$rank == "Order"),]) >0) {
        sgcn_fwsListingWP[i,]$order <- df$name[which(df$rank == "Order")]
      } 
      if (nrow(df[which(df$rank == "Family"),]) >0) {
        sgcn_fwsListingWP[i,]$family <- df$name[which(df$rank == "Family")]
      } 
      if (nrow(df[which(df$rank == "Genus"),]) >0) {
        sgcn_fwsListingWP[i,]$genus <- df$name[which(df$rank == "Genus")]
      }
    }
  }
}
detach("package:plyr", unload=TRUE)

# Next we need to get everything prepped for the Notebook
# Group by class

r2 <- sgcn_fwsListingWP %>%
  group_by(class) %>%
  summarize(Count = n(), Total = sum(bisontotal))

# Move the count of the number of species within that class to the class field
r2$class <- paste(r2$class, r2$Count, sep = " (")
r2$class <- paste(r2$class, ")", sep = "")
r2 <- r2[,-2]

# order by total instead of alphabetically
r2$class <- factor(r2$class, levels = r2$class[order(r2$Total)])
r2 <- r2[order(r2$Total),] 

# Group by proposed time frame
t2 <- sgcn_fwsListingWP %>%
  group_by(Timeframe) %>%
  summarize(Count = n(), Total = sum(bisontotal))

# Move the count of the number of species within that class to the class field
t2$Timeframe <- paste(t2$Timeframe, t2$Count, sep = " (")
t2$Timeframe <- paste(t2$Timeframe, ")", sep = "")
t2 <- t2[,-2]

# Create proposed timeframe tables
tableprep_FWSlistingWP <- sgcn_fwsListingWP[c("Common Name","Scientific Name","Range","statelist2015","Lead FWS Regional Office","Proposed FWS Decision Timeframe (Fiscal Year)","bisontotal")]
tableprep_FWSlistingWP$`Scientific Name` <- factor(tableprep_FWSlistingWP$`Scientific Name`, levels = tableprep_FWSlistingWP$`Scientific Name`[order(tableprep_FWSlistingWP$bisontotal)])
tableprep_FWSlistingWP <- tableprep_FWSlistingWP[order(tableprep_FWSlistingWP$bisontotal),] 


tf_table <- tableprep_FWSlistingWP %>%
  group_by(`Proposed FWS Decision Timeframe (Fiscal Year)`) %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# Create tables for each fiscal year
fy17 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2017),]
fy17 <- fy17[,-6]
fy18 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2018),]
fy18 <- fy18[,-6]
fy19 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2019),]
fy19 <- fy19[,-6]
fy20 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2020),]
fy20 <- fy20[,-6]
fy21 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2021),]
fy21 <- fy21[,-6]
fy22 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2022),]
fy22 <- fy22[,-6]
fy23 <- tableprep_FWSlistingWP[which(tableprep_FWSlistingWP$`Proposed FWS Decision Timeframe (Fiscal Year)` == 2023),]
fy23 <- fy23[,-6]

