library(jsonlite)
library(tidyverse)
library(mongolite)
library(dplyr)
library(kableExtra)
library(plyr)


# Pulls the SGCN National List from MongoDB
bis_sgcnsynthesis2 <- mongo(collection = "xxxxxxxxxxxxxxxxx", db = "xxxxxxxxxxxx", url = "xxxxxxxxxxxxxxxxxx")
mongo_sgcn2 <- bis_sgcnsynthesis2$find('{}', fields = '{"_id":1, "Common Name":1, "Taxonomic Group":1, "Match Method":1, "Taxonomy":1, "TESS":1, "NatureServe":1, "Source Data Summary":1, "ITIS":1, "WoRMS":1}')


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
FWSRegion3_list <- sgcn_natlist[grep(paste(toMatchFWSRegion1,collapse = "|"),sgcn_natlist$statelist2015),]


# Extract out the FWS listing status from the document structure coming back from Mongodb2.0
FWSRegion3_list$ListingStatus <- NA
FWSRegion3_list$ListingStatus2 <- NA
FWSRegion3_list$ListingStatus3 <- NA
FWSRegion3_list$ListingStatus4 <- NA
for (i in 1:nrow (FWSRegion3_list)) {
  if (!is.null(FWSRegion3_list[i,]$TESS$listingStatus[[1]])){
    tessrow <- as.data.frame(FWSRegion3_list[i,]$TESS$listingStatus, simplifyDataFrame=T)
    if (nrow(tessrow) == 1) {
      FWSRegion3_list[i,]$ListingStatus <- tessrow$STATUS[[1]]
    }
    if (nrow(tessrow) == 2){
    FWSRegion3_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
    FWSRegion3_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
    }
    if (nrow(tessrow) == 3){
      FWSRegion3_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      FWSRegion3_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      FWSRegion3_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
    }
    if (nrow(tessrow) == 4){
      FWSRegion3_list[i,]$ListingStatus <- tessrow$STATUS[[1]] 
      FWSRegion3_list[i,]$ListingStatus2 <- tessrow$STATUS[[2]]
      FWSRegion3_list[i,]$ListingStatus3 <- tessrow$STATUS[[3]]
      FWSRegion3_list[i,]$ListingStatus4 <- tessrow$STATUS[[4]]
    }
  } else {
    FWSRegion3_list[i,]$ListingStatus <- NA
  }
}

FWSRegion3_list$TESS <- NULL

# Swap higher priority listing status for lower priority ones
for (i in 1:nrow(FWSRegion3_list)) {
  if (!is.na(FWSRegion3_list[i,]$ListingStatus2) & FWSRegion3_list[i,]$ListingStatus2 == "Endangered") {
    FWSRegion3_list[i,]$ListingStatus <- paste(FWSRegion3_list[i,]$ListingStatus,FWSRegion3_list[i,]$ListingStatus2, sep = "")
    FWSRegion3_list[i,]$ListingStatus2 <- substr(FWSRegion3_list[i,]$ListingStatus,0,nchar(FWSRegion3_list[i,]$ListingStatus) - nchar(FWSRegion3_list[i,]$ListingStatus2))
    FWSRegion3_list[i,]$ListingStatus <- substr(FWSRegion3_list[i,]$ListingStatus,nchar(FWSRegion3_list[i,]$ListingStatus2) + 1, nchar(FWSRegion3_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(FWSRegion3_list)) {
  if (!is.na(FWSRegion3_list[i,]$ListingStatus3) & FWSRegion3_list[i,]$ListingStatus3 == "Endangered") {
    FWSRegion3_list[i,]$ListingStatus <- paste(FWSRegion3_list[i,]$ListingStatus,FWSRegion3_list[i,]$ListingStatus3, sep = "")
    FWSRegion3_list[i,]$ListingStatus3 <- substr(FWSRegion3_list[i,]$ListingStatus,0,nchar(FWSRegion3_list[i,]$ListingStatus) - nchar(FWSRegion3_list[i,]$ListingStatus3))
    FWSRegion3_list[i,]$ListingStatus <- substr(FWSRegion3_list[i,]$ListingStatus,nchar(FWSRegion3_list[i,]$ListingStatus3) + 1, nchar(FWSRegion3_list[i,]$ListingStatus))
  }
}

for (i in 1:nrow(FWSRegion3_list)) {
  if (!is.na(FWSRegion3_list[i,]$ListingStatus3) & FWSRegion3_list[i,]$ListingStatus3 == "Recovery") {
    FWSRegion3_list[i,]$ListingStatus <- paste(FWSRegion3_list[i,]$ListingStatus,FWSRegion3_list[i,]$ListingStatus3, sep = "")
    FWSRegion3_list[i,]$ListingStatus3 <- substr(FWSRegion3_list[i,]$ListingStatus,0,nchar(FWSRegion3_list[i,]$ListingStatus) - nchar(FWSRegion3_list[i,]$ListingStatus3))
    FWSRegion3_list[i,]$ListingStatus <- substr(FWSRegion3_list[i,]$ListingStatus,nchar(FWSRegion3_list[i,]$ListingStatus3) + 1, nchar(FWSRegion3_list[i,]$ListingStatus))
  }
}

# Now we want to grab what we need from the NatureServe rank data
FWSRegion3_list$NSGlobalDescription <- FWSRegion3_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$roundedRank$description
FWSRegion3_list$NSGlobalReviewDate <- FWSRegion3_list$NatureServe$conservationStatus$natureServeStatus$globalStatus$statusLastReviewed

# Not grabbing the NS state ranks for now. Try and come back to that later.
FWSRegion3_list$NatureServe <- NULL

# Populating the taxonomic hierarchy from the Taxonomy information from Mongodb
FWSRegion3_list$kingdom <- NA
FWSRegion3_list$phylum <- NA
FWSRegion3_list$class <- NA
FWSRegion3_list$order <- NA
FWSRegion3_list$family <- NA
FWSRegion3_list$genus <- NA
for (i in 1: nrow(FWSRegion3_list)) {
  if (!is.null(FWSRegion3_list$Taxonomy[[i]])) {
  taxonomy_row <- FWSRegion3_list[i,]$Taxonomy
  df <- ldply(taxonomy_row, data.frame)
  if (is.na(FWSRegion3_list[i,]$kingdom)) {
    FWSRegion3_list[i,]$kingdom <- df$name[which(df$rank == "Kingdom")]
    FWSRegion3_list[i,]$phylum <- df$name[which(df$rank == "Phylum" | df$rank == "Division")]
    if (nrow(df[which(df$rank == "Class"),]) >0) { 
      FWSRegion3_list[i,]$class <- df$name[which(df$rank == "Class")]
    } 
    if (nrow(df[which(df$rank == "Order"),]) >0) {
      FWSRegion3_list[i,]$order <- df$name[which(df$rank == "Order")]
    } 
    if (nrow(df[which(df$rank == "Family"),]) >0) {
      FWSRegion3_list[i,]$family <- df$name[which(df$rank == "Family")]
    } 
    if (nrow(df[which(df$rank == "Genus"),]) >0) {
      FWSRegion3_list[i,]$genus <- df$name[which(df$rank == "Genus")]
    }
    }
  }
}

# Grab taxonomic ranks so we can filter out any taxonomic identifications made at a level higher than species because they'll bring in too
# many records from BISON and result in duplication

FWSRegion3_list$rank <- NA
for (i in 1:nrow(FWSRegion3_list)) {
  if (is.na(FWSRegion3_list[i,]$rank)) {
    if (!is.null(FWSRegion3_list[i,]$ITIS[[1]])){
      itis <- as.data.frame(FWSRegion3_list[i,]$ITIS)
      FWSRegion3_list[i,]$rank <- itis[which(itis$usage == "valid" | itis$usage == "accepted"),]$rank
    }
  }
}

for (i in 1:nrow(FWSRegion3_list)){
  if (is.na(FWSRegion3_list[i,]$rank)){
    if(!is.null(FWSRegion3_list[i,]$WoRMS[[1]])){
      worms <- as.data.frame(FWSRegion3_list[i,]$WoRMS)
      if (!is.null(worms$rank)){
        if (!is.na(worms[1,]$rank) | !is.na(worms[2,]$rank)) {
          if (any(worms$status == "accepted")){
            FWSRegion3_list[i,]$rank <- worms[which(worms$status == "accepted" ),]$rank
          } else if(worms$status == "uncertain" | worms$status == "nomen dubium" & worms$status != "accepted") {
            FWSRegion3_list[i,]$rank <- worms$status
          }
        }
      }
    }
  }
}


FWSRegion3_list_speciesonly <- FWSRegion3_list[which(FWSRegion3_list$rank != "Family" & FWSRegion3_list$rank != "Genus" & FWSRegion3_list$rank != "Order" & FWSRegion3_list$rank != "Subclass" & FWSRegion3_list$rank != "Suborder"),]
FWSRegion3_list_speciesonly$ITIS <- NULL
FWSRegion3_list_speciesonly$WoRMS <- NULL


# Need to unload the package "plyr" here because it causes problems for summarizing the data below
detach(package:plyr)

### See what BISON has for these species
FWSRegion3_list_speciesonly$bisonQuery <- NA
for(i in 1:nrow(FWSRegion3_list_speciesonly)){
  if (is.na(FWSRegion3_list_speciesonly[i,]$bisonQuery)){
      FWSRegion3_list_speciesonly[i,]$bisonQuery <- paste0("https://data.usgs.gov/solr/occurrences/select?q=scientificName:(%22", (URLencode(FWSRegion3_list_speciesonly[i,]$ScientificName)))
    }
  }
FWSRegion3_list_speciesonly$bisonQuery <- paste0(FWSRegion3_list_speciesonly$bisonQuery, "%22)&facet.mincount=1&rows=0&facet=true&facet.missing=true&facet.limit=-1&wt=json&indent=true&facet.field=basisOfRecord")

FWSRegion3_list_speciesonly$bisontotal <- NA
for(i in 1:nrow(FWSRegion3_list_speciesonly)){
  if (is.na(FWSRegion3_list_speciesonly[i,]$bisontotal)){
    bisonrow <- fromJSON(FWSRegion3_list_speciesonly[i,]$bisonQuery)
    FWSRegion3_list_speciesonly[i,]$bisontotal <- bisonrow$response$numFound
  }
}

N <- nrow(FWSRegion3_list_speciesonly)
bisondata <- vector(mode="list", length=N)

df <- data.frame(X_id=NA, bisonQuery=NA, total=NA, literature=NA, fossil=NA, observation = NA, specimen=NA, unknown=NA )
df_total <- data.frame(X_id=NA, bisonQuery=NA, total=NA, literature=NA, fossil=NA, observation = NA, specimen=NA, unknown=NA )
for(i in 1:N){
  query  <- toString(FWSRegion3_list_speciesonly$bisonQuery[i])
  bisondata[[i]] <- fromJSON(query)
  xid = bisondata[[i]]$responseHeader$params$q
  bisondata[[i]] <- bisondata[[i]]$facet_counts$facet_fields$basisOfRecord
  dummy_df <- as.data.frame(t(matrix(unlist(bisondata[[i]]), nrow=length(bisondata[[i]])/2, byrow=T)), stringsAsFactors = F)
  colnames(dummy_df) = dummy_df[1, ]
  dummy_df <- dummy_df[complete.cases(dummy_df), ]
  # merge with df with all columns
  if(!is.null(dim(dummy_df))){
    merged <- bind_rows(df, dummy_df)
    merged[2,"X_id"] = xid 
    merged[2,"bisonQuery"] = query
  }
  else{ 
    dummy_df <- df
    dummy_df[1,"X_id"] = xid
    dummy_df[1,"bisonQuery"] = query
    merged <- bind_rows(df, dummy_df)
  }
  merged <- merged[c("X_id", "bisonQuery", "total", "literature", "fossil", "observation", "specimen", "unknown")]
  merged <- merged[-1, ]
  # stack bison query into one final df
  df_total <- rbind(df_total, merged)
  
}
# final df
df_total <- df_total[-1, ]  # first row is NA's
df_total$total <- NULL

# Merge df with original data
FWSRegion3_list_speciesonly <- merge(FWSRegion3_list_speciesonly, df_total, by = "bisonQuery", all.x = T)
FWSRegion3_list_speciesonly[,28:32] <- lapply(FWSRegion3_list_speciesonly[,28:32], function(x) as.numeric(x))

# Now that we have the data for each SGCN species as the type of data (basis of record), let's figure out how many records are available in each
# state for each species listed
FWSRegion3_list_speciesonly$bisonStateQuery <- NA
for(i in 1:nrow(FWSRegion3_list_speciesonly)){
  if (is.na(FWSRegion3_list_speciesonly[i,]$bisonStateQuery)){
    FWSRegion3_list_speciesonly[i,]$bisonStateQuery <- paste0("https://data.usgs.gov/solr/occurrences/select?q=scientificName:(%22", (URLencode(FWSRegion3_list_speciesonly[i,]$ScientificName)))
  }
}
FWSRegion3_list_speciesonly$bisonStateQuery <- paste0(FWSRegion3_list_speciesonly$bisonStateQuery, "%22)%20AND%20calculatedState:(%22Illinois%22%20%22Indiana%22%20%22Iowa%22%20%22Michigan%22%20%22Missouri%22%20%22Minnesota%22%20%22Ohio%22%20%22Wisconsin%22)&facet.mincount=1&rows=0&facet=true&facet.missing=true&facet.limit=-1&wt=json&indent=true&facet.field=calculatedState")

N <- nrow(FWSRegion3_list_speciesonly)
bisonStatedata <- vector(mode="list", length=N)

df <- data.frame(X_id=NA, bisonStateQuery=NA, Illinois=NA, Indiana=NA, Iowa=NA, Michigan=NA, Missouri=NA, Minnesota=NA, Ohio=NA, Wisconsin=NA )
df_total <- data.frame(X_id=NA, bisonStateQuery=NA, Illinois=NA, Indiana=NA, Iowa=NA, Michigan=NA, Missouri=NA, Minnesota=NA, Ohio=NA, Wisconsin=NA )
for(i in 1:N){
  query  <- toString(FWSRegion3_list_speciesonly$bisonStateQuery[i])
  bisondata[[i]] <- fromJSON(query)
  xid = bisondata[[i]]$responseHeader$params$q
  bisondata[[i]] <- bisondata[[i]]$facet_counts$facet_fields$calculatedState
  dummy_df <- as.data.frame(t(matrix(unlist(bisondata[[i]]), nrow=length(bisondata[[i]])/2, byrow=T)), stringsAsFactors = F)
  colnames(dummy_df) = dummy_df[1, ]
  dummy_df <- dummy_df[complete.cases(dummy_df), ]
  # merge with df with all columns
  if(!is.null(dim(dummy_df))){
    merged <- bind_rows(df, dummy_df)
    merged[2,"X_id"] = xid 
    merged[2,"bisonStateQuery"] = query
  }
  else{ 
    dummy_df <- df
    dummy_df[1,"X_id"] = xid
    dummy_df[1,"bisonStateQuery"] = query
    merged <- bind_rows(df, dummy_df)
  }
  merged <- merged[c("X_id", "bisonStateQuery", "Illinois", "Indiana", "Iowa", "Michigan", "Missouri", "Minnesota", "Ohio", "Wisconsin")]
  merged <- merged[-1, ]
  # stack bison query into one final df
  df_total <- rbind(df_total, merged)
  
}
# final df
df_total <- df_total[-1, ]  # first row is NA's

# Merge df with original data
FWSRegion3_list_speciesonly <- merge(FWSRegion3_list_speciesonly, df_total, by = "bisonStateQuery", all.x = T)
FWSRegion3_list_speciesonly[,35:42] <- lapply(FWSRegion3_list_speciesonly[,35:42], function(x) as.numeric(x))

# Group by class

r1 <- FWSRegion3_list_speciesonly %>%
  group_by(class) %>%
  summarize(Total = sum(bisontotal), Illinois = sum(Illinois, na.rm = T), Indiana = sum(Indiana, na.rm = T), 
            Iowa = sum(Iowa, na.rm = T), Michigan = sum(Michigan, na.rm = T), Missouri = sum(Missouri, na.rm = T), 
            Minnesota = sum(Minnesota, na.rm = T), Ohio = sum(Ohio, na.rm = T), Wisconsin = sum(Wisconsin, na.rm = T), Count = n())
r1$Total <- NA
for (i in 1:nrow(r1)){
  r1[i,]$Total <- (r1[i,]$Illinois + r1[i,]$Indiana + r1[i,]$Iowa + r1[i,]$Michigan + r1[i,]$Missouri + r1[i,]$Minnesota + r1[i,]$Ohio + r1[i,]$Wisconsin)
}
r1$class <- factor(r1$class, levels = r1$class[order(r1$Total)])
r1 <- r1[order(r1$Total),] 


# Move the count of the number of species within that class to the class field
r1$class <- paste(r1$class, r1$Count, sep = " (")
r1$class <- paste(r1$class, ")", sep = "")
r1 <- r1[,-11] #Removes "Count" column

# # Create a log version of the data to prep for a dot plot since Aves makes it impossible to see the other classes
# r_log <- r
# r_log$logTotal <- log10(r_log$FWSRegionTotal)
# r_log <- r_log[order(r_log$logTotal),]

ls_r1 <- FWSRegion3_list %>%
  group_by(class, ListingStatus) %>%
  summarize(count = n())
ls_r1 <- ls_r1[which(!is.na(ls_r1$ListingStatus)),]
ls_r1 <- ls_r1[which(ls_r1$ListingStatus == "Candidate" | ls_r1$ListingStatus == "Endangered" | 
                       ls_r1$ListingStatus == "Threatened" | 
                       ls_r1$ListingStatus == "Under Review in the Candidate or Petition Process"),]

ListingStatus2 <- FWSRegion3_list[which(!is.na(FWSRegion1_list$ListingStatus2)),]

EndangeredList <- FWSRegion3_list[which(FWSRegion1_list$ListingStatus == "Endangered"),]

# What can we say about the distribution of observation types at the class level?

o1 <- FWSRegion3_list_speciesonly %>%
  group_by(class) %>%
  summarise(Count = n(), Literature = sum(literature, na.rm = T), Observation = sum(observation, na.rm = T), 
            Specimen = sum(specimen, na.rm = T), Fossil = sum(fossil, na.rm = T), Unknown = sum(unknown, na.rm = T))
o1$class <- paste(o1$class, o1$Count, sep = " (")
o1$class <- paste(o1$class, ")", sep = "")
o1 <- o1[,-2] #Removes "Count" column
o1$class <- factor(o1$class, levels = o1$class[order(o1$Observation)])
o1 <- o1[order(o1$Observation),]
o1_gather <- gather(o1, key = "type", value = "Number of Occurrences", -class)






