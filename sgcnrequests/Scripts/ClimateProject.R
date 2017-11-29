library(jsonlite)
library(plyr)
library(tidyverse)
library(stringr)


nl <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_nationallist")
nl_data <- as_tibble(nl$features$properties)

### FWS Listing Status
fwsdata <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_search%20WHERE%20tess-%3E%3E%27result%27%20=%20%27true%27")
fws_data <- as_tibble(fwsdata$features$properties)
fws_data <- fws_data[c("scientificname","tess")]


# Grabbing the STATUS value from JSON file

tess_row <- list()
fws_data$listingStatus <- NA
for (i in 1: nrow(fws_data)) {
  tess_row[[i]] <- fromJSON(fws_data$tess[[i]], simplifyDataFrame=T) 
  fws_data$listingStatus[i] <- tess_row[[i]]$listingStatus$STATUS
}

nl_w_fws <- merge(nl_data, fws_data, by="scientificname", all.x = TRUE)


### Combine FWS and national list subset
cp_fws <- merge(climProject_natlist, fws_data, by = "scientificname")
cp_fws_simple <- cp_fws[c(-2:-5, -7:-9)]

# Can I easily pull in higher taxonomy to get a better picture of taxa group breakdown?
itisdata <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_search%20WHERE%20itis-%3E%3E%27MatchMethod%27%20!=%20%27Not%20Matched%27")
itis_data <- as_tibble(itisdata$features$properties)
itis_data <- itis_data[c("scientificname","itis")]

itis_row <- list()
itis_data$kingdom <- NA
itis_data$phylum <- NA
itis_data$class <- NA
itis_data$order <- NA
itis_data$family <- NA
itis_data$genus <- NA
for (i in 1: nrow(itis_data)) {
  if (is.na(itis_data[i,4])) {
    itis_row <- fromJSON(itis_data$itis[[i]], simplifyDataFrame=T) 
    df <- ldply(itis_row, data.frame)
    itis_data[i,]$kingdom <- df$name[which(df$rank == "Kingdom")]
    itis_data[i,]$phylum <- df$name[which(df$rank == "Phylum" | df$rank == "Division")]
    if (nrow(df[which(df$rank == "Class"),]) >0) { 
      itis_data[i,]$class <- df$name[which(df$rank == "Class")]
    } 
    if (nrow(df[which(df$rank == "Order"),]) >0) {
      itis_data[i,]$order <- df$name[which(df$rank == "Order")]
    } 
    if (nrow(df[which(df$rank == "Family"),]) >0) {
      itis_data[i,]$family <- df$name[which(df$rank == "Family")]
    } 
    if (nrow(df[which(df$rank == "Genus"),]) >0) {
      itis_data[i,]$genus <- df$name[which(df$rank == "Genus")]
    }
  }
}

## Now need to do the same for WoRMS data
wormsdata <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_search%20WHERE%20worms-%3E%3E%27MatchMethod%27%20!=%20%27Not%20Matched%27%20AND%20itis-%3E%3E%27MatchMethod%27=%27Not%20Matched%27")
worms_data <- as_tibble(wormsdata$features$properties)
worms_data <- worms_data[c("scientificname","worms")]

### Same issue with this for loop as with the ITIS one.
worms_row <- list()
worms_data$kingdom <- NA
worms_data$phylum <- NA
worms_data$class <- NA
worms_data$order <- NA
worms_data$family <- NA
worms_data$genus <- NA
for (i in 1: nrow(worms_data)) {
  if (is.na(worms_data[i,3])) {
    worms_row[[i]] <- fromJSON(worms_data$worms[[i]], simplifyDataFrame=T) 
    worms_data$kingdom[i] <- worms_row[[i]]$kingdom
    worms_data$phylum[i] <- worms_row[[i]]$phylum
    if (!is.null(worms_row[[i]]$class)) {
      worms_data$class[i] <- worms_row[[i]]$class
    }
    if (!is.null(worms_row[[i]]$order)) {
      worms_data$order[i] <- worms_row[[i]]$order
    }
    if (!is.null(worms_row[[i]]$family)) {
      worms_data$family[i] <- worms_row[[i]]$family
    }
    if (!is.null(worms_row[[i]]$genus)) {
      worms_data$genus[i] <- worms_row[[i]]$genus
    }
  }
}

## Merge ITIS and WoRMS data together
worms_data$worms <- NULL
itis_data$itis <- NULL
hierarchydata <- rbind(worms_data, itis_data)

## Merge hierarchy data to dataframe with FWS listing status
nl_fws_itis_worms <- merge(nl_w_fws, hierarchydata, by = "scientificname", all.x = T)

### Subset the data to just the national lists species for a particular region
toMatchClimProject <- c("Montana","Wyoming","Colorado","North Dakota","South Dakota","Nebraska","Kansas")
climProject_natlist <- nl_fws_itis_worms[grep(paste(toMatchClimProject,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
climProject_natlist_simple <- climProject_natlist[c("scientificname", "commonname", "matchmethod","scientificnames_submitted",
                                                    "statelist_2005","statelist_2015","listingStatus","kingdom","phylum","class")]

### Matrix view with pertinent states along the top and scientific names along the side
climProject_natlist_matrix <- climProject_natlist[c("scientificname","statelist_2015")]
climProject_natlist_matrix$Montana <- 0
climProject_natlist_matrix$Wyoming <- 0
climProject_natlist_matrix$Colorado <- 0
climProject_natlist_matrix$`North Dakota` <- 0
climProject_natlist_matrix$`South Dakota` <- 0
climProject_natlist_matrix$Nebraska <- 0
climProject_natlist_matrix$Kansas <- 0

for (i in 1: nrow(climProject_natlist_matrix)) {
  clim_row <- climProject_natlist_matrix[i,]$statelist_2015
  clim_row <- unlist(strsplit(clim_row, split = ","))
  if (colnames(climProject_natlist_matrix[3]) %in% clim_row) {
    climProject_natlist_matrix[i,]$Montana <- 1
  }
  if (colnames(climProject_natlist_matrix[4]) %in% clim_row) {
    climProject_natlist_matrix[i,]$Wyoming <- 1
  }
  if (colnames(climProject_natlist_matrix[5]) %in% clim_row) {
    climProject_natlist_matrix[i,]$Colorado <- 1
  }
  if (colnames(climProject_natlist_matrix[6]) %in% clim_row) {
    climProject_natlist_matrix[i,]$`North Dakota` <- 1
  }
  if (colnames(climProject_natlist_matrix[7]) %in% clim_row) {
    climProject_natlist_matrix[i,]$`South Dakota` <- 1
  }
  if (colnames(climProject_natlist_matrix[8]) %in% clim_row) {
    climProject_natlist_matrix[i,]$Nebraska <- 1
  }
  if (colnames(climProject_natlist_matrix[9]) %in% clim_row) {
    climProject_natlist_matrix[i,]$Kansas <- 1
  }
}

write.csv(climProject_natlist_matrix, file = "SGCN_NationalList_ClimateInterestMatrix_20171129.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(climProject_natlist_simple, file = "SGCN_NationalList_ClimateInterestNorthCentralStates_20171129.csv", fileEncoding = "UTF-8", 
          row.names = F)

 