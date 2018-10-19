library(jsonlite)
library(tidyverse)
library(stringr)
library(plyr)
library(taxize)

# Need BuildGC2Tables.R in the DataChecking R Project to be run first
# The sgcn_search table should give us the information we need just need to limit it to SEAFWA

seafwa <- c("Florida", "Georgia", "South Carolina", "North Carolina", "Kentucky", "Tennessee", "Alabama", "Mississippi",  "Arkansas", 
            "Louisiana", "Virginia", "West Virginia", "Missouri", "Oklahoma", "Texas")
seafwa_natlist <- sgcn_search[grep(paste(seafwa,collapse = "|"),sgcn_search$statelist_2015),]
seafwa_natlist_wFWSListingStatus <- seafwa_natlist[which(!is.na(seafwa_natlist$ListingStatus)),]
write.csv(seafwa_natlist_wFWSListingStatus, file = "seafwa_reglist_wFWSListStat.csv", fileEncoding = "UTF-8", row.names = F)

#_________________ Old Method using GC2 below__________________
nl <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_nationallist")
nl_data <- as_tibble(nl$features$properties)

### FWS Listing Status
fwsdata <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_search%20WHERE%20tess-%3E%3E%27result%27%20=%20%27true%27")
fws_data <- as_tibble(fwsdata$features$properties)
fws_data <- fws_data[c("scientificname","tess")]

tess_row <- list()
fws_data$listingStatus <- NA
for (i in 1: nrow(fws_data)) {
  tess_row[[i]] <- fromJSON(fws_data$tess[[i]], simplifyDataFrame=T) 
  fws_data$listingStatus[i] <- tess_row[[i]]$listingStatus$STATUS
}

nl_w_fws <- merge(nl_data, fws_data, by="scientificname", all.x = TRUE)

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

# Find missing taxonomy
missingtax <- nl_fws_itis_worms[which(is.na(nl_fws_itis_worms$kingdom) & nl_fws_itis_worms$matchmethod != "Legacy Match"),]
lut_itis <- missingtax[c("scientificname")]

lut_itis$id <- ""
getid <- function(x) {
  for (i in 1:nrow(x)) {
    if (nchar(x[i,ncol(x)]) == 0) {
      lut_itis[i,]$id <<- get_tsn(x[i,]$scientificname, searchtype = "scientific", ask = TRUE, verbose = TRUE)
    }}
}
getid(lut_itis)

lut_itis <- lut_itis[which(!is.na(lut_itis$id)),]

lut_itis$kingdom <- ""
lut_itis$phylum <- ""
lut_itis$class <- ""
lut_itis$order <- ""
lut_itis$family <- ""
lut_itis$genus <- ""

for (i in 1:nrow(lut_itis)) {
  if (nchar(lut_itis[667,4] < 1))
    ls <- classification(lut_itis[i,]$id, db = "itis")
  df <- ldply(ls, data.frame)
  lut_itis[i,]$kingdom <- df$name[which(df$rank == "kingdom")]
  lut_itis[i,]$phylum <- df$name[which(df$rank == "phylum" | df$rank == "division")]
  if (nrow(df[which(df$rank == "class"),]) >0) { 
    lut_itis[i,]$class <- df$name[which(df$rank == "class")]
  } 
  if (nrow(df[which(df$rank == "order"),]) >0) {
    lut_itis[i,]$order <- df$name[which(df$rank == "order")]
  } 
  if (nrow(df[which(df$rank == "family"),]) >0) {
    lut_itis[i,]$family <- df$name[which(df$rank == "family")]
  } 
  if (nrow(df[which(df$rank == "genus"),]) >0) {
    lut_itis[i,]$genus <- df$name[which(df$rank == "genus")]
  }
}

lut_itis$id <- NULL
hierarchydata <- rbind(hierarchydata, lut_itis)
nl_fws_itis_worms <- merge(nl_w_fws, hierarchydata, by = "scientificname", all.x = T)
nl_fws_itis_worms_2015 <- nl_fws_itis_worms[which(nl_fws_itis_worms$sgcn2015 > 0),]




# Break out by FWS WSFR Regions https://wsfrprograms.fws.gov/Subpages/ContactUs/ContactUs.htm

wsfr_region1 <- c("Idaho", "Oregon", "Washington" ,"Hawaii", "Northern Mariana Islands") #eventually add in Guam, American Samoa, Puerto Rico, and U.S. Virgin Islands
wsfr_region2 <- c("Arizona", "New Mexico", "Oklahoma", "Texas")
wsfr_region3 <- c("Illinois", "Indiana", "Iowa", "Michigan", "Minnesota", "Missouri", "Ohio", "Wisconsin")
wsfr_region4 <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North Carolina", 
                  "South Carolina", "Tennessee") #eventually add in Puerto Rico and U.S. Virgin Islands
wsfr_region5 <- c("Connecticut", "Delaware", "District of Columbia", "Maine", "Maryland", "Massachusetts", "New Hampshire", 
                  "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "Virginia", "West Virginia")
wsfr_region6 <- c("Colorado", "Idaho", "Kansas", "Montana", "Nebraska", "North Dakota", "South Dakota", "Utah", "Wyoming") #Idaho overlaps
wsfr_region7 <- c("Alaska")
wsfr_region8 <- c("California", "Nevada")
wsfr1_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region1,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr1_natlist_simple <- wsfr1_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region1_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr2_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region2,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr2_natlist_simple <- wsfr2_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region2_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr3_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region3,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr3_natlist_simple <- wsfr3_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region3_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr4_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region4,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr4_natlist_simple <- wsfr4_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region4_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr5_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region5,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr5_natlist_simple <- wsfr5_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region5_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr6_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region6,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr6_natlist_simple <- wsfr6_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region6_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr7_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region7,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr7_natlist_simple <- wsfr7_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region7_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)
wsfr8_natlist <- nl_fws_itis_worms[grep(paste(wsfr_region8,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wsfr8_natlist_simple <- wsfr8_natlist[c(-2:-5,-15)]
write.csv(wsfr1_natlist_simple, file = "wsfr_region8_NationaListSubset.csv", fileEncoding = "UTF-8", row.names = FALSE)

# Break out by AFWA regions
neafwa <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Connecticut", "Rhode Island", "New York", "Pennsylvania", "New Jersey", 
            "Delaware", "Maryland", "Virginia", "West Virginia", "District of Columbia")
seafwa <- c("Florida", "Georgia", "South Carolina", "North Carolina", "Kentucky", "Tennessee", "Alabama", "Mississippi",  "Arkansas", 
            "Louisiana", "Virginia", "West Virginia", "Missouri", "Oklahoma", "Texas")
mafwa <- c("Ohio", "Indiana", "Illinois", "Michigan", "Minnesota", "Wisconsin", "Iowa", "Missouri", "Kansas", "Kentucky", "Nebraska", 
           "North Dakota", "South Dakota")
wafwa <- c("Washington", "Oregon", "California", "Idaho", "Montana", "Nevada", "Arizona", "Utah", "Wyoming", "Colorado", "New Mexico", 
           "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota", "North Dakota")
neafwa_natlist <- nl_fws_itis_worms[grep(paste(neafwa,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
neafwa_natlist_simple <- neafwa_natlist[c(-2:-5,-15)]
write.csv(neafwa_natlist_simple, file = "neafwa_NationalListSubset.csv", fileEncoding = "UTF-8", row.names = F)
seafwa_natlist <- nl_fws_itis_worms[grep(paste(seafwa,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
seafwa_natlist_simple <- seafwa_natlist[c(-2:-5,-15)]
write.csv(seafwa_natlist_simple, file = "seafwa_NationalListSubset.csv", fileEncoding = "UTF-8", row.names = F)
mafwa_natlist <- nl_fws_itis_worms[grep(paste(mafwa,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
mafwa_natlist_simple <- mafwa_natlist[c(-2:-5,-15)]
write.csv(mafwa_natlist_simple, file = "mafwa_NationalListSubset.csv", fileEncoding = "UTF-8", row.names = F)
wafwa_natlist <- nl_fws_itis_worms[grep(paste(wafwa,collapse = "|"),nl_fws_itis_worms$statelist_2015),]
wafwa_natlist_simple <- wafwa_natlist[c(-2:-5,-15)]
write.csv(wafwa_natlist_simple, file = "wafwa_NationalListSubset.csv", fileEncoding = "UTF-8", row.names = F)


# Build a table broken down by taxa group by species only and subspecies only for each year

wildlifespecies <- nl_fws_itis_worms_2015[which(nl_fws_itis_worms_2015$taxonomicrank == "Species" & nl_fws_itis_worms_2015$kingdom == "Animalia"),]
wildlife_subspecies <- nl_fws_itis_worms_2015[which(nl_fws_itis_worms_2015$taxonomicrank == "Subspecies" & nl_fws_itis_worms_2015$kingdom == 
                                                      "Animalia"),]
plantspecies <- nl_fws_itis_worms_2015[which(nl_fws_itis_worms_2015$kingdom == "Plantae"),]


# Build a table that would count how many have a different listing status
fws_table <- nl_fws_itis_worms_2015 %>%
  group_by(class, listingStatus) %>%
  dplyr::summarize(count = n()) %>%
  tidyr::spread(listingStatus, count)
write.csv(fws_table, file = "NationalList_ListingStatusbyClass_2015.csv", fileEncoding = "UTF-8", row.names = F)

nl_fws_itis_worms_2015_simple <- nl_fws_itis_worms_2015[c("scientificname","commonname","taxonomicrank","matchmethod",
                                                          "scientificnames_submitted","statelist_2005","statelist_2015","listingStatus",
                                                          "kingdom","phylum","class","order","family")]

write.csv(nl_fws_itis_worms_2015_simple, file = "SGCN_NationalList2015_TaxonomyListingStatus_20171130.csv", fileEncoding = "UTF-8", row.names = F)

# How many states have at least one plant on their list?
subset <- nl_fws_itis_worms_2015[which(nl_fws_itis_worms_2015$kingdom == "Plantae"),]
subset <- subset[c("statelist_2015")]
subset <- unlist(strsplit(subset$statelist_2015, split = ","))
unique(subset)




