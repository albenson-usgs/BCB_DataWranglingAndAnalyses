library(readr)
library(jsonlite)

# Bring in species list Meryl Mims supplied via email on Nov 20, 2017 
NFVAP_SpeciesList <- read_csv("~/BCB/MultipleSpeciesVulnerabilityNA/SpeciesLists_Nov2017_forSGCNquery.csv")

# Bring in Species of Greatest Conservation Need national list data
nl <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_nationallist")
nl_data <- as_tibble(nl$features$properties)

# Rename scientific name column to match SGCN national list
NFVAP_SpeciesList$scientificname <- NFVAP_SpeciesList$`Scientific name`
NFVAP_SpeciesList$`Scientific name` <- NULL

# Merge Meryl's list with the SGCN national list, condense down to key information fields
NFVAP_SpeciesList <- merge(NFVAP_SpeciesList, nl_data, all.x = T, by = "scientificname")
NFVAP_SpeciesList <- NFVAP_SpeciesList[c("scientificname", "Family", "Common name", "acceptedauthorityapi", "scientificnames_submitted", 
                                         "statelist_2005", "statelist_2015")]

write.csv(NFVAP_SpeciesList, file = "NFVAP_SpeciesList_SGCNmatches_Nov2017.csv", fileEncoding = "UTF-8", row.names = F)