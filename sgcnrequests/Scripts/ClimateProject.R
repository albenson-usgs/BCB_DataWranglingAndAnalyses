library(jsonlite)
library(tidyverse)
library(stringr)

nl <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_nationallist")
nl_data <- as_tibble(nl$features$properties)

### Subset the data to just the national lists species for a particular region
toMatchClimProject <- c("Montana","Wyoming","Colorado","North Dakota","South Dakota","Nebraska","Kansas")
climProject_natlist <- nl_data[grep(paste(toMatchClimProject,collapse = "|"),nl_data$statelist_2015),]
climProject_natlist_simple <- climProject_natlist[c(-2:-5,-9)]

### FWS Listing Status
fwsdata <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_search%20WHERE%20tess-%3E%3E%27result%27%20=%20%27true%27")
fws_data <- as_tibble(fwsdata$features$properties)
fws_data <- fws_data[c("scientificname","tess")]


# Grabbing the STATUS value from JSON file

tess_row <- list()
statuses <- c()
for (i in 1: nrow(fws_data)) {
  tess_row[[i]] <- fromJSON(fws_data$tess[[i]], simplifyDataFrame=T) 
  statuses[i] <- tess_row[[i]]$listingStatus$STATUS
}

### My old way of grabbing the abbreviated status value, not ideal
# fws_data <- fws_data %>%
#   mutate(tess = strsplit(tess, ",")) %>%
#   unnest(tess) %>%
#   group_by(scientificname) %>%
#   mutate(row = row_number()) %>%
#   spread(row, tess)
# 
# fws_data$listingStatus <- fws_data$`5`


### Combine FWS and national list subset
cp_fws <- merge(climProject_natlist, fws_data, by = "scientificname")
cp_fws_simple <- cp_fws[c(-2:-5, -7:-9)]
