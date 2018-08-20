# Using the OBIS v3 API to build a biogeographic analysis package for v1.1 of the National Biogeographic Map
# Abby Benson 2018-08-09

library(jsonlite)
library(tidyverse)
library(dplyr)

# First need to pull in the US EEZ area ids
USAreaIdsquery <- "http://api.iobis.org/v3/area"
OBISAreas <- fromJSON(USAreaIdsquery)
USAreasIDs <- ifelse(OBISAreas$results$country == "United States", paste(OBISAreas$results$id), NA)
USAreasIDs <- USAreasIDs[!is.na(USAreasIDs)]
USAreasIDs <- as.data.frame(USAreasIDs)
USAreasIDs$name <- "" #How to bring in name?
# for (i in 1:nrow(USAreasIDs)){
#   when(USAreasIDs[i,]$USAreasIDs == OBISAreas$results$id){
#     USAreasIDs[i,]$name <- OBISAreas$results$name 
#   }
# }

# Next need to build the queries for those particular OBIS Area IDs
USAreasIDs$query <- ""
for (i in 1:nrow(USAreasIDs)){
  USAreasIDs[i,]$query <- paste0("http://api.iobis.org/v3/statistics/composition/class?areaid=", USAreasIDs[i,]$USAreasIDs)
}

# Then need to grab the data for those ids and create the figure
data <- fromJSON(USAreasIDs$query[1])
Area267data <- do.call(rbind.data.frame, data)
for(i in 1:nrow(USAreasIDs)){
  data <- fromJSON(USAreasIDs$query[i])
  df <- do.call(rbind.data.frame, data)
  
}

# Create class column, move the count of the number of species within that class to the class field
Area267data <- rownames_to_column(Area267data, var="class")
Area267data$class <- paste(Area267data$class, Area267data$species, sep = " (")
Area267data$class <- paste(Area267data$class, ")", sep = "")
Area267data <- Area267data[,-3] #Removes "species" column
Area267data$class <- factor(Area267data$class, levels = Area267data$class[order(Area267data$records)])

# Create plot of the data
p1 <- ggplot(Area267data, aes(x=class, y=records)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(records), 
                   yend=max(records)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Observations by class for OBIS Area 267",
       caption="") +  
  theme(axis.title.x = element_blank()) +
  coord_flip()
p1 <- ggplotGrob(p1)
grid::grid.draw(p1)
