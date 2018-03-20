#' BISON - SGCN Data Mashup Report for WSFR Region 3
#' Abby Benson  
#' March 20, 2018  


# Data for this notebook was brought together and managed using the R script BISON_SGCN_Mashup_FWSRegion3.R

#' ## General information about the data
#' The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in Fish
#' and Wildlife Service Wildlife and Sport Fish Restoration Program Region 3. SGCN lists were collated by USGS and matched to the Integrated 
#' Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
#' and the NatureServe database. Species that were not found in ITIS or WoRMS are not considered part of the SGCN National List but are still 
#' included in this analysis.

#+ Load libraries, include=F
library(scales)
library(tidyverse)
library(ggplot2)
library(knitr)
library(gridExtra)
library(kableExtra)
library(gtable)
library(reshape2)
library(plotly)

#+ SGCN_number, include=FALSE
#' ### Number of Species of Greatest Conservation Need in Fish and Wildlife Service Region 1:
nrow(FWSRegion3_list)

#+ FWSListing_number, include=FALSE
#' ### Number of SGCN that are considered Endangered, Threatened, Candidate, or Under Review by the Fish and Wildlife Service:
nrow(FWSRegion3_list[which(FWSRegion3_list$ListingStatus == "Candidate" | FWSRegion3_list$ListingStatus == "Endangered" | 
                             FWSRegion3_list$ListingStatus == "Threatened" | 
                             FWSRegion3_list$ListingStatus == "Under Review in the Candidate or Petition Process"),])



lsp1 <- ggplot(ls_r1, aes(x=count, y=class)) + geom_point(shape=1)

# Divide by levels of FWS listing status, in the vertical direction
lsp1 + facet_grid(ListingStatus ~ .)

#+ BISON_data, include = TRUE
#' ### BISON Occurrence Data Available for these SGCN grouped by the Class level of the taxonomic hierarchy
#' First we'll examine what the breakdown looks like for the types of observations for these species for all observations from all locations.
#' But as you can see, it's difficult to discern what's available for anything but birds
op1 <- ggplot(o1_gather, aes(class, `Number of Occurrences` ))
op1 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") 

#' Therefore we take birds out and see what is available for the rest of the classes.
o1_1_gather <- o1_gather[which(o1_gather$class != "Aves (152)"),]
op2 <- ggplot(o1_1_gather, aes(class, `Number of Occurrences` ))
op2 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") 


#' Next we'll see how this breaks down for the region of interest- FWS WSFR Region 3
p1 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="BISON occurrences of SGCN", 
       subtitle="FWS WSFR Region 3", 
       caption="") +  
  theme(axis.title.x = element_blank()) +
  ylim(0,10000) +
  coord_flip()
p1 <- ggplotGrob(p1)

p2 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) + 
  labs(title="", 
       subtitle="", 
       caption="") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylim(10001,100000) +
  coord_flip()
p2 <- ggplotGrob(p2)

p3 <- ggplot(r1, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +
  labs(title="", 
       subtitle="", 
       caption="source: SGCN") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylim(100001,9300000) + 
  coord_flip()
p3 <- ggplotGrob(p3)

# grid.arrange(p1, p2, p3, ncol=3)
g <- cbind(p1, p2, p3, size = "first")
grid::grid.draw(g)

#+ Stacked bar chart, include = FALSE
#' Stack bar chart showing the proportion of records that are in each state by class
r1_gather <- gather(r1, key = "state", value = "occCount", -class)
r1_gather <- r1_gather[which(r1_gather$state != "classtotal" & r1_gather$state != "Total"),]
r1_1_gather <- r1_gather[which(r1_gather$class != "Aves (152)"),]
r1_2_gather <- r1_gather[which(r1_gather$class == "Aves (152)"),]
g1 <- ggplot(r1_1_gather, aes(class, occCount))
g1 + geom_bar(aes(fill = state), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") + ylim(0,100000) #doesn't show Aves

#knitr::spin('C:/Users/albenson/Documents/SWAPs/DataAnalysis/BISON_SGCN_Mashup/BISON_SGCN_Mashup_Notebook_FWSR3.R')