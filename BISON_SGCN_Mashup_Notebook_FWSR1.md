BISON - SGCN Data Mashup Report  
Abby Benson  
January 17, 2018  


```r
# Data for this notebook was brought together and managed using the R script BISON_SGCN_Mashup_FWSRegion1.R
```

## General information about the data
The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in Fish
and Wildlife Service Wildlife and Sport Fish Restoration Program Region 1. SGCN lists were collated by USGS and matched to the Integrated 
Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
and the NatureServe database. Species that were not found in ITIS or WoRMS are not considered part of the SGCN National List but are still 
included in this analysis.




### Number of Species of Greatest Conservation Need in Fish and Wildlife Service Region 1:


```r
nrow(FWSRegion1_list)
```

```
## [1] 1858
```


### Number of SGCN that are considered Endangered, Threatened, Candidate, or Under Review by the Fish and Wildlife Service:


```r
nrow(FWSRegion1_list[which(FWSRegion1_list$ListingStatus == "Candidate" | FWSRegion1_list$ListingStatus == "Endangered" | 
                                FWSRegion1_list$ListingStatus == "Threatened" | 
                                FWSRegion1_list$ListingStatus == "Under Review in the Candidate or Petition Process"),])
```

```
## [1] 528
```

```r
lsp1 <- ggplot(ls_r1, aes(x=count, y=class)) + geom_point(shape=1)

# Divide by levels of FWS listing status, in the vertical direction
lsp1 + facet_grid(ListingStatus ~ .)
```

![plot of chunk unnamed-chunk-3](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/BISON-SGCN-FWSRegion1-ListStat.png)


### BISON Occurrence Data Available for these SGCN grouped by the Class level of the taxonomic hierarchy
First we'll examine what the breakdown looks like for the types of observations for these species for all observations from all locations.


```r
o1_1_gather <- o1_gather[which(o1_gather$class != "Aves (165)"),]
o1_2_gather <- o1_gather[which(o1_gather$class == "Aves (165)"),]
op1 <- ggplot(o1_1_gather, aes(class, Count))
op1 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") 
```

![plot of chunk unnamed-chunk-4](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/BISON-SGCN-FWSRegion1-ObsType.png)

```r
# op2 <- ggplot(o1_2_gather, aes(class, Count))
# op2 + geom_bar(aes(fill = type), position = position_stack(reverse = TRUE), stat = "identity") +
#   coord_flip() +
#   theme(legend.position = "top") 
```

Next we'll see how this breaks down for the region of interest- FWS WSFR Region 1


```r
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
       subtitle="FWS WSFR Region 1", 
       caption="") +  
  theme(axis.title.x = element_blank()) +
  ylim(0,10000) +
  coord_flip()
p1 <- ggplotGrob(p1)
```

```
## Warning: Removed 4 rows containing missing values (geom_point).

## Warning: Removed 4 rows containing missing values (geom_point).
```

```
## Warning: Removed 40 rows containing missing values (geom_segment).
```

```r
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
```

```
## Warning: Removed 37 rows containing missing values (geom_point).
```

```
## Warning: Removed 37 rows containing missing values (geom_point).
```

```
## Warning: Removed 40 rows containing missing values (geom_segment).
```

```r
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
  ylim(100001,10000000) + 
  coord_flip()
p3 <- ggplotGrob(p3)
```

```
## Warning: Removed 39 rows containing missing values (geom_point).
```

```
## Warning: Removed 39 rows containing missing values (geom_point).
```

```
## Warning: Removed 40 rows containing missing values (geom_segment).
```

```r
# grid.arrange(p1, p2, p3, ncol=3)
g <- cbind(p1, p2, p3, size = "first")
grid::grid.draw(g)
```

![plot of chunk unnamed-chunk-5](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/BISON-SGCN-FWSRegion1-chunk-4-1.png)


Two-way table shows the counts of observations for different taxonomic classes of the Species of Greatest Conservation Need in FWS WSFR
Region 3. 


```r
r1_twoway %>% 
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|           class            | Idaho  | Oregon | Washington | Hawaii | Guam | AmericanSamoa | VirginIslands | PuertoRico | NMariananIslands |  Total  |
|:--------------------------:|:------:|:------:|:----------:|:------:|:----:|:-------------:|:-------------:|:----------:|:----------------:|:-------:|
|         Ascidiacea         |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
| Chlorophyta incertae sedis |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
|         Clitellata         |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
|      Conjugatophyceae      |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
|       Cryptophyceae        |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
|        Cyanophyceae        |   0    |   0    |     0      |   0    |  0   |       0       |       0       |     0      |        0         |    0    |
|        Gymnolaemata        |   0    |   0    |     0      |   1    |  0   |       0       |       0       |     0      |        0         |    1    |
|           Anopla           |   0    |   0    |     0      |   3    |  0   |       0       |       0       |     0      |        0         |    3    |
|         Diplopoda          |   0    |   0    |     1      |   0    |  0   |       0       |       0       |     2      |        0         |    3    |
|       Lycopodiopsida       |   0    |   0    |     0      |   8    |  0   |       0       |       0       |     0      |        0         |    8    |
|        Branchiopoda        |   0    |   11   |     0      |   0    |  0   |       0       |       0       |     0      |        0         |   11    |
|       Trepaxonemata        |   0    |   0    |     0      |   2    |  0   |      11       |       0       |     0      |        0         |   13    |
|         Polychaeta         |   0    |   0    |     0      |   16   |  0   |       0       |       0       |     0      |        0         |   16    |
|         Chilopoda          |   4    |   8    |     3      |   0    |  0   |       4       |       0       |     0      |        0         |   19    |
|       Holothuroidea        |   0    |   0    |     0      |   18   |  0   |       1       |       0       |     0      |        1         |   20    |
|        Ulvophyceae         |   0    |   0    |     0      |   23   |  0   |       0       |       0       |     0      |        0         |   23    |
|         Bryopsida          |   0    |   4    |     17     |   7    |  0   |       0       |       0       |     0      |        0         |   28    |
|     Bacillariophyceae      |   0    |   19   |     16     |   17   |  0   |       0       |       0       |     0      |        0         |   52    |
|         Collembola         |   9    |   13   |     30     |   11   |  0   |       0       |       0       |     0      |        0         |   63    |
|         Echinoidea         |   0    |   9    |     11     |   41   |  0   |       4       |       0       |     0      |        0         |   65    |
|       Chondrichthyes       |   0    |   0    |     70     |   8    |  0   |       0       |       0       |     0      |        0         |   78    |
|        Cephalopoda         |   0    |   0    |     7      |   97   |  4   |       4       |       0       |     0      |        2         |   114   |
|        Chondrostei         |  103   |   28   |     3      |   0    |  0   |       0       |       0       |     0      |        0         |   134   |
|         Asteroidea         |   0    |   38   |    152     |   43   |  0   |       0       |       0       |     0      |        0         |   233   |
|          Bivalvia          |   23   |   66   |     98     |   68   |  15  |      28       |       0       |     0      |        12        |   310   |
|     Cephalaspidomorphi     |   0    |  292   |     50     |   0    |  0   |       0       |       0       |     0      |        0         |   342   |
|        Phaeophyceae        |   0    |   7    |     31     |  405   |  0   |       0       |       0       |     0      |        0         |   443   |
|        Malacostraca        |   5    |   27   |     55     |  309   |  4   |      42       |       0       |     0      |        20        |   462   |
|         Arachnida          |  249   |  152   |    337     |   10   |  0   |       0       |       0       |     0      |        0         |   748   |
|          Anthozoa          |   0    |   2    |     1      |  649   |  75  |      164      |       0       |     14     |        7         |   912   |
|      Florideophyceae       |   0    |   0    |     3      |  1200  |  0   |      11       |       0       |     0      |        13        |  1227   |
|          Reptilia          |  352   |  287   |    799     |   46   |  0   |      49       |       0       |     11     |       107        |  1651   |
|       Polypodiopsida       |   0    |   36   |     5      |  1752  |  1   |       1       |       0       |     3      |        0         |  1798   |
|         Gastropoda         |  244   |  353   |    109     |  2016  |  56  |      28       |       1       |     7      |        28        |  2842   |
|          Insecta           |  1223  |  1757  |    2291    |   60   |  0   |       0       |       0       |    234     |        0         |  5565   |
|          Mammalia          |  493   |  2150  |    2588    |  342   |  0   |       9       |       7       |     28     |        16        |  5633   |
|          Amphibia          |  601   |  6345  |    4104    |   0    |  0   |       0       |       0       |     0      |        0         |  11050  |
|       Magnoliopsida        |  153   |  1420  |    395     | 17366  |  5   |      12       |      14       |    209     |        22        |  19596  |
|         Teleostei          |  5010  |  6358  |   14036    |  1241  |  7   |      37       |       0       |     3      |        6         |  26698  |
|            Aves            | 145676 | 475605 |   673533   | 952470 | 565  |      305      |     16868     |   15207    |       4619       | 2284848 |


Maybe as a stacked bar chart instead?


```r
r1_gather <- gather(r1, key = "state", value = "occCount", -class)
r1_gather <- r1_gather[which(r1_gather$state != "classtotal" & r1_gather$state != "Total"),]
r1_1_gather <- r1_gather[which(r1_gather$class != "Aves"),]
r1_2_gather <- r1_gather[which(r1_gather$class == "Aves"),]
g1 <- ggplot(r1_1_gather, aes(class, occCount))
g1 + geom_bar(aes(fill = state), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") + ylim(0,7500) #doesn't show Aves
```

```
## Warning: Removed 2 rows containing missing values (position_stack).
```

```
## Warning: Removed 4 rows containing missing values (geom_bar).
```

![plot of chunk unnamed-chunk-7](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/BISON-SGCN-FWSRegion1-chunk-6-1.png)

```r
# Need to figure out a better way to represent Aves for this type of figure
# g1_2 <- ggplot(r1_2_gather, aes(class, occCount))
# g1_2 + geom_bar(aes(fill = state), position = position_stack(reverse = TRUE), stat = "identity") +
#   coord_flip() +
#   theme(legend.position = "top")

#grid.arrange(g1, g1_2) # not working, puts the classes on the x axis and occCount on the y axis (flips things)
```

