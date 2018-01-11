BISON - SGCN Data Mashup Report  
Abby Benson  
January 9, 2018  


```r
# Data for this notebook was brought together and managed using the R script BISON_SGCN_Mashup.R
```

## General information about the data
The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in Fish
and Wildlife Service Region 3. SGCN lists were collated by USGS and matched to the Integrated Taxonomic Information System (ITIS), the World Register of
Marine Species (WoRMS), Fish and Wildlife Service listing status information, and the NatureServe database. Species that were not found in ITIS
or WoRMS are not considered part of the SGCN National List but are still included in this analysis.




### Number of Species of Greatest Conservation Need in Fish and Wildlife Service Region 3:


```r
nrow(FWSRegion3_natlist)
```

```
## [1] 1572
```


### Number of SGCN that are considered Endangered, Threatened, Candidate, or Under Review by the Fish and Wildlife Service:


```r
nrow(FWSRegion3_natlist[which(FWSRegion3_natlist$ListingStatus == "Candidate" | FWSRegion3_natlist$ListingStatus == "Endangered" | 
                                FWSRegion3_natlist$ListingStatus == "Threatened" | 
                                FWSRegion3_natlist$ListingStatus == "Under Review in the Candidate or Petition Process"),])
```

```
## [1] 78
```


### BISON Occurrence Data Available for these SGCN grouped by the Class level of the taxonomic heirarchy


```r
r_gather <- gather(r, key = "state", value = "occCount", -class)
r_gather <- r_gather[which(r_gather$state != "classtotal" & r_gather$state != "FWSRegionTotal"),]
g <- ggplot(r_gather, aes(class, occCount))
g + geom_bar(aes(fill = state), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") + ylim(0,100000) #doesn't show Aves
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-4](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/unnamed-chunk-4-1.png)

Or maybe this way


```r
r_twoway %>% 
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|       class        | Illinois | Indiana |  Iowa  | Michigan | Missouri | Minnesota |  Ohio   | Wisconsin |  Total  |
|:------------------:|:--------:|:-------:|:------:|:--------:|:--------:|:---------:|:-------:|:---------:|:-------:|
|     Collembola     |    0     |    0    |   0    |    0     |    0     |     0     |    0    |     0     |    0    |
|    Maxillopoda     |    0     |    0    |   0    |    0     |    0     |     0     |    0    |     0     |    0    |
|   Trepaxonemata    |    0     |    0    |   0    |    0     |    0     |     0     |    0    |     0     |    0    |
|   Actinopterygii   |    0     |    3    |   0    |    0     |    0     |     0     |    0    |     0     |    3    |
|    Branchiopoda    |    0     |    1    |   0    |    0     |    0     |     0     |    2    |     0     |    3    |
|     Diplopoda      |    1     |    0    |   0    |    1     |    1     |     0     |    3    |     1     |    7    |
|     Arachnida      |    16    |    3    |   5    |    2     |    12    |     5     |    3    |     4     |   50    |
|    Chondrostei     |    12    |    9    |   18   |    1     |    22    |    23     |    4    |     8     |   97    |
| Cephalaspidomorphi |    3     |   12    |   2    |    38    |   116    |    51     |   16    |    54     |   292   |
|   Lycopodiopsida   |    6     |    7    |   0    |    20    |    14    |    210    |   22    |    103    |   382   |
|    Malacostraca    |   242    |   49    |   8    |    15    |    59    |    14     |   21    |    57     |   465   |
|      Holostei      |    76    |   49    |   80   |   103    |    30    |    165    |    1    |    17     |   521   |
|     Gastropoda     |   699    |   73    |  143   |   324    |   101    |    60     |   177   |    67     |  1644   |
|   Polypodiopsida   |   138    |   117   |   23   |   118    |   143    |    476    |   388   |    298    |  1701   |
|      Bivalvia      |   305    |   125   |   72   |   279    |   200    |    52     |   532   |    399    |  1964   |
|      Reptilia      |   697    |  1425   |  203   |   503    |   410    |    190    |  1335   |    267    |  5030   |
|      Mammalia      |   991    |   804   |  562   |   1973   |   2992   |    363    |  1450   |    438    |  9573   |
|      Amphibia      |   1620   |  2799   |  191   |   1489   |   2152   |    446    |  4449   |    560    |  13706  |
|   Magnoliopsida    |   2705   |  1570   |  1173  |   2725   |   2755   |   3018    |  1582   |   2382    |  17910  |
|         NA         |   3913   |  2269   |  478   |   5512   |   2685   |    581    |  5867   |   6236    |  27541  |
|      Insecta       |   5615   |  59587  |  564   |   4512   |   3197   |    998    |  2247   |   1972    |  78692  |
|     Teleostei      |   3099   |  6929   |  6220  |  12686   |  10346   |   32343   |  5660   |   20427   |  97710  |
|        Aves        | 1499841  | 650097  | 327424 | 1611066  |  757260  |  905268   | 1773414 |  1693185  | 9217555 |

```r
#knitr::spin('C:/Users/albenson/Documents/SWAPs/DataAnalysis/BISON_SGCN_Mashup/BISON_SGCN_Mashup_Notebook.R')
```

