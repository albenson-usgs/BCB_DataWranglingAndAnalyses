Midwestern Association of Fish and Wildlife Agencies (MAFWA) SGCN Turtle Analysis
Abby Benson  
April 12, 2018  


```r
# Data for this notebook was brought together and managed using the R script MAFWA_SGCN_Analysis.R
```

## General information about the data
The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in the
Midwestern Association of Fish and Wildlife Agences (MAFWA). SGCN lists were collated by USGS and matched to the Integrated 
Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
and the NatureServe database. In this notebook we are looking specifically at turtles identified as SGCN in the MAFWA states and the
information available for those turtle species in BISON.



Note: In order to have MAFWA_list and MAFWA_list_species, you need to first run the R script MAFWA_SGCN_Analysis.R and MAFWA_turtles_analysis.R



### Number of Species of Greatest Conservation Need turtles listed in MAFWA states:


```r
nrow(MAFWA_turtlesOcc)
```

```
## [1] 20
```


### General information about the twenty turtles from SGCN, Fish and Wildlife Service, and NatureServe


```r
MAFWA_turtlesGenInfo <- MAFWA_turtlesGenInfo[c("ScientificName","Common Name","statelist2015","ListingStatus","NSGlobalDescription",
                                               "NSGlobalReviewDate")]
MAFWA_turtlesGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|                ScientificName                 |           Common Name           |                                             statelist2015                                             |                   ListingStatus                   | NSGlobalDescription | NSGlobalReviewDate |
|:---------------------------------------------:|:-------------------------------:|:-----------------------------------------------------------------------------------------------------:|:-------------------------------------------------:|:-------------------:|:------------------:|
|             Apalone mutica mutica             | Midland Smooth Softshell Turtle |                                                 Ohio                                                  |                        NA                         |       Secure        |     1996-10-31     |
|                Apalone mutica                 |        Smooth Softshell         | Illinois,Iowa,Kansas,Louisiana,Minnesota,Nebraska,North Dakota,South Dakota,Tennessee,Texas,Wisconsin |                        NA                         |       Secure        |     1996-10-23     |
|           Chrysemys picta dorsalis            |     Southern Painted Turtle     |                                               Kentucky                                                |                        NA                         |       Secure        |     2005-05-02     |
|                Clemmys guttata                |         Spotted Turtle          |                                                 Ohio                                                  |                        NA                         |       Secure        |     2016-02-02     |
|        Deirochelys reticularia miaria         |     Western Chicken Turtle      |                                Louisiana,Mississippi,Missouri,Oklahoma                                | Under Review in the Candidate or Petition Process |       Secure        |     1996-10-31     |
|             Emydoidea blandingii              |        Blanding's Turtle        |                                             Nebraska,Ohio                                             | Under Review in the Candidate or Petition Process |  Apparently Secure  |     2016-02-02     |
|             Graptemys geographica             |       Northern Map Turtle       |                                                 Ohio                                                  |                        NA                         |       Secure        |     2016-02-02     |
|            Graptemys ouachitensis             |       Ouachita Map Turtle       |                                                 Ohio                                                  |                        NA                         |       Secure        |     2005-05-02     |
|      Graptemys pseudogeographica kohnii       |     Mississippi Map Turtle      |                                           Kentucky,Oklahoma                                           |                        NA                         |  Apparently Secure  |     2002-02-20     |
| Graptemys pseudogeographica pseudogeographica |        False Map Turtle         |                                               Kentucky                                                |                        NA                         |       Secure        |     1996-10-31     |
|          Graptemys pseudogeographica          |        False Map Turtle         |                                    Iowa,North Dakota,South Dakota                                     |                Species of Concern                 |       Secure        |     2005-05-02     |
|            Kinosternon flavescens             |        Yellow Mud Turtle        |                                Arizona,Colorado,Illinois,Iowa,Missouri                                |                        NA                         |       Secure        |     2001-08-06     |
|             Kinosternon subrubrum             |       Eastern Mud Turtle        |                                 District of Columbia,Indiana,Kentucky                                 |                        NA                         |       Secure        |     1996-10-23     |
|              Pseudemys concinna               |          River Cooter           |                             Illinois,Indiana,South Carolina,West Virginia                             |                Species of Concern                 |         NA          |         NA         |
|             Sternotherus odoratus             |           Musk Turtle           |                          District of Columbia,Iowa,Michigan,New York,Vermont                          |                        NA                         |       Secure        |     2016-02-02     |
|          Terrapene carolina carolina          |       Eastern Box Turtle        |                                                 Ohio                                                  |                        NA                         |       Secure        |     2016-02-02     |
|         Terrapene carolina triunguis          |      Three-toed Box Turtle      |                                               Missouri                                                |                        NA                         |       Secure        |     1996-10-31     |
|            Terrapene ornata ornata            |        Ornate Box Turtle        |                                               Missouri                                                |                        NA                         |       Secure        |     1996-10-31     |
|               Terrapene ornata                |        Ornate Box Turtle        |                                                Indiana                                                |                        NA                         |       Secure        |     2005-05-04     |
|           Trachemys scripta elegans           |        Red-eared Slider         |                                               Nebraska                                                |                        NA                         |       Secure        |     1996-10-31     |

### Let's examine the available data from BISON


```r
bisonTurtleGenInfo <- bisonTurtleData %>%
  group_by(ScientificName) %>%
  summarise(Total = n(), `Minimum Date`=min(eventDate, na.rm = T), `Maximum Date`= max(eventDate, na.rm = T))

bisonTurtleGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|                ScientificName                 | Total | Minimum Date | Maximum Date |
|:---------------------------------------------:|:-----:|:------------:|:------------:|
|                Apalone mutica                 |  658  |  1878-05-01  |  2016-06-20  |
|             Apalone mutica mutica             |  127  |  1883-01-01  |  2016-06-03  |
|           Chrysemys picta dorsalis            |  28   |  1906-03-15  |  1978-01-01  |
|                Clemmys guttata                |  566  |  1864-01-01  |  2014-05-15  |
|        Deirochelys reticularia miaria         |  13   |  1969-01-01  |  1969-01-01  |
|             Emydoidea blandingii              | 3131  |  1858-08-01  |  2016-09-04  |
|             Graptemys geographica             | 1546  |  1853-01-01  |  2016-09-27  |
|            Graptemys ouachitensis             | 6480  |  1878-04-01  |  2016-08-31  |
|          Graptemys pseudogeographica          |  425  |  1878-04-01  |  2015-03-15  |
|      Graptemys pseudogeographica kohnii       |  14   |  1930-08-01  |  2006-10-03  |
| Graptemys pseudogeographica pseudogeographica | 2451  |  1888-08-09  |  1985-01-01  |
|            Kinosternon flavescens             |  647  |  1910-01-01  |  2015-07-31  |
|             Kinosternon subrubrum             |  51   |  1883-01-01  |  2014-06-01  |
|              Pseudemys concinna               |  109  |  1878-05-01  |  2013-06-08  |
|             Sternotherus odoratus             | 1481  |  1859-01-01  |  2016-09-22  |
|          Terrapene carolina carolina          | 1133  |  1897-05-06  |  2016-09-23  |
|         Terrapene carolina triunguis          |  176  |  1878-06-01  |  2016-09-24  |
|               Terrapene ornata                | 1452  |  1878-06-01  |  2016-09-24  |
|            Terrapene ornata ornata            |  122  |  1871-06-20  |  2016-07-25  |
|           Trachemys scripta elegans           |  412  |  1890-01-01  |  2016-09-12  |

Some of the observations were missing dates


```r
sum(is.na(bisonTurtleData$eventDate))
```

```
## [1] 4592
```

Plot a map of all the points from all species


```r
bisonTurtleData$decimalLatitude <- as.numeric(bisonTurtleData$decimalLatitude)
bisonTurtleData$decimalLongitude <- as.numeric(bisonTurtleData$decimalLongitude)
plot_map_leaflet(bisonTurtleData)
```
![plot of chunk unnamed-chunk-6](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_Notebook_map.png)
```
## Error in loadNamespace(name): there is no package called 'webshot'
```

We might also want to know which institutions are supplying the data for these species


```r
ds <- count(bisonTurtleData, dataSource)
ds_plot <- ggplot(ds, aes(dataSource, n))
ds_plot + geom_bar(aes(fill = dataSource), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  guides(fill = F)
```

![plot of chunk unnamed-chunk-7](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_Notebook_dataSources.png)

