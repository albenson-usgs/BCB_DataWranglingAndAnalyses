Midwestern Association of Fish and Wildlife Agencies (MAFWA) SGCN Turtle Analysis
Abby Benson  
April 30, 2018  


```r
# Data for this notebook was brought together and managed using the R script MAFWA_SGCN_Analysis.R
```

## General information about the data
The data contained in this report are a combination of the Species of Greatest Conservation Need (SGCN) as identified by the states in the
Midwestern Association of Fish and Wildlife Agences (MAFWA). SGCN lists were collated by USGS and matched to the Integrated 
Taxonomic Information System (ITIS), the World Register of Marine Species (WoRMS), Fish and Wildlife Service listing status information, 
and the NatureServe database. In this notebook we are looking specifically at turtles identified as SGCN in the MAFWA states and the
information available for those turtle species in BISON.
List of states included in this analysis: Illinois, Indiana, Iowa, Kansas, Kentucky, Michigan, Minnesota, Missouri, Nebraska, North Dakota,
Ohio, South Dakota, Wisconsin



Note: In order to have MAFWA_list and MAFWA_list_species, you need to first run the R script MAFWA_SGCN_Analysis.R and MAFWA_turtles_analysis.R



### Number of Species of Greatest Conservation Need turtles listed in MAFWA states:


```r
nrow(MAFWA_turtlesOcc)
```

```
## [1] 25
```


### General information about the twenty turtles from SGCN, Fish and Wildlife Service, and NatureServe


```r
MAFWA_turtlesGenInfo <- MAFWA_turtlesGenInfo[c("ScientificName","Common Name","statelist2015","ListingStatus","NS_NationalStatus",
                                               "NS_NationalReviewDate")]
MAFWA_turtlesGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|                ScientificName                 |           Common Name           |                                                                                                                 statelist2015                                                                                                                  |                   ListingStatus                   | NS_NationalStatus | NS_NationalReviewDate |
|:---------------------------------------------:|:-------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------:|:-----------------:|:---------------------:|
|             Apalone mutica mutica             | Midland Smooth Softshell Turtle |                                                                                                      Kentucky,Missouri,Ohio,West Virginia                                                                                                      |                        NA                         |      Secure       |      1996-10-05       |
|                Apalone mutica                 |        Smooth Softshell         |                                                                 Illinois,Iowa,Kansas,Louisiana,Minnesota,Nebraska,North Dakota,Oklahoma,South Dakota,Tennessee,Texas,Wisconsin                                                                 |                        NA                         |      Secure       |      1996-10-05       |
|               Apalone spinifera               |         Spiny Softshell         |                                                                                        Iowa,Maryland,North Dakota,Oklahoma,South Carolina,Texas,Vermont                                                                                        |                        NA                         |      Secure       |      1996-10-05       |
|              Chelydra serpentina              |         Snapping Turtle         |                                                                                            Iowa,New York,North Dakota,South Carolina,Texas,Virginia                                                                                            |                        NA                         |      Secure       |      1996-10-05       |
|           Chrysemys picta dorsalis            |     Southern Painted Turtle     |                                                                                                               Kentucky,Missouri                                                                                                                |                        NA                         |      Secure       |      2003-02-26       |
|                Clemmys guttata                |         Spotted Turtle          | Connecticut,Delaware,District of Columbia,Florida,Georgia,Illinois,Indiana,Maine,Maryland,Massachusetts,Michigan,New Hampshire,New Jersey,New York,North Carolina,Ohio,Pennsylvania,Rhode Island,South Carolina,Vermont,Virginia,West Virginia |                        NA                         |      Secure       |      1996-10-05       |
|        Deirochelys reticularia miaria         |     Western Chicken Turtle      |                                                                                                    Louisiana,Mississippi,Missouri,Oklahoma                                                                                                     | Under Review in the Candidate or Petition Process |      Secure       |      1996-10-05       |
|             Emydoidea blandingii              |        Blanding's Turtle        |                                                       Illinois,Indiana,Iowa,Maine,Massachusetts,Michigan,Minnesota,Missouri,Nebraska,New Hampshire,New York,Ohio,Pennsylvania,Wisconsin                                                        | Under Review in the Candidate or Petition Process | Apparently Secure |      1996-10-05       |
|              Glyptemys insculpta              |           Wood Turtle           |                           Connecticut,District of Columbia,Iowa,Maine,Maryland,Massachusetts,Michigan,Minnesota,New Hampshire,New Jersey,New York,Pennsylvania,Rhode Island,Vermont,Virginia,West Virginia,Wisconsin                           | Under Review in the Candidate or Petition Process |    Vulnerable     |      2010-11-11       |
|             Graptemys geographica             |       Northern Map Turtle       |                                                                               Iowa,Kansas,Maryland,New York,North Carolina,Ohio,Oklahoma,Virginia,West Virginia                                                                                |                        NA                         |      Secure       |      1996-10-05       |
|            Graptemys ouachitensis             |       Ouachita Map Turtle       |                                                                                                       Iowa,Louisiana,Ohio,West Virginia                                                                                                        |                        NA                         |      Secure       |      1996-10-05       |
|      Graptemys pseudogeographica kohnii       |     Mississippi Map Turtle      |                                                                                                               Kentucky,Oklahoma                                                                                                                |                        NA                         | Apparently Secure |      1996-10-21       |
| Graptemys pseudogeographica pseudogeographica |        False Map Turtle         |                                                                                                                    Kentucky                                                                                                                    |                        NA                         |      Secure       |      1996-10-05       |
|          Graptemys pseudogeographica          |        False Map Turtle         |                                                                                                         Iowa,North Dakota,South Dakota                                                                                                         |                Species of Concern                 |      Secure       |      1996-10-05       |
|            Kinosternon flavescens             |        Yellow Mud Turtle        |                                                                                                    Arizona,Colorado,Illinois,Iowa,Missouri                                                                                                     |                        NA                         |      Secure       |      1996-10-05       |
|             Kinosternon subrubrum             |       Eastern Mud Turtle        |                                                                                                     District of Columbia,Indiana,Kentucky                                                                                                      |                        NA                         |      Secure       |      1996-10-05       |
|            Macrochelys temminckii             |    Alligator Snapping Turtle    |                                                                    Florida,Georgia,Illinois,Indiana,Kansas,Kentucky,Louisiana,Mississippi,Missouri,Oklahoma,Tennessee,Texas                                                                    |                        NA                         |    Vulnerable     |      1996-10-05       |
|              Pseudemys concinna               |          River Cooter           |                                                                                             Illinois,Indiana,Oklahoma,South Carolina,West Virginia                                                                                             |                Species of Concern                 |      Secure       |      1996-10-05       |
|             Sternotherus odoratus             |           Musk Turtle           |                                                                                              District of Columbia,Iowa,Michigan,New York,Vermont                                                                                               |                        NA                         |      Secure       |      1996-10-05       |
|          Terrapene carolina carolina          |       Eastern Box Turtle        |                                             Connecticut,District of Columbia,Maine,Michigan,New Hampshire,New Jersey,New York,North Carolina,Ohio,Pennsylvania,Rhode Island,Virginia,West Virginia                                             |                        NA                         |      Secure       |      1996-10-05       |
|         Terrapene carolina triunguis          |      Three-toed Box Turtle      |                                                                                                                    Missouri                                                                                                                    |                        NA                         |      Secure       |      1996-10-05       |
|              Terrapene carolina               |       Eastern Box Turtle        |                                                                            Delaware,Florida,Illinois,Indiana,Maryland,Massachusetts,South Carolina,Tennessee,Texas                                                                             |                        NA                         |      Secure       |      1996-10-05       |
|            Terrapene ornata ornata            |        Ornate Box Turtle        |                                                                                                                Missouri,Wyoming                                                                                                                |                        NA                         |      Secure       |      1996-10-05       |
|               Terrapene ornata                |        Ornate Box Turtle        |                                                                                 Arizona,Arkansas,Illinois,Indiana,Iowa,Louisiana,South Dakota,Texas,Wisconsin                                                                                  |                        NA                         |      Secure       |      1996-10-05       |
|           Trachemys scripta elegans           |        Red-eared Slider         |                                                                                                                    Nebraska                                                                                                                    |                        NA                         |      Secure       |      1996-10-05       |

### Let's examine the available data from BISON


```r
bisonTurtleGenInfo <- bisonTurtleData %>%
  group_by(ScientificName) %>%
  summarise(Total = n(), `Minimum Date`=min(eventDate, na.rm = T), `Maximum Date`= max(eventDate, na.rm = T))
bisonTurtleGenInfo <- merge(bisonTurtleGenInfo, turtleCommonNames, by="ScientificName")
bisonTurtleGenInfo <- bisonTurtleGenInfo[c("ScientificName", "Common Name", "Total", "Minimum Date", "Maximum Date")]

bisonTurtleGenInfo %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|                ScientificName                 |           Common Name           | Total | Minimum Date | Maximum Date |
|:---------------------------------------------:|:-------------------------------:|:-----:|:------------:|:------------:|
|                Apalone mutica                 |        Smooth Softshell         |  658  |  1878-05-01  |  2016-06-20  |
|             Apalone mutica mutica             | Midland Smooth Softshell Turtle |  127  |  1883-01-01  |  2016-06-03  |
|               Apalone spinifera               |         Spiny Softshell         | 1497  |  1858-01-01  |  2016-08-31  |
|              Chelydra serpentina              |         Snapping Turtle         | 2037  |  1869-01-01  |  2016-09-22  |
|           Chrysemys picta dorsalis            |     Southern Painted Turtle     |  28   |  1906-03-15  |  1978-01-01  |
|                Clemmys guttata                |         Spotted Turtle          |  566  |  1864-01-01  |  2014-05-15  |
|        Deirochelys reticularia miaria         |     Western Chicken Turtle      |  13   |  1969-01-01  |  1969-01-01  |
|             Emydoidea blandingii              |        Blanding's Turtle        | 3131  |  1858-08-01  |  2016-09-04  |
|              Glyptemys insculpta              |           Wood Turtle           |  725  |  1990-06-01  |  2016-07-12  |
|             Graptemys geographica             |       Northern Map Turtle       | 1546  |  1853-01-01  |  2016-09-27  |
|            Graptemys ouachitensis             |       Ouachita Map Turtle       | 6480  |  1878-04-01  |  2016-08-31  |
|          Graptemys pseudogeographica          |        False Map Turtle         |  425  |  1878-04-01  |  2015-03-15  |
|      Graptemys pseudogeographica kohnii       |     Mississippi Map Turtle      |  14   |  1930-08-01  |  2006-10-03  |
| Graptemys pseudogeographica pseudogeographica |        False Map Turtle         | 2451  |  1888-08-09  |  1985-01-01  |
|            Kinosternon flavescens             |        Yellow Mud Turtle        |  647  |  1910-01-01  |  2015-07-31  |
|             Kinosternon subrubrum             |       Eastern Mud Turtle        |  51   |  1883-01-01  |  2014-06-01  |
|            Macrochelys temminckii             |    Alligator Snapping Turtle    |  143  |  1920-01-01  |  1999-01-01  |
|              Pseudemys concinna               |          River Cooter           |  109  |  1878-05-01  |  2013-06-08  |
|             Sternotherus odoratus             |           Musk Turtle           | 1481  |  1859-01-01  |  2016-09-22  |
|              Terrapene carolina               |       Eastern Box Turtle        | 1190  |  1873-01-01  |  2016-09-20  |
|          Terrapene carolina carolina          |       Eastern Box Turtle        | 1133  |  1897-05-06  |  2016-09-23  |
|         Terrapene carolina triunguis          |      Three-toed Box Turtle      |  176  |  1878-06-01  |  2016-09-24  |
|               Terrapene ornata                |        Ornate Box Turtle        | 1452  |  1878-06-01  |  2016-09-24  |
|            Terrapene ornata ornata            |        Ornate Box Turtle        |  122  |  1871-06-20  |  2016-07-25  |
|           Trachemys scripta elegans           |        Red-eared Slider         |  412  |  1890-01-01  |  2016-09-12  |

Some of the observations were missing dates


```r
sum(is.na(bisonTurtleData$eventDate))
```

```
## [1] 6263
```

Plot a map of Blanding's, Spotted, and Wood Turtles with NatureServe Centroid locations removed


```r
bisonTurtleData$decimalLatitude <- as.numeric(bisonTurtleData$decimalLatitude)
bisonTurtleData$decimalLongitude <- as.numeric(bisonTurtleData$decimalLongitude)
bisonBlandingsTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Emydoidea blandingii" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]
bisonSpottedTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Clemmys guttata" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]
bisonWoodTurtles <- bisonTurtleData[which(bisonTurtleData$ScientificName == "Glyptemys insculpta" & bisonTurtleData$dataSource != "NatureServe Network Species Occurrence Data (County Centroids)"),]
```

#### Blanding's Turtle map


```r
plot_map_leaflet(bisonBlandingsTurtles)
```
![plot of chunk unnamed-chunk-1](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_BlandingsTurtleMap.png)
```
## Error in loadNamespace(name): there is no package called 'webshot'
```

#### Spotted Turtle map


```r
plot_map_leaflet(bisonSpottedTurtles)
```
![plot of chunk unnamed-chunk-2](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_SpottedTurtleMap.png)
```
## Error in loadNamespace(name): there is no package called 'webshot'
```

#### Wood Turtle map


```r
plot_map_leaflet(bisonWoodTurtles)
```
![plot of chunk unnamed-chunk-3](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_WoodTurtleMap.png)
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

![plot of chunk unnamed-chunk-10](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_Notebook_dataSources.png)

Limit the previous figure to only the occurrences from the last 20 years


```r
bisonTurtleData_recent <- bisonTurtleData[which(bisonTurtleData$eventDate >= "1998-01-01"),]
ds2 <- count(bisonTurtleData_recent, dataSource)
ds2_plot <- ggplot(ds2, aes(dataSource, n))
ds2_plot + geom_bar(aes(fill = dataSource), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  guides(fill = F)
```

![plot of chunk unnamed-chunk-11](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/MAFWA_turtles/MAFWA_turtles_Notebook_dataSources_recent20yrs.png)

