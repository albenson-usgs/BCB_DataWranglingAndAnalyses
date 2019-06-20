On-the-fly Spatial Queries of OBIS Using the API
Abby Benson  
June 20, 2019  
## General information about the data
OBIS has had requests for the ability to conduct on-the-fly spatial queries of the database using user-defined geographic areas. To
accomplish this we need to pass those areas to the API as a Well Known Text or WKT. This notebook will show how to do this. The
visualization provided is simplistic and intended to show how the data can be accessed but will not provide an in depth analysis.
I am using the Flower Garden Banks National Marine Sanctuary general area as my focal spatial area for this visualzation.



First we need to grab the Well Known Text for the area of interest. We can do that using the 
OBIS WKT Tool - https://obis.org/maptool/#. IMPORTANT - try to limit the number of vertices. Keep the polygon simple.
I went ahead and did this for the area of interest and ended up with this WKT:
POLYGON ((-94.71863 28.37690, -94.65271 27.70298, -91.75232 27.78563, -91.85120 28.60381, -94.71863 28.37690))
Note the number of vertices is four.
Next we'll need to access the OBIS API for the data of interest


```r
OBIS_stats <- fromJSON("https://api.obis.org/v3/statistics/years?geometry=POLYGON%20((-94.71863%2028.37690,%20-94.65271%2027.70298,%20-91.75232%2027.78563,%20-91.85120%2028.60381,%20-94.71863%2028.37690))") 
```

The data from OBIS is in wide format so we need to reshape it for plotting


```r
head(OBIS_stats, n=10)
```

```
##    year records species taxa
## 1  1901       1       1    1
## 2  1917       1       1    1
## 3  1938      14       9   11
## 4  1939       1       1    1
## 5  1950      20      18   19
## 6  1951      10       8    9
## 7  1952       9       7    7
## 8  1954       1       0    1
## 9  1956       1       1    1
## 10 1957      92      42   52
```

```r
OBIS_stats_long <- gather(OBIS_stats, count_type, count, records:taxa, factor_key = T)
```

Finally we'll create a visualization of the data


```r
ggplot(data = OBIS_stats_long, aes(x=year, y=count)) + geom_line(aes(colour=count_type))
```

![plot of chunk unnamed-chunk-3](https://github.com/albenson-usgs/BCB_DataWranglingAndAnalyses/blob/master/OBISapi_Spatial_Query/Rplot.png)

