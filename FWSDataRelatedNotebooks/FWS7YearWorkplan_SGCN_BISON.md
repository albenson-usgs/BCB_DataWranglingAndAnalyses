Fish and Wildlife Service 7-Year Workplan combined with SGCN and BISON 
Abby Benson  
January 26, 2018  


```r
# Data for this notebook was brought together and managed using the R script SGCNMatchedtoFWSNatWP.R
```

## General information about the data
The data contained in this report are a combination of the compiled national Species of Greatest Conservation Need (SGCN) as identified by 
states and territories. SGCN lists were collated by USGS and aligned to the Integrated Taxonomic Information System (ITIS), the World Register 
of Marine Species (WoRMS), and the Fish and Wildlife Service Threatened and Endangered Species System. Species that were not found in ITIS or 
WoRMS are not considered part of the SGCN National List but are still included in this analysis.




### Number of Species of Greatest Conservation Need that are also FWS 7 Year Workplan Species:


```r
nrow(sgcn_fwsListingWP)
```

```
## [1] 138
```

Plot of the number of occurrences for the SGCN that are FWS 7 year workplan species


```r
p1 <- ggplot(r2, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Total Number of BISON Observations for FWS 7 Year Workplan Species Which Are SGCN Species Summarized by Class", 
       caption="") +  
  theme(axis.title.x = element_blank()) +
  ylim(0,1000) +
  coord_flip()
p1 <- ggplotGrob(p1)
```

```
## Warning: Removed 4 rows containing missing values (geom_point).

## Warning: Removed 4 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_segment).
```

```r
p2 <- ggplot(r2, aes(x=class, y=Total)) + 
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
  ylim(1001,10000) +
  coord_flip()
p2 <- ggplotGrob(p2)
```

```
## Warning: Removed 6 rows containing missing values (geom_point).
```

```
## Warning: Removed 6 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_segment).
```

```r
p3 <- ggplot(r2, aes(x=class, y=Total)) + 
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
       caption="source: SGCN, BISON, FWS TESS") + 
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank()) +
  ylim(10001,55000) + 
  coord_flip()
p3 <- ggplotGrob(p3)
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_segment).
```

grid.arrange(p1, p2, p3, ncol=3)


```r
g2 <- cbind(p1, p2, p3, size = "first")
grid::grid.draw(g2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Tables of SGCN + FWS National Working Plan species by proposed time frames


```r
tf_table <- tableprep_FWSlistingWP %>%
  group_by(Timeframe) %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```

```r
#knitr::spin('C:/Users/albenson/Documents/SWAPs/DataAnalysis/FWSNationalListingWorkplan/FWS7YearWorkplan_SGCN_BISON.R')
```

