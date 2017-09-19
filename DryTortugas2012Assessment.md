DryTortugas2012
================
Abby Benson
September 19, 2017

Dry Tortugas 2012
-----------------

This notebook was created in response to a current iOBIS data harvest in which the Dry Tortugas 2012 dataset seems to have errors. The data used were downloaded from <https://www1.usgs.gov/obis-usa/ipt/resource?r=drytortugasreefvisualcensus2012>.

I'm working against the DwC-A for the Dry Tortugas 2012 dataset and I'm not seeing the missing eventIDs. Here are some examples.

Here is the event file, note no missing eventIDs

``` r
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.3.3

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

``` r
describe(event$eventID)
```

    ## event$eventID 
    ##        n  missing distinct 
    ##     2280        0     2280 
    ## 
    ## lowest : PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 1:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 2:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 3:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889:TimePeriodObserved 1:Depth:27.1 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889:TimePeriodObserved 2:Depth:27.1
    ## highest: PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392:TimePeriodObserved 2:Depth:26.1 PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392:TimePeriodObserved 3:Depth:26.1 PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 1:Depth:23   PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 2:Depth:23   PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 3:Depth:23

``` r
describe(event$parentEventID)
```

    ## event$parentEventID 
    ##        n  missing distinct 
    ##     2280        0      813 
    ## 
    ## lowest : PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889 PrimarySamplingUnit 442U:StationNumber 1:MapGridNumber 2573639 PrimarySamplingUnit 442U:StationNumber 2:MapGridNumber 2573639 PrimarySamplingUnit 443H:StationNumber 2:MapGridNumber 2569884
    ## highest: PrimarySamplingUnit N06U:StationNumber 2:MapGridNumber 2922416 PrimarySamplingUnit N07U:StationNumber 1:MapGridNumber 2967420 PrimarySamplingUnit N07U:StationNumber 2:MapGridNumber 2967420 PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392 PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392

Here is the occurrence file, note no missing eventIDs

``` r
library(Hmisc)
describe(occurrence$eventID)
```

    ## occurrence$eventID 
    ##        n  missing distinct 
    ##   384195        0     2280 
    ## 
    ## lowest : PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 1:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 2:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889:TimePeriodObserved 3:Depth:26.4 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889:TimePeriodObserved 1:Depth:27.1 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889:TimePeriodObserved 2:Depth:27.1
    ## highest: PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392:TimePeriodObserved 2:Depth:26.1 PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392:TimePeriodObserved 3:Depth:26.1 PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 1:Depth:23   PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 2:Depth:23   PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392:TimePeriodObserved 3:Depth:23

``` r
describe(occurrence$parentEventID)
```

    ## occurrence$parentEventID 
    ##        n  missing distinct 
    ##   384195        0      813 
    ## 
    ## lowest : PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889 PrimarySamplingUnit 441U:StationNumber 2:MapGridNumber 2569889 PrimarySamplingUnit 442U:StationNumber 1:MapGridNumber 2573639 PrimarySamplingUnit 442U:StationNumber 2:MapGridNumber 2573639 PrimarySamplingUnit 443H:StationNumber 2:MapGridNumber 2569884
    ## highest: PrimarySamplingUnit N06U:StationNumber 2:MapGridNumber 2922416 PrimarySamplingUnit N07U:StationNumber 1:MapGridNumber 2967420 PrimarySamplingUnit N07U:StationNumber 2:MapGridNumber 2967420 PrimarySamplingUnit N08U:StationNumber 1:MapGridNumber 2862392 PrimarySamplingUnit N08U:StationNumber 2:MapGridNumber 2862392

But maybe it's just that they don't match so I'll check one of the ones that you gave me specifically: parentEventID PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889

``` r
d <- occurrence[which(occurrence$parentEventID == 'PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889'),]
describe(d$eventID)
```

    ## d$eventID 
    ##        n  missing distinct 
    ##      493        0        3

``` r
describe(d$parentEventID)
```

    ## d$parentEventID 
    ##                                                              n 
    ##                                                            493 
    ##                                                        missing 
    ##                                                              0 
    ##                                                       distinct 
    ##                                                              1 
    ##                                                          value 
    ## PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889 
    ##                                                                          
    ## Value      PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889
    ## Frequency                                           493                  
    ## Proportion                                          1

Same thing for the event file

``` r
e <- event[which(event$parentEventID == 'PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889'),]
describe(e$eventID)
```

    ## e$eventID 
    ##        n  missing distinct 
    ##        3        0        3

``` r
describe(e$parentEventID)
```

    ## e$parentEventID 
    ##                                                              n 
    ##                                                              3 
    ##                                                        missing 
    ##                                                              0 
    ##                                                       distinct 
    ##                                                              1 
    ##                                                          value 
    ## PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889 
    ##                                                                          
    ## Value      PrimarySamplingUnit 441U:StationNumber 1:MapGridNumber 2569889
    ## Frequency                                           3                    
    ## Proportion                                          1
