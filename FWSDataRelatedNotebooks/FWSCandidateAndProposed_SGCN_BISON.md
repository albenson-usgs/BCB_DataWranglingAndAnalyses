Fish and Wildlife Service Candidate and Proposed for Listing Species combined with SGCN and BISON 
Abby Benson  
January 30, 2018  


```r
# Data for this notebook was brought together and managed using the R script FWSCandidateProposed_SGCNSpecies.R
```

## General information about the data
The data contained in this report are a combination of the compiled national Species of Greatest Conservation Need (SGCN) as identified by 
states and territories. SGCN lists were collated by USGS and aligned to the Integrated Taxonomic Information System (ITIS), the World Register 
of Marine Species (WoRMS), and the Fish and Wildlife Service Threatened and Endangered Species System. Species that were not found in ITIS or 
WoRMS are not considered part of the SGCN National List but are still included in this analysis.




### Number of Species of Greatest Conservation Need that are also FWS Candidate or Proposed for Listing Species:


```r
nrow(CandPropSpecies)
```

```
## [1] 480
```

Plot of the number of occurrences for the SGCN that are candidate or proposed for listing species


```r
p1 <- ggplot(r, aes(x=class, y=Total)) + 
  geom_point(stat = "identity") +
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=class, 
                   xend=class, 
                   y=min(Total), 
                   yend=max(Total)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Total Number of BISON Observations for FWS Candidate and Proposed for Listing Species Which Are SGCN Species Summarized by Class", 
       caption="") +  
  theme(axis.title.x = element_blank()) +
  ylim(0,1000) +
  coord_flip()
p1 <- ggplotGrob(p1)
```

```
## Warning: Removed 10 rows containing missing values (geom_point).

## Warning: Removed 10 rows containing missing values (geom_point).
```

```
## Warning: Removed 18 rows containing missing values (geom_segment).
```

```r
p2 <- ggplot(r, aes(x=class, y=Total)) + 
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
## Warning: Removed 12 rows containing missing values (geom_point).
```

```
## Warning: Removed 12 rows containing missing values (geom_point).
```

```
## Warning: Removed 18 rows containing missing values (geom_segment).
```

```r
p3 <- ggplot(r, aes(x=class, y=Total)) + 
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
  ylim(10001,260000) + 
  coord_flip()
p3 <- ggplotGrob(p3)
```

```
## Warning: Removed 14 rows containing missing values (geom_point).
```

```
## Warning: Removed 14 rows containing missing values (geom_point).
```

```
## Warning: Removed 18 rows containing missing values (geom_segment).
```

```r
g <- cbind(p1, p2, p3, size = "first")
grid::grid.draw(g)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## Breakdown of Occurrences in BISON by FWS Region of the SGCN Species + Candidate or Proposed for Listing Species
# FWS Region 1 - Hawaii, Idaho, Oregon, Washington, American Samoa, Commonwealth of the Northern Mariana Islands, Guam and the Pacific Trust Territories


```r
FWSRegion1_CPlist_tab <- FWSRegion1_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion1_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|       ScientificName        |         Common Name         | bisontotal |                   ListingStatus                   |
|:---------------------------:|:---------------------------:|:----------:|:-------------------------------------------------:|
|     Actinemys marmorata     |     Western Pond Turtle     |    2055    | Under Review in the Candidate or Petition Process |
|    Arborimus longicaudus    |        Red Tree Vole        |    605     |                     Candidate                     |
|    Batrachoseps wrighti     |  Oregon Slender Salamander  |    644     | Under Review in the Candidate or Petition Process |
|      Bombus franklini       |    Franklin's Bumble Bee    |     17     | Under Review in the Candidate or Petition Process |
|    Copablepharon fuscum     |      Sand-verbena Moth      |     40     | Under Review in the Candidate or Petition Process |
|     Cryptomastix devia      |       Puget oregonian       |     64     | Under Review in the Candidate or Petition Process |
|   Cryptomastix hendersoni   |     Columbia oregonian      |     47     | Under Review in the Candidate or Petition Process |
| Euchloe ausonides insulanus |        Island marble        |     0      |                     Candidate                     |
|     Fratercula cirrhata     |        Tufted Puffin        |   24084    | Under Review in the Candidate or Petition Process |
|   Monadenia fidelis minor   |       Dalles sideband       |     0      | Under Review in the Candidate or Petition Process |
|      Phacelia argentea      |     Sand dune phacelia      |    161     | Under Review in the Candidate or Petition Process |
|         Rana boylii         | Foothill Yellow-legged Frog |    5170    | Under Review in the Candidate or Petition Process |
|        Rana cascadae        |        Cascades Frog        |    2643    | Under Review in the Candidate or Petition Process |
|    Rhyacotriton cascadae    | Cascade Torrent Salamander  |    582     | Under Review in the Candidate or Petition Process |
|     Rhyacotriton kezeri     | Columbia Torrent Salamander |    738     | Under Review in the Candidate or Petition Process |
|    Vulpes vulpes necator    |    Sierra Nevada Red Fox    |    273     |                     Candidate                     |

# FWS Region 2 - Arizona, New Mexico, Oklahoma, and Texas


```r
FWSRegion2_CPlist_tab <- FWSRegion2_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion2_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|              ScientificName               |         Common Name         | bisontotal |                   ListingStatus                   |
|:-----------------------------------------:|:---------------------------:|:----------:|:-------------------------------------------------:|
|           Amazona viridigenalis           |     Red-crowned Amazon      |   15851    |                     Candidate                     |
|            Amblyscirtes linda             |  Linda's Roadside-Skipper   |     53     | Under Review in the Candidate or Petition Process |
|             Amphinaias aurea              |         golden orb          |     0      |                     Candidate                     |
|          Amphinaias houstonensis          |      smooth pimpleback      |     0      |                     Candidate                     |
|            Amphinaias petrina             |      Texas pimpleback       |     0      |                     Candidate                     |
|              Amsonia tharpii              |      feltleaf bluestar      |     44     | Under Review in the Candidate or Petition Process |
|            Asclepias prostrata            |     prostrate milkweed      |     12     | Under Review in the Candidate or Petition Process |
|          Ashmunella macromphala           |  Cook's Peak woodlandsnail  |     4      | Under Review in the Candidate or Petition Process |
|              Bartonia texana              |       Texas screwstem       |     35     | Under Review in the Candidate or Petition Process |
|             Cambarus tartarus             |   Oklahoma cave crayfish    |     12     | Under Review in the Candidate or Petition Process |
|             Cyprinella lepida             |       plateau shiner        |    204     | Under Review in the Candidate or Petition Process |
|           Cyprinodon pecosensis           |        Pecos pupfish        |    518     | Under Review in the Candidate or Petition Process |
|            Cyprinodon tularosa            |     White Sands pupfish     |     15     | Under Review in the Candidate or Petition Process |
|             Cyprogenia aberti             |      western fanshell       |    298     | Under Review in the Candidate or Petition Process |
|      Deirochelys reticularia miaria       |   Western Chicken Turtle    |     57     | Under Review in the Candidate or Petition Process |
|             Dipodomys elator              |     Texas Kangaroo Rat      |    421     | Under Review in the Candidate or Petition Process |
|             Eurycea latitans              | Cascade Caverns Salamander  |     62     | Under Review in the Candidate or Petition Process |
|             Eurycea neotenes              |      Texas Salamander       |    3850    | Under Review in the Candidate or Petition Process |
|              Eurycea robusta              |   Blanco Blind Salamander   |     1      | Under Review in the Candidate or Petition Process |
|           Eurycea tridentifera            |   Comal Blind Salamander    |    154     | Under Review in the Candidate or Petition Process |
|            Eurycea tynerensis             |     Oklahoma Salamander     |    1033    | Under Review in the Candidate or Petition Process |
|           Fusconaia lananensis            |       triangle pigtoe       |     28     | Under Review in the Candidate or Petition Process |
|            Genistidium dumosum            |          brushpea           |     4      | Under Review in the Candidate or Petition Process |
|                Gila nigra                 |       headwater chub        |     41     |                Proposed Threatened                |
|           Glossopetalon texense           |      Texas greasebush       |     6      | Under Review in the Candidate or Petition Process |
| Helianthus occidentalis ssp. plantagineus |      fewleaf sunflower      |     0      | Under Review in the Candidate or Petition Process |
|           Hexalectris revoluta            |      Chisos coral-root      |     12     | Under Review in the Candidate or Petition Process |
|            Lampsilis bracteata            |       Texas fatmucket       |     71     |                     Candidate                     |
|    Lepidomeda mollispinis mollispinis     |      Virgin Spinedace       |     13     | Under Review in the Candidate or Petition Process |
|          Macrhybopsis australis           |        prairie chub         |    185     | Under Review in the Candidate or Petition Process |
|        Notophthalmus meridionalis         |     Black-spotted Newt      |    288     | Under Review in the Candidate or Petition Process |
|           Notropis perpallidus            |       peppered shiner       |    100     | Under Review in the Candidate or Petition Process |
|             Notropis suttkusi             |        rocky shiner         |     88     | Under Review in the Candidate or Petition Process |
|           Orconectes saxatilis            |      Kiamichi crayfish      |     16     | Under Review in the Candidate or Petition Process |
|            Oreohelix pilsbryi             | Mineral Creek mountainsnail |     11     | Under Review in the Candidate or Petition Process |
|             Papaipema eryngii             |  Rattlesnake Master Borer   |     44     |                     Candidate                     |
|            Paronychia congesta            |     bushy whitlow-wort      |     4      | Under Review in the Candidate or Petition Process |
|          Pediomelum pentaphyllum          |   small Indian breadroot    |     30     | Under Review in the Candidate or Petition Process |
|              Percina nasuta               |       longnose darter       |    147     | Under Review in the Candidate or Petition Process |
|           Phreatodrobia imitata           |       mimic cavesnail       |     2      | Under Review in the Candidate or Petition Process |
|           Physostegia correllii           | Correll's false dragonhead  |     41     | Under Review in the Candidate or Petition Process |
|            Pituophis ruthveni             |    Louisiana Pine Snake     |    120     |                Proposed Threatened                |
|           Pleurobema riddellii            |      Louisiana pigtoe       |     81     | Under Review in the Candidate or Petition Process |
|             Popenaias popeii              |       Texas hornshell       |     65     |                Proposed Endangered                |
|          Potamilus amphichaenus           |     Texas heelsplitter      |     69     | Under Review in the Candidate or Petition Process |
|           Potamilus metnecktayi           |        Salina mucket        |     16     | Under Review in the Candidate or Petition Process |
|           Pteronotropis hubbsi            |       bluehead shiner       |     76     | Under Review in the Candidate or Petition Process |
|           Pyrgulopsis arizonae            |     Apache springsnail      |     35     | Under Review in the Candidate or Petition Process |
|            Pyrgulopsis bacchus            |   Grand Wash springsnail    |     8      | Under Review in the Candidate or Petition Process |
|            Pyrgulopsis conica             |     Kingman springsnail     |     24     | Under Review in the Candidate or Petition Process |
|           Salvia penstemonoides           |        big red sage         |     11     | Under Review in the Candidate or Petition Process |
|             Satan eurystomus              |     widemouth blindcat      |     19     | Under Review in the Candidate or Petition Process |
|          Somatochlora margarita           |        Texas Emerald        |     21     | Under Review in the Candidate or Petition Process |
|          Somatochlora ozarkensis          |        Ozark Emerald        |     27     | Under Review in the Candidate or Petition Process |
|           Sonorella allynsmithi           |    Squaw Peak Talussnail    |     7      | Under Review in the Candidate or Petition Process |
|             Sonorella eremita             |    San Xavier talussnail    |     18     | Under Review in the Candidate or Petition Process |
|           Sonorella grahamensis           |     Pinaleno talussnail     |     13     | Under Review in the Candidate or Petition Process |
|          Sonorella macrophallus           |    Wet Canyon talussnail    |     8      | Under Review in the Candidate or Petition Process |
|          Sonorella magdalenensis          |     Sonoran talussnail      |     11     | Under Review in the Candidate or Petition Process |
|             Sonorella todseni             |     Dona Ana talussnail     |     8      | Under Review in the Candidate or Petition Process |
|              Speyeria idalia              |      Regal Fritillary       |    2181    | Under Review in the Candidate or Petition Process |
|          Streptanthus bracteatus          |     bracted jewelflower     |     51     |                     Candidate                     |
| Symphyotrichum puniceum var. scabricaule  |      purplestem aster       |     0      | Under Review in the Candidate or Petition Process |
|        Tamias minimus atristriatus        |   Penñasco least chipmunk   |     10     |                     Candidate                     |
|          Trogloglanis pattersoni          |     toothless blindcat      |     32     | Under Review in the Candidate or Petition Process |
|             Truncilla cognata             |      Mexican fawnsfoot      |     25     | Under Review in the Candidate or Petition Process |
|            Truncilla macrodon             |       Texas fawnsfoot       |     99     |                     Candidate                     |
|               Tryonia gilae               |        Gila tryonia         |     43     | Under Review in the Candidate or Petition Process |
|           Tryonia quitobaquitae           |    Quitobaquito tryonia     |     16     | Under Review in the Candidate or Petition Process |
|        Tympanuchus pallidicinctus         |   Lesser Prairie-Chicken    |    2851    | Under Review in the Candidate or Petition Process |
|                    Uma                    |     Fringe-toed Lizards     |     74     | Under Review in the Candidate or Petition Process |
|           Vermivora chrysoptera           |    Golden-winged Warbler    |   60040    | Under Review in the Candidate or Petition Process |

# FWS Region 3 - Illinois, Indiana, Iowa, Ohio, Michigan, Minnesota, Missouri, and Wisconsin


```r
FWSRegion3_CPlist_tab <- FWSRegion3_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion3_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|         ScientificName         |          Common Name          | bisontotal |                   ListingStatus                   |
|:------------------------------:|:-----------------------------:|:----------:|:-------------------------------------------------:|
|       Amblyscirtes linda       |   Linda's Roadside-Skipper    |     53     | Under Review in the Candidate or Petition Process |
|       Ambystoma barbouri       |     Streamside Salamander     |    493     | Under Review in the Candidate or Petition Process |
|         Aneides aeneus         |       Green Salamander        |    1922    | Under Review in the Candidate or Petition Process |
|        Bombus terricola        |   Yellow banded bumble bee    |   10547    | Under Review in the Candidate or Petition Process |
|        Catinella gelida        | A land snail (no common name) |     0      | Under Review in the Candidate or Petition Process |
|  Cryptobranchus alleganiensis  |          Hellbender           |    1861    | Under Review in the Candidate or Petition Process |
|       Cyprogenia aberti        |       western fanshell        |    298     | Under Review in the Candidate or Petition Process |
| Deirochelys reticularia miaria |    Western Chicken Turtle     |     57     | Under Review in the Candidate or Petition Process |
|      Emydoidea blandingii      |       Blanding's Turtle       |    5189    | Under Review in the Candidate or Petition Process |
|        Erimystax harryi        |          Ozark chub           |    193     | Under Review in the Candidate or Petition Process |
|       Eurycea tynerensis       |      Oklahoma Salamander      |    1033    | Under Review in the Candidate or Petition Process |
|      Eurytides marcellus       |       Zebra Swallowtail       |    2830    | Under Review in the Candidate or Petition Process |
|      Fusconaia subrotunda      |          long-solid           |    783     | Under Review in the Candidate or Petition Process |
|      Leptoxis arkansensis      |       Arkansas mudalia        |     14     | Under Review in the Candidate or Petition Process |
|       Notropis ariommus        |         popeye shiner         |    322     | Under Review in the Candidate or Petition Process |
|       Notropis ozarcanus       |         Ozark shiner          |    220     | Under Review in the Candidate or Petition Process |
|      Obovaria subrotunda       |       round hickorynut        |    908     | Under Review in the Candidate or Petition Process |
|      Orconectes eupunctus      |      coldwater crayfish       |     53     | Under Review in the Candidate or Petition Process |
|      Orconectes marchandi      |     Sharp River crayfish      |     32     | Under Review in the Candidate or Petition Process |
|       Papaipema eryngii        |   Rattlesnake Master Borer    |     44     |                     Candidate                     |
|      Percina cymatotaenia      |       bluestripe darter       |    144     | Under Review in the Candidate or Petition Process |
|      Percina macrocephala      |        longhead darter        |    376     | Under Review in the Candidate or Petition Process |
|         Percina nasuta         |        longnose darter        |    147     | Under Review in the Candidate or Petition Process |
|       Pleurobema rubrum        |        pyramid pigtoe         |    561     | Under Review in the Candidate or Petition Process |
|      Simpsonaias ambigua       |       salamander mussel       |    349     | Under Review in the Candidate or Petition Process |
|    Somatochlora ozarkensis     |         Ozark Emerald         |     27     | Under Review in the Candidate or Petition Process |
|        Speyeria idalia         |       Regal Fritillary        |    2181    | Under Review in the Candidate or Petition Process |
| Spilogale putorius interrupta  |     Plains spotted skunk      |    651     | Under Review in the Candidate or Petition Process |
|      Thamnophis sauritus       |     Eastern Ribbon Snake      |    3531    | Under Review in the Candidate or Petition Process |
|     Vermivora chrysoptera      |     Golden-winged Warbler     |   60040    | Under Review in the Candidate or Petition Process |

# FWS Region 4 - Alabama, Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, Puerto Rico, and the U.S. Virgin Islands


```r
FWSRegion4_CPlist_tab <- FWSRegion4_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion4_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|           ScientificName            |                  Common Name                  | bisontotal |                   ListingStatus                   |
|:-----------------------------------:|:---------------------------------------------:|:----------:|:-------------------------------------------------:|
|           Agarodes logani           |          Logan's Agarodes Caddisfly           |     1      | Under Review in the Candidate or Petition Process |
|         Alasmidonta arcula          |              Altamaha arcmussel               |     94     | Under Review in the Candidate or Petition Process |
|       Alasmidonta triangulata       |                southern elktoe                |     75     | Under Review in the Candidate or Petition Process |
|        Alasmidonta varicosa         |                 brook floater                 |    630     | Under Review in the Candidate or Petition Process |
|         Allocapnia brooksi          |                Sevier Snowfly                 |     7      | Under Review in the Candidate or Petition Process |
|       Allocapnia cunninghami        |                 Karst Snowfly                 |     41     | Under Review in the Candidate or Petition Process |
|          Allocapnia fumosa          |                Smokies Snowfly                |     6      | Under Review in the Candidate or Petition Process |
|         Amblyopsis spelaea          |               northern cavefish               |    292     | Under Review in the Candidate or Petition Process |
|         Amblyscirtes linda          |           Linda's Roadside-Skipper            |     53     | Under Review in the Candidate or Petition Process |
|         Ambystoma barbouri          |             Streamside Salamander             |    493     | Under Review in the Candidate or Petition Process |
| Ammodramus maritimus macgillivraii  |        Macgillivray's Seaside Sparrow         |    236     | Under Review in the Candidate or Petition Process |
|  Amorpha georgiana var. georgiana   |             Georgia false indigo              |     0      | Under Review in the Candidate or Petition Process |
|        Amphinemura mockfordi        |              Tennessee Forestfly              |     1      | Under Review in the Candidate or Petition Process |
|          Amphiuma pholeter          |               One-toed Amphiuma               |    329     | Under Review in the Candidate or Petition Process |
|           Aneides aeneus            |               Green Salamander                |    1922    | Under Review in the Candidate or Petition Process |
|        Anodontoides radiatus        |               rayed creekshell                |    272     | Under Review in the Candidate or Petition Process |
|          Antrorbis breweri          |               Manitou Cavesnail               |     12     | Under Review in the Candidate or Petition Process |
|        Aphaostracon asthenes        |              Blue Spring hydrobe              |     6      | Under Review in the Candidate or Petition Process |
|      Aphaostracon chalarogyrus      |               freemouth hydrobe               |     9      | Under Review in the Candidate or Petition Process |
|         Aphaostracon monas          |                Wekiwa hydrobe                 |     10     | Under Review in the Candidate or Petition Process |
|         Aphaostracon pycnum         |                 dense hydrobe                 |     0      | Under Review in the Candidate or Petition Process |
|     Aphaostracon theiocrenetum      |            Clifton Spring hydrobe             |     13     | Under Review in the Candidate or Petition Process |
|      Arnoglossum diversifolium      |         variableleaf Indian plantain          |     85     | Under Review in the Candidate or Petition Process |
|         Automeris louisiana         |            Louisiana Eyed Silkmoth            |     12     | Under Review in the Candidate or Petition Process |
|        Balduina atropurpurea        |           purpledisk honeycombhead            |    140     | Under Review in the Candidate or Petition Process |
|         Baptisia megacarpa          |           Apalachicola wild indigo            |     68     | Under Review in the Candidate or Petition Process |
|           Bartonia texana           |                Texas screwstem                |     35     | Under Review in the Candidate or Petition Process |
|          Bombus terricola           |           Yellow banded bumble bee            |   10547    | Under Review in the Candidate or Petition Process |
|        Bouchardina robisoni         |             Bayou Bodcau Crayfish             |     13     | Under Review in the Candidate or Petition Process |
|         Calamovilfa arcuata         |             Cumberland Sandgrass              |    154     | Under Review in the Candidate or Petition Process |
|         Cambarellus blacki          |               cypress crayfish                |     7      | Under Review in the Candidate or Petition Process |
|        Cambarellus diminutus        |                least caryfish                 |     30     | Under Review in the Candidate or Petition Process |
|         Cambarellus lesliei         |            Angular Dwarf Crayfish             |     20     | Under Review in the Candidate or Petition Process |
|         Cambarus bouchardi          |            Big South Fork crayfish            |     37     | Under Review in the Candidate or Petition Process |
|        Cambarus chaugaensis         |                Chauga crayfish                |     74     | Under Review in the Candidate or Petition Process |
|        Cambarus coosawattae         |             Coosawattae crayfish              |     48     | Under Review in the Candidate or Petition Process |
|          Cambarus cracens           |             Slenderclaw Crayfish              |     10     | Under Review in the Candidate or Petition Process |
|        Cambarus cryptodytes         |         Dougherty Plain cave crayfish         |     71     | Under Review in the Candidate or Petition Process |
|         Cambarus cymatilis          |            Conasauga Blue Burrower            |     18     | Under Review in the Candidate or Petition Process |
|        Cambarus eeseeohensis        |         Grandfather Mountain Crayfish         |     0      | Under Review in the Candidate or Petition Process |
|         Cambarus extraneus          |             Chickamauga crayfish              |     75     | Under Review in the Candidate or Petition Process |
|         Cambarus fasciatus          |                Etowah crayfish                |     60     | Under Review in the Candidate or Petition Process |
|          Cambarus georgiae          |           Little Tennessee crayfish           |     52     | Under Review in the Candidate or Petition Process |
|           Cambarus harti            |            piedmont blue burrower             |     17     | Under Review in the Candidate or Petition Process |
|           Cambarus jonesi           |             Alabama cave crayfish             |     34     | Under Review in the Candidate or Petition Process |
|         Cambarus obeyensis          |                 Obey crayfish                 |     30     | Under Review in the Candidate or Petition Process |
|          Cambarus parrishi          |          Hiwassee headwater crayfish          |     34     | Under Review in the Candidate or Petition Process |
|         Cambarus pristinus          |               pristine crayfish               |     57     | Under Review in the Candidate or Petition Process |
|           Cambarus scotti           |               Chatooga crayfish               |     48     | Under Review in the Candidate or Petition Process |
|          Cambarus spicatus          |          Broad River Spiny Crayfish           |     30     | Under Review in the Candidate or Petition Process |
|         Cambarus strigosus          |                 Lean Crayfish                 |     25     | Under Review in the Candidate or Petition Process |
|          Cambarus unestami          |             Blackbarred Crayfish              |     34     | Under Review in the Candidate or Petition Process |
|          Cambarus williami          |            Brawleys Fork crayfish             |     57     | Under Review in the Candidate or Petition Process |
|         Cordulegaster sayi          |                Say's Spiketail                |     47     | Under Review in the Candidate or Petition Process |
|       Coreopsis integrifolia        |              fringeleaf tickseed              |     52     | Under Review in the Candidate or Petition Process |
|        Crangonyx grandimanus        |           Florida cave crangonyctid           |     23     | Under Review in the Candidate or Petition Process |
|          Crangonyx hobbsi           |            Hobbs cave crangonyctid            |     45     | Under Review in the Candidate or Petition Process |
|         Crotalus adamanteus         |        Eastern Diamondback Rattlesnake        |    2391    | Under Review in the Candidate or Petition Process |
|    Cryptobranchus alleganiensis     |                  Hellbender                   |    1861    | Under Review in the Candidate or Petition Process |
|       Cyprinella callitaenia        |               bluestripe shiner               |    231     | Under Review in the Candidate or Petition Process |
|         Cyprinella xaenura          |                Altamaha shiner                |    183     | Under Review in the Candidate or Petition Process |
|          Cyprogenia aberti          |               western fanshell                |    298     | Under Review in the Candidate or Petition Process |
|   Deirochelys reticularia miaria    |            Western Chicken Turtle             |     57     | Under Review in the Candidate or Petition Process |
|        Desmognathus abditus         |          Cumberland Dusky Salamander          |    103     | Under Review in the Candidate or Petition Process |
|         Desmognathus aeneus         |              Seepage Salamander               |    1058    | Under Review in the Candidate or Petition Process |
|     Diadophis punctatus acricus     |              Key Ringneck Snake               |     19     | Under Review in the Candidate or Petition Process |
|       Distocambarus carlsoni        |                mimic crayfish                 |     52     | Under Review in the Candidate or Petition Process |
|        Distocambarus devexus        |        Broad River Burrowing Crayfish         |     20     | Under Review in the Candidate or Petition Process |
|      Distocambarus youngineri       |          Newberry Burrowing Crayfish          |     33     | Under Review in the Candidate or Petition Process |
|          Elassoma boehlkei          |            Carolina pygmy sunfish             |     81     | Under Review in the Candidate or Petition Process |
|            Elimia acuta             |                 acute elimia                  |    101     | Under Review in the Candidate or Petition Process |
|            Elimia ampla             |                 ample elimia                  |     92     | Under Review in the Candidate or Petition Process |
|           Elimia annettae           |               Lilyshoals elimia               |     68     | Under Review in the Candidate or Petition Process |
|         Elimia arachnoidea          |                 spider elimia                 |    111     | Under Review in the Candidate or Petition Process |
|         Elimia bellacrenata         |                princess elimia                |     37     | Under Review in the Candidate or Petition Process |
|          Elimia cochliaris          |                 Cockle Elimia                 |     16     | Under Review in the Candidate or Petition Process |
|           Elimia lachryma           |                Teardrop Elimia                |     54     | Under Review in the Candidate or Petition Process |
|          Elimia melanoides          |                 Black Mudalia                 |     0      | Under Review in the Candidate or Petition Process |
|           Elimia nassula            |               round-rib elimia                |     58     | Under Review in the Candidate or Petition Process |
|          Elimia perstriata          |                engraved elimia                |     48     | Under Review in the Candidate or Petition Process |
|            Elimia teres             |                elegant elimia                 |     20     | Under Review in the Candidate or Petition Process |
|         Elimia vanuxemiana          |                 cobble elimia                 |    253     | Under Review in the Candidate or Petition Process |
|            Elliptio arca            |                 Alabama spike                 |    229     | Under Review in the Candidate or Petition Process |
|          Elliptio arctata           |                delicate spike                 |    649     | Under Review in the Candidate or Petition Process |
|          Elliptio fraterna          |                 brother spike                 |     47     | Under Review in the Candidate or Petition Process |
|         Elliptio lanceolata         |                 yellow lance                  |    392     |                Proposed Threatened                |
|         Elliptio purpurella         |                Inflated Spike                 |    110     | Under Review in the Candidate or Petition Process |
|         Etheostoma bellator         |                Warrior darter                 |    109     | Under Review in the Candidate or Petition Process |
|       Etheostoma brevirostrum       |                holiday darter                 |    147     | Under Review in the Candidate or Petition Process |
|         Etheostoma cinereum         |                  ashy darter                  |    330     | Under Review in the Candidate or Petition Process |
|         Etheostoma forbesi          |                Barrens darter                 |     61     | Under Review in the Candidate or Petition Process |
|       Etheostoma microlepidum       |               smallscale darter               |     77     | Under Review in the Candidate or Petition Process |
|      Etheostoma pallididorsum       |                paleback darter                |     76     | Under Review in the Candidate or Petition Process |
|      Etheostoma pseudovulatum       |               egg-mimic darter                |     67     | Under Review in the Candidate or Petition Process |
|        Etheostoma striatulum        |                striated darter                |     84     | Under Review in the Candidate or Petition Process |
|        Etheostoma tecumsehi         |                Shawnee darter                 |     26     | Under Review in the Candidate or Petition Process |
|         Etheostoma trisella         |                trispot darter                 |    183     |                Proposed Threatened                |
|        Etheostoma tuscumbia         |               Tuscumbia darter                |    256     | Under Review in the Candidate or Petition Process |
|       Euphyes dukesi calhouni       |               Calhoun's Skipper               |     8      | Under Review in the Candidate or Petition Process |
|       Euphyes pilatka klotsi        |                Klots' Skipper                 |     1      | Under Review in the Candidate or Petition Process |
|        Eurybia saxicastellii        |               rockcastle aster                |     62     | Under Review in the Candidate or Petition Process |
|        Eurycea chamberlaini         |        Chamberlain's Dwarf Salamander         |     78     | Under Review in the Candidate or Petition Process |
|         Eurycea tynerensis          |              Oklahoma Salamander              |    1033    | Under Review in the Candidate or Petition Process |
|        Fallicambarus burrisi        |            burrowing bog crayfish             |     11     | Under Review in the Candidate or Petition Process |
|       Fallicambarus danielae        |          speckled burrowing crayfish          |     24     | Under Review in the Candidate or Petition Process |
|        Fallicambarus gilpini        |           Jefferson County crayfish           |     10     | Under Review in the Candidate or Petition Process |
|         Fallicambarus harpi         |          Ouachita burrowing crayfish          |     22     | Under Review in the Candidate or Petition Process |
|        Fallicambarus hortoni        |          Hatchie burrowing crayfish           |     16     | Under Review in the Candidate or Petition Process |
|     Fallicambarus petilicarpus      |        slenderwrist burrowing crayfish        |     9      | Under Review in the Candidate or Petition Process |
|        Fallicambarus strawni        |           saline burrowing crayfish           |     30     | Under Review in the Candidate or Petition Process |
|       Fimbristylis perpusilla       |                Harper's fimbry                |     84     | Under Review in the Candidate or Petition Process |
|          Fissidens hallii           |              Hall's Pocket Moss               |     5      | Under Review in the Candidate or Petition Process |
|           Floridobia mica           |             Ichetucknee Siltsnail             |     8      | Under Review in the Candidate or Petition Process |
|         Forestiera godfreyi         |             Godfrey's swampprivet             |     63     | Under Review in the Candidate or Petition Process |
|          Fundulus jenkinsi          |              saltmarsh topminnow              |    117     | Under Review in the Candidate or Petition Process |
|          Fundulus julisia           |               Barrens topminnow               |    165     | Under Review in the Candidate or Petition Process |
|         Gomphus consanguis          |               Cherokee Clubtail               |     61     | Under Review in the Candidate or Petition Process |
|          Gomphus sandrius           |              Tennessee Clubtail               |     34     | Under Review in the Candidate or Petition Process |
|           Gomphus septima           |              Septima's Clubtail               |     51     | Under Review in the Candidate or Petition Process |
|          Gomphus westfalli          |              Westfall's Clubtail              |     28     | Under Review in the Candidate or Petition Process |
|         Gopherus polyphemus         |                Gopher Tortoise                |    4493    |                     Candidate                     |
|          Graptemys ernsti           |              Escambia Map Turtle              |     97     | Under Review in the Candidate or Petition Process |
|         Graptemys gibbonsi          |             Pascagoula Map Turtle             |     90     | Under Review in the Candidate or Petition Process |
|         Graptemys nigrinoda         |           Black-knobbed Map Turtle            |    120     | Under Review in the Candidate or Petition Process |
|          Graptemys pulchra          |              Alabama Map Turtle               |    247     | Under Review in the Candidate or Petition Process |
|      Gyrinophilus gulolineatus      |             Berry Cave Salamander             |     11     |                     Candidate                     |
|       Gyrinophilus palleucus        |           Tennessee Cave Salamander           |    244     | Under Review in the Candidate or Petition Process |
|       Hartwrightia floridana        |             Florida hartwrightia              |    171     | Under Review in the Candidate or Petition Process |
|           Heterodon simus           |           Southern Hog-nosed Snake            |    1151    | Under Review in the Candidate or Petition Process |
|         Hobbseus cristatus          |           Crested Rivulet Crayfish            |     2      | Under Review in the Candidate or Petition Process |
|       Hobbseus orconectoides        |          Oktibbeha riverlet crayfish          |     41     | Under Review in the Candidate or Petition Process |
|          Hobbseus petilus           |          Tombigbee riverlet crayfish          |     24     | Under Review in the Candidate or Petition Process |
|       Hobbseus yalobushensis        |          Yalobusha riverlet crayfish          |     6      | Under Review in the Candidate or Petition Process |
|         Hydroptila sykorai          |         Sykora's Hydroptila Caddisfly         |     0      | Under Review in the Candidate or Petition Process |
|            Io fluvialis             |               spiny riversnail                |    878     | Under Review in the Candidate or Petition Process |
|          Isoetes hyemalis           |              evergreen quillwort              |     43     | Under Review in the Candidate or Petition Process |
|        Lampsilis fullerkati         |              Waccamaw fatmucket               |    113     | Under Review in the Candidate or Petition Process |
|         Lasmigona holstonia         |            Tennessee heelsplitter             |    381     | Under Review in the Candidate or Petition Process |
|         Lepidostoma morsei          |       Morse's Little Plain Brown Sedge        |     2      | Under Review in the Candidate or Petition Process |
|           Leptoxis picta            |               spotted rocksnail               |     91     | Under Review in the Candidate or Petition Process |
|          Leptoxis virgata           |                smooth mudalia                 |     52     | Under Review in the Candidate or Petition Process |
|          Leuctra szczytkoi          |              Louisiana Needlefly              |     6      | Under Review in the Candidate or Petition Process |
|         Libellula jesseana          |                Purple Skimmer                 |     13     | Under Review in the Candidate or Petition Process |
|         Lindera subcoriacea         |                 bog spicebush                 |    178     | Under Review in the Candidate or Petition Process |
|           Lithasia curta            |               knobby rocksnail                |     20     | Under Review in the Candidate or Petition Process |
|         Lithasia duttoniana         |               helmet rocksnail                |    174     | Under Review in the Candidate or Petition Process |
|          Lithobates capito          |                  Gopher Frog                  |    691     | Under Review in the Candidate or Petition Process |
|          Lobelia boykinii           |               Boykin's lobelia                |    173     | Under Review in the Candidate or Petition Process |
|          Ludwigia brevipes          |          Long Beach primrose-willow           |     66     | Under Review in the Candidate or Petition Process |
|         Ludwigia spathulata         |             spoon primrose-willow             |     87     | Under Review in the Candidate or Petition Process |
|          Lythrum curtissii          |             Curtiss' loosestrife              |     57     | Under Review in the Candidate or Petition Process |
|        Macbridea caroliniana        |           Carolina birds-in-a-nest            |    111     | Under Review in the Candidate or Petition Process |
|         Macromia margarita          |            Mountain River Cruiser             |     24     | Under Review in the Candidate or Petition Process |
|       Marshallia grandiflora        |         Monongahela Barbara's buttons         |    175     | Under Review in the Candidate or Petition Process |
|         Marstonia agarhecta         |              Ocmulgee Marstonia               |     8      | Under Review in the Candidate or Petition Process |
|          Marstonia castor           |             Beaverpond Marstonia              |     7      | Under Review in the Candidate or Petition Process |
|        Medionidus conradicus        |           Cumberland moccasinshell            |    533     | Under Review in the Candidate or Petition Process |
|       Megaceros aenigmaticus        |              Headwaters Hornwort              |    124     | Under Review in the Candidate or Petition Process |
|       Megaleuctra williamsae        |               Smokies Needlefly               |     5      | Under Review in the Candidate or Petition Process |
|         Minuartia godfreyi          |             Godfrey's stitchwort              |     38     | Under Review in the Candidate or Petition Process |
|         Moxostoma robustum          |               smallfin redhorse               |    248     | Under Review in the Candidate or Petition Process |
|           Najas filifolia           |             needleleaf waternymph             |     45     | Under Review in the Candidate or Petition Process |
|        Necturus alabamensis         |            Black Warrior Waterdog             |    582     |                Proposed Endangered                |
|           Necturus lewisi           |             Neuse River Waterdog              |    162     | Under Review in the Candidate or Petition Process |
|      Notophthalmus perstriatus      |                 Striped Newt                  |    1408    |                     Candidate                     |
|          Notropis ariommus          |                 popeye shiner                 |    322     | Under Review in the Candidate or Petition Process |
|         Notropis ozarcanus          |                 Ozark shiner                  |    220     | Under Review in the Candidate or Petition Process |
|        Notropis perpallidus         |                peppered shiner                |    100     | Under Review in the Candidate or Petition Process |
|          Notropis suttkusi          |                 rocky shiner                  |     88     | Under Review in the Candidate or Petition Process |
|          Noturus fasciatus          |                Saddled Madtom                 |    101     | Under Review in the Candidate or Petition Process |
|          Noturus furiosus           |                Carolina madtom                |    128     | Under Review in the Candidate or Petition Process |
|          Noturus gilberti           |               orangefin madtom                |    100     | Under Review in the Candidate or Petition Process |
|          Noturus gladiator          |                Piebald Madtom                 |     46     | Under Review in the Candidate or Petition Process |
|          Noturus lachneri           |                Ouachita madtom                |    101     | Under Review in the Candidate or Petition Process |
|           Noturus munitus           |              frecklebelly madtom              |    359     | Under Review in the Candidate or Petition Process |
|           Noturus taylori           |                 Caddo madtom                  |     43     | Under Review in the Candidate or Petition Process |
|          Obovaria unicolor          |              Alabama hickorynut               |    230     | Under Review in the Candidate or Petition Process |
|            Oecetis parva            |      Little Oecetis Longhorned Caddisfly      |     14     | Under Review in the Candidate or Petition Process |
|       Ophiogomphus australis        |              Southern Snaketail               |     9      | Under Review in the Candidate or Petition Process |
|        Ophiogomphus edmundo         |              Edmund's Snaketail               |     16     | Under Review in the Candidate or Petition Process |
|       Ophiogomphus incurvatus       |             Appalachian Snaketail             |     8      | Under Review in the Candidate or Petition Process |
|          Orconectes blacki          |              Calcasieu crayfish               |     13     | Under Review in the Candidate or Petition Process |
|        Orconectes eupunctus         |              coldwater crayfish               |     53     | Under Review in the Candidate or Petition Process |
|        Orconectes hartfieldi        |                Yazoo Crayfish                 |     6      | Under Review in the Candidate or Petition Process |
|        Orconectes incomptus         |            Tennessee cave crayfish            |     22     | Under Review in the Candidate or Petition Process |
|          Orconectes jonesi          |          Sucarnoochee River crayfish          |     29     | Under Review in the Candidate or Petition Process |
|         Orconectes maletae          |          Kisatchie Painted Crawfish           |     14     | Under Review in the Candidate or Petition Process |
|        Orconectes marchandi         |             Sharp River crayfish              |     32     | Under Review in the Candidate or Petition Process |
|         Orconectes packardi         |           Appalachian Cave Crayfish           |     0      | Under Review in the Candidate or Petition Process |
|         Orconectes sheltae          |             Shelta Cave crayfish              |     4      | Under Review in the Candidate or Petition Process |
|       Orconectes virginiensis       |              Chowanoke crayfish               |     38     | Under Review in the Candidate or Petition Process |
|         Orconectes wrighti          |                Hardin crayfish                |     41     | Under Review in the Candidate or Petition Process |
|         Oryzomys palustris          |                Marsh Rice Rat                 |    3310    | Under Review in the Candidate or Petition Process |
|          Oxyethira setosa           | Setose Cream And Brown Mottled Microcaddisfly |     4      | Under Review in the Candidate or Petition Process |
|          Papaipema eryngii          |           Rattlesnake Master Borer            |     44     |                     Candidate                     |
|         Percina brevicauda          |                  coal darter                  |     79     | Under Review in the Candidate or Petition Process |
|           Percina crypta            |               Halloween darter                |     57     | Under Review in the Candidate or Petition Process |
|            Percina kusha            |                Bridled Darter                 |     97     | Under Review in the Candidate or Petition Process |
|        Percina macrocephala         |                longhead darter                |    376     | Under Review in the Candidate or Petition Process |
|           Percina nasuta            |                longnose darter                |    147     | Under Review in the Candidate or Petition Process |
|            Percina sipsi            |                Bankhead Darter                |     43     | Under Review in the Candidate or Petition Process |
|   Pituophis melanoleucus mugitus    |              Florida Pine Snake               |    480     | Under Review in the Candidate or Petition Process |
|         Pituophis ruthveni          |             Louisiana Pine Snake              |    120     |                Proposed Threatened                |
|       Plagiochila caduciloba        |         Brittle-Lobed Leafy Liverwort         |    113     | Under Review in the Candidate or Petition Process |
|         Plagiochila sharpii         |                 "A Liverwort"                 |     88     | Under Review in the Candidate or Petition Process |
|        Planorbella magnifica        |             magnificent rams-horn             |     12     |                     Candidate                     |
|    Plestiodon egregius insularis    |             Cedar Key Mole Skink              |     0      | Under Review in the Candidate or Petition Process |
|         Pleurobema athearni         |             Canoe Creek Clubshell             |     5      | Under Review in the Candidate or Petition Process |
|         Pleurobema oviforme         |              Tennessee clubshell              |    613     | Under Review in the Candidate or Petition Process |
|         Pleurobema rubellum         |                Warrior pigtoe                 |     85     | Under Review in the Candidate or Petition Process |
|        Pleurocera corpulenta        |              corpulent hornsnail              |     45     | Under Review in the Candidate or Petition Process |
|          Pleurocera curta           |             shortspire hornsnail              |    294     | Under Review in the Candidate or Petition Process |
|        Pleurocera pyrenella         |               skirted hornsnail               |    229     | Under Review in the Candidate or Petition Process |
|        Pleuronaia barnesiana        |               Tennessee pigtoe                |    474     | Under Review in the Candidate or Petition Process |
|       Potamilus amphichaenus        |              Texas heelsplitter               |     69     | Under Review in the Candidate or Petition Process |
|      Potamogeton tennesseensis      |              Tennessee pondweed               |    116     | Under Review in the Candidate or Petition Process |
|          Problema bulenta           |                 Rare Skipper                  |     71     | Under Review in the Candidate or Petition Process |
|       Procambarus acherontis        |             Orlando cave crayfish             |     15     | Under Review in the Candidate or Petition Process |
|      Procambarus apalachicolae      |          coastal flatwoods crayfish           |     14     | Under Review in the Candidate or Petition Process |
|        Procambarus attiguus         |         Silver Glen Springs crayfish          |     9      | Under Review in the Candidate or Petition Process |
|        Procambarus barbiger         |           Jackson prairie crayfish            |     11     | Under Review in the Candidate or Petition Process |
|        Procambarus delicatus        |            bigcheek cave crayfish             |     4      | Under Review in the Candidate or Petition Process |
|        Procambarus econfinae        |             Panama City crayfish              |     50     | Under Review in the Candidate or Petition Process |
|        Procambarus erythrops        |            Santa Fe cave crayfish             |     30     | Under Review in the Candidate or Petition Process |
|      Procambarus fitzpatricki       |              spinytail crayfish               |     26     | Under Review in the Candidate or Petition Process |
|         Procambarus franzi          |           Orange Lake cave crayfish           |     15     | Under Review in the Candidate or Petition Process |
|        Procambarus lagniappe        |              lagniappe crayfish               |     15     | Under Review in the Candidate or Petition Process |
|       Procambarus leitheuseri       |         Coastal Lowland cave crayfish         |     15     | Under Review in the Candidate or Petition Process |
|        Procambarus lucifugus        |             Florida cave crayfish             |     30     | Under Review in the Candidate or Petition Process |
|          Procambarus lylei          |              shutispear crayfish              |     43     | Under Review in the Candidate or Petition Process |
|         Procambarus milleri         |              Miami cave crayfish              |     27     | Under Review in the Candidate or Petition Process |
|         Procambarus morrisi         |          Putnam County cave crayfish          |     14     | Under Review in the Candidate or Petition Process |
|        Procambarus pallidus         |             pallid cave crayfish              |    100     | Under Review in the Candidate or Petition Process |
|         Procambarus pictus          |            spotted royal crayfish             |     17     | Under Review in the Candidate or Petition Process |
|         Procambarus regalis         |           Regal Burrowing Crayfish            |     15     | Under Review in the Candidate or Petition Process |
|         Procambarus reimeri         |         Irons Fork Burrowing Crayfish         |     20     | Under Review in the Candidate or Petition Process |
|       Pseudemys rubriventris        |              red-bellied turtle               |    929     | Under Review in the Candidate or Petition Process |
| Pseudobranchus striatus lustricolus |            Gulf Hammock Mud Siren             |     49     | Under Review in the Candidate or Petition Process |
|         Pterodroma hasitata         |              Black-capped Petrel              |    9914    | Under Review in the Candidate or Petition Process |
|       Pteronotropis euryzonus       |              broadstripe shiner               |    200     | Under Review in the Candidate or Petition Process |
|        Pteronotropis hubbsi         |                bluehead shiner                |     76     | Under Review in the Candidate or Petition Process |
|         Ptilimnium ahlesii          |            coastal mock bishopweed            |     18     | Under Review in the Candidate or Petition Process |
|         Rhexia salicifolia          |            panhandle meadowbeauty             |    173     | Under Review in the Candidate or Petition Process |
|        Rhynchospora crinipes        |              mosquito beaksedge               |     68     | Under Review in the Candidate or Petition Process |
|        Rhynchospora thornei         |              Thorne's beaksedge               |     61     | Under Review in the Candidate or Petition Process |
|        Rudbeckia auriculata         |               eared coneflower                |     59     | Under Review in the Candidate or Petition Process |
|        Rudbeckia heliopsidis        |             sunfacing coneflower              |     96     | Under Review in the Candidate or Petition Process |
|           Salix floridana           |                Florida willow                 |     99     | Under Review in the Candidate or Petition Process |
|   Sarracenia purpurea f. purpurea   |         Mountain Purple Pitcherplant          |     0      | Under Review in the Candidate or Petition Process |
|          Sceloporus woodi           |             Florida Scrub Lizard              |    2770    | Under Review in the Candidate or Petition Process |
|        Scutellaria ocmulgee         |               Ocmulgee skullcap               |     29     | Under Review in the Candidate or Petition Process |
|         Sideroxylon thornei         |                 Georgia bully                 |     77     | Under Review in the Candidate or Petition Process |
|         Solidago arenicola          |          southern racemose goldenrod          |     23     | Under Review in the Candidate or Petition Process |
|        Somatochlora calverti        |               Calvert's Emerald               |     6      | Under Review in the Candidate or Petition Process |
|       Somatochlora margarita        |                 Texas Emerald                 |     21     | Under Review in the Candidate or Petition Process |
|       Somatochlora ozarkensis       |                 Ozark Emerald                 |     27     | Under Review in the Candidate or Petition Process |
|       Somatogyrus alcoviensis       |              reverse pebblesnail              |     12     | Under Review in the Candidate or Petition Process |
|       Sporobolus teretifolius       |              Wire-Leaf Dropseed               |    112     | Under Review in the Candidate or Petition Process |
|        Stellaria fontinalis         |            American water starwort            |    169     | Under Review in the Candidate or Petition Process |
|         Stylurus potulentus         |             Yellow-sided Clubtail             |     2      | Under Review in the Candidate or Petition Process |
|          Thalictrum debile          |              southern meadow-rue              |     52     | Under Review in the Candidate or Petition Process |
|        Toxolasma corvunculus        |           southern purple lilliput            |    118     | Under Review in the Candidate or Petition Process |
|          Toxolasma pullus           |               Savannah lilliput               |     84     | Under Review in the Candidate or Petition Process |
|        Triaenodes tridontus         |                  A Caddisfly                  |     3      | Under Review in the Candidate or Petition Process |
|       Troglocambarus maclanei       |             spider cave crayfish              |     44     | Under Review in the Candidate or Petition Process |
|          Tsuga caroliniana          |               Carolina hemlock                |    711     | Under Review in the Candidate or Petition Process |
|         Urspelerpes brucei          |            Patch-nosed Salamander             |     0      | Under Review in the Candidate or Petition Process |
|         Utterbackia heardi          |             Apalachicola floater              |     0      | Under Review in the Candidate or Petition Process |
|        Vermivora chrysoptera        |             Golden-winged Warbler             |   60040    | Under Review in the Candidate or Petition Process |
|          Villosa nebulosa           |                Alabama rainbow                |    305     | Under Review in the Candidate or Petition Process |
|          Villosa ortmanni           |              Kentucky creekshell              |    149     | Under Review in the Candidate or Petition Process |
|         Waldsteinia lobata          |          piedmont barren strawberry           |     99     | Under Review in the Candidate or Petition Process |

# FWS Region 5 - Connecticut, Delaware, District of Columbia, Maine, Maryland, Massachusetts, New Hampshire, New Jersey, New York, Pennsylvania, Rhode Island, Vermont, Virginia, and West Virginia


```r
FWSRegion5_CPlist_tab <- FWSRegion5_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion5_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|             ScientificName             |           Common Name           | bisontotal |                   ListingStatus                   |
|:--------------------------------------:|:-------------------------------:|:----------:|:-------------------------------------------------:|
|         Acroneuria kosztarabi          |   Kosztarab's common stonefly   |     5      | Under Review in the Candidate or Petition Process |
|           Allocapnia fumosa            |         Smokies Snowfly         |     6      | Under Review in the Candidate or Petition Process |
|           Ambystoma barbouri           |      Streamside Salamander      |    493     | Under Review in the Candidate or Petition Process |
|             Aneides aeneus             |        Green Salamander         |    1922    | Under Review in the Candidate or Petition Process |
|            Bombus terricola            |    Yellow banded bumble bee     |   10547    | Under Review in the Candidate or Petition Process |
|           Caecidotea cannula           |            An Isopod            |     18     | Under Review in the Candidate or Petition Process |
|        Cambarus chasmodactylus         |       New River crayfish        |     36     | Under Review in the Candidate or Petition Process |
|           Cambarus elkensis            |       Elk River crayfish        |     13     | Under Review in the Candidate or Petition Process |
|          Cambarus jezerinaci           |      Spiny scale crayfish       |     15     | Under Review in the Candidate or Petition Process |
|           Cambarus nerterius           |    Greenbrier cave crayfish     |     31     | Under Review in the Candidate or Petition Process |
|        Cicindela marginipennis         |    Cobblestone Tiger Beetle     |     99     | Under Review in the Candidate or Petition Process |
|      Cryptobranchus alleganiensis      |           Hellbender            |    1861    | Under Review in the Candidate or Petition Process |
|       Danaus plexippus plexippus       |        Monarch Butterfly        |    484     | Under Review in the Candidate or Petition Process |
|           Elimia arachnoidea           |          spider elimia          |    111     | Under Review in the Candidate or Petition Process |
|          Elliptio lanceolata           |          yellow lance           |    392     |                Proposed Threatened                |
|          Etheostoma cinereum           |           ashy darter           |    330     | Under Review in the Candidate or Petition Process |
|           Etheostoma osburni           |          candy darter           |    108     |                Proposed Threatened                |
|          Eurytides marcellus           |        Zebra Swallowtail        |    2830    | Under Review in the Candidate or Petition Process |
|            Fusconaia masoni            |         Atlantic pigtoe         |    259     | Under Review in the Candidate or Petition Process |
|           Gomphus consanguis           |        Cherokee Clubtail        |     61     | Under Review in the Candidate or Petition Process |
|            Gomphus septima             |       Septima's Clubtail        |     51     | Under Review in the Candidate or Petition Process |
|       Gyrinophilus subterraneus        | West Virginia Spring Salamander |     19     | Under Review in the Candidate or Petition Process |
|              Io fluvialis              |        spiny riversnail         |    878     | Under Review in the Candidate or Petition Process |
|          Lasmigona holstonia           |     Tennessee heelsplitter      |    381     | Under Review in the Candidate or Petition Process |
|            Lirceus culveri             |      Rye Cove cave isopod       |     10     | Under Review in the Candidate or Petition Process |
|           Macromia margarita           |     Mountain River Cruiser      |     24     | Under Review in the Candidate or Petition Process |
|         Marshallia grandiflora         |  Monongahela Barbara's buttons  |    175     | Under Review in the Candidate or Petition Process |
|         Medionidus conradicus          |    Cumberland moccasinshell     |    533     | Under Review in the Candidate or Petition Process |
|         Megaleuctra williamsae         |        Smokies Needlefly        |     5      | Under Review in the Candidate or Petition Process |
|           Notropis ariommus            |          popeye shiner          |    322     | Under Review in the Candidate or Petition Process |
|            Noturus gilberti            |        orangefin madtom         |    100     | Under Review in the Candidate or Petition Process |
| Ophiogomphus incurvatus alleghaniensis |       Allegheny snaketail       |     17     | Under Review in the Candidate or Petition Process |
|        Orconectes virginiensis         |       Chowanoke crayfish        |     38     | Under Review in the Candidate or Petition Process |
|          Percina macrocephala          |         longhead darter         |    376     | Under Review in the Candidate or Petition Process |
|          Plethodon hubrichti           |    Peaks of Otter Salamander    |    1662    | Under Review in the Candidate or Petition Process |
|          Pleurobema oviforme           |       Tennessee clubshell       |    613     | Under Review in the Candidate or Petition Process |
|       Potamogeton tennesseensis        |       Tennessee pondweed        |    116     | Under Review in the Candidate or Petition Process |
|            Problema bulenta            |          Rare Skipper           |     71     | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus avernus        |       Avernus cave beetle       |     1      | Under Review in the Candidate or Petition Process |
|     Pseudanophthalmus cordicollis      |   Little Kennedy Cave beetle    |     2      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus egberti        |       Narrows Cave beetle       |     3      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus hirsutus       |       Cudjo's Cave beetle       |     4      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus hubbardi       |      Hubbard's cave beetle      |     4      | Under Review in the Candidate or Petition Process |
|      Pseudanophthalmus hubrichti       |     Hubricht's cave beetle      |     1      | Under Review in the Candidate or Petition Process |
|     Pseudanophthalmus intersectus      |     crossroads cave beetle      |     2      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus limicola       |     Shenandoah cave beetle      |     2      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus montanus       |   Dry Fork Valley cave beetle   |     4      | Under Review in the Candidate or Petition Process |
|        Pseudanophthalmus pontis        |   Natural Bridge cave beetle    |     2      | Under Review in the Candidate or Petition Process |
|      Pseudanophthalmus potomacus       | South Branch Valley cave beetle |     6      | Under Review in the Candidate or Petition Process |
|    Pseudanophthalmus praetermissus     |     overlooked cave beetle      |     1      | Under Review in the Candidate or Petition Process |
|     Pseudanophthalmus sanctipauli      |     Saint Paul cave beetle      |     2      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus sericus        |       silken cave beetle        |     1      | Under Review in the Candidate or Petition Process |
|       Pseudanophthalmus thomasi        |       Thomas' cave beetle       |     2      | Under Review in the Candidate or Petition Process |
|      Pseudanophthalmus virginicus      |    Maiden Spring cave beetle    |     1      | Under Review in the Candidate or Petition Process |
|         Pseudemys rubriventris         |       red-bellied turtle        |    929     | Under Review in the Candidate or Petition Process |
|           Remenus kirchneri            |      Blue Ridge springfly       |     1      | Under Review in the Candidate or Petition Process |
|            Speyeria idalia             |        Regal Fritillary         |    2181    | Under Review in the Candidate or Petition Process |
|          Stygobromus cooperi           |     Cooper's cave amphipod      |     1      | Under Review in the Candidate or Petition Process |
|         Stygobromus indentatus         | Tidewater stygonectid amphipod  |     10     | Under Review in the Candidate or Petition Process |
|         Stygobromus morrisoni          |    Morrison's cave amphipod     |     9      | Under Review in the Candidate or Petition Process |
|           Stygobromus parvus           |      minute cave amphipod       |     7      | Under Review in the Candidate or Petition Process |
|          Synaptomys borealis           |      Northern Bog Lemming       |    2083    | Under Review in the Candidate or Petition Process |
|           Tallaperla lobata            |         Lobed roachfly          |     3      | Under Review in the Candidate or Petition Process |
|          Thamnophis sauritus           |      Eastern Ribbon Snake       |    3531    | Under Review in the Candidate or Petition Process |
|         Typhlogastrura helleri         |     Heller Cave Springtail      |     0      | Under Review in the Candidate or Petition Process |
|         Vermivora chrysoptera          |      Golden-winged Warbler      |   60040    | Under Review in the Candidate or Petition Process |

# FWS Region 6 - Colorado, Kansas, Montana, Nebraska, North Dakota, South Dakota, Utah, and Wyoming


```r
FWSRegion6_CPlist_tab <- FWSRegion6_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion6_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|       ScientificName        |           Common Name            | bisontotal |                   ListingStatus                   |
|:---------------------------:|:--------------------------------:|:----------:|:-------------------------------------------------:|
|     Amblyscirtes linda      |     Linda's Roadside-Skipper     |     53     | Under Review in the Candidate or Petition Process |
| Arsapnia [=Capnia] arapahoe |         Arapahoe snowfly         |     0      |                     Candidate                     |
|   Astragalus microcymbus    |         skiff milkvetch          |     28     |                     Candidate                     |
|    Astragalus schmolliae    |       Schmoll's milkvetch        |     17     |                     Candidate                     |
|      Bombus terricola       |     Yellow banded bumble bee     |   10547    | Under Review in the Candidate or Petition Process |
|      Catinella gelida       |  A land snail (no common name)   |     0      | Under Review in the Candidate or Petition Process |
|    Corispermum navicula     |         crescent bugseed         |     10     | Under Review in the Candidate or Petition Process |
|      Cyprogenia aberti      |         western fanshell         |    298     | Under Review in the Candidate or Petition Process |
|        Draba weberi         |          Weber's draba           |     7      | Under Review in the Candidate or Petition Process |
|    Emydoidea blandingii     |        Blanding's Turtle         |    5189    | Under Review in the Candidate or Petition Process |
|     Eurytides marcellus     |        Zebra Swallowtail         |    2830    | Under Review in the Candidate or Petition Process |
|        Gila robusta         |          roundtail chub          |    1397    |                Proposed Threatened                |
| Lagopus leucura altipetens  | Southern white-tailed ptarmigan  |     39     | Under Review in the Candidate or Petition Process |
|     Pyrgulopsis anguina     |     Longitudinal Gland Pyrg      |     0      | Under Review in the Candidate or Petition Process |
|   Pyrgulopsis hamlinensis   |        Hamlin Valley Pyrg        |     0      | Under Review in the Candidate or Petition Process |
|    Pyrgulopsis saxatilis    |      Sub-globose Snake Pyrg      |     0      | Under Review in the Candidate or Petition Process |
|    Schoenoplectus hallii    |          Hall's bulrush          |    261     | Under Review in the Candidate or Petition Process |
|   Somatochlora ozarkensis   |          Ozark Emerald           |     27     | Under Review in the Candidate or Petition Process |
|       Speyeria idalia       |         Regal Fritillary         |    2181    | Under Review in the Candidate or Petition Process |
|  Speyeria nokomis nokomis   | Great Basin silverspot butterfly |     23     | Under Review in the Candidate or Petition Process |
| Tympanuchus pallidicinctus  |      Lesser Prairie-Chicken      |    2851    | Under Review in the Candidate or Petition Process |

# FWS Region 7 - Alaska


```r
FWSRegion7_CPlist_tab <- FWSRegion7_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion7_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|     ScientificName      |  Common Name  | bisontotal |                   ListingStatus                   |
|:-----------------------:|:-------------:|:----------:|:-------------------------------------------------:|
|   Fratercula cirrhata   | Tufted Puffin |   24084    | Under Review in the Candidate or Petition Process |
| Spirinchus thaleichthys | longfin smelt |    1220    |                     Candidate                     |

# FWS Region 8 - California and Nevada


```r
FWSRegion8_CPlist_tab <- FWSRegion8_CPlist[c("ScientificName","Common Name","bisontotal","ListingStatus")]
FWSRegion8_CPlist_tab %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|           ScientificName           |                  Common Name                  | bisontotal |                   ListingStatus                   |
|:----------------------------------:|:---------------------------------------------:|:----------:|:-------------------------------------------------:|
|         Agelaius tricolor          |             Tricolored Blackbird              |   133372   | Under Review in the Candidate or Petition Process |
|         Batrachoseps campi         |           Inyo Mountains Salamander           |    212     | Under Review in the Candidate or Petition Process |
|         Batrachoseps minor         |           Lesser Slender Salamander           |    270     | Under Review in the Candidate or Petition Process |
|        Batrachoseps simatus        |        Kern Canyon Slender Salamander         |    171     | Under Review in the Candidate or Petition Process |
|          Bombus franklini          |             Franklin's Bumble Bee             |     17     | Under Review in the Candidate or Petition Process |
| Chorizanthe parryi var. fernandina |        San Fernando Valley spineflower        |     0      |                Proposed Threatened                |
|        Elgaria panamintina         |           Panamint Alligator Lizard           |     50     | Under Review in the Candidate or Petition Process |
|        Fratercula cirrhata         |                 Tufted Puffin                 |   24084    | Under Review in the Candidate or Petition Process |
|         Hydromantes brunus         |             Limestone Salamander              |    226     | Under Review in the Candidate or Petition Process |
|        Hydromantes shastae         |               Shasta Salamander               |    507     | Under Review in the Candidate or Petition Process |
|       Lavinia exilicauda chi       |               Clear Lake Hitch                |     0      | Under Review in the Candidate or Petition Process |
| Lepidomeda mollispinis mollispinis |               Virgin Spinedace                |     13     | Under Review in the Candidate or Petition Process |
|           Lycaena hermes           |                 Hermes Copper                 |     85     |                     Candidate                     |
|          Microcina jungi           | Jungâ<U+0080><U+0099>s micro-blind harvestman |     1      | Under Review in the Candidate or Petition Process |
|         Phacelia argentea          |              Sand dune phacelia               |    161     | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis anguina         |            Longitudinal Gland Pyrg            |     0      | Under Review in the Candidate or Petition Process |
|       Pyrgulopsis crystalis        |              Crystal springsnail              |     18     | Under Review in the Candidate or Petition Process |
|      Pyrgulopsis erythropoma       |            Ash Meadows pebblesnail            |     55     | Under Review in the Candidate or Petition Process |
|     Pyrgulopsis fairbanksensis     |             Fairbanks springsnail             |     13     | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis isolata         |          elongate-gland springsnail           |     12     | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis landyei         |                 Landyes pyrg                  |     0      | Under Review in the Candidate or Petition Process |
|         Pyrgulopsis nanus          |           distal-gland springsnail            |     51     | Under Review in the Candidate or Petition Process |
|       Pyrgulopsis neritella        |         neritiform Steptoe Ranch pyrg         |     0      | Under Review in the Candidate or Petition Process |
|       Pyrgulopsis orbiculata       |        sub-globose Steptoe Ranch pyrg         |     0      | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis pisteri         |           median-gland springsnail            |     28     | Under Review in the Candidate or Petition Process |
|       Pyrgulopsis planulata        |           flat-topped Steptoe pyrg            |     0      | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis serrata         |             northern Steptoe pyrg             |     0      | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis sterilis        |              sterile basin pyrg               |     0      | Under Review in the Candidate or Petition Process |
|        Pyrgulopsis sulcata         |             southern Steptoe pyrg             |     0      | Under Review in the Candidate or Petition Process |
|       Pyrgulopsis turbatrix        |             southeast Nevada pyrg             |    178     | Under Review in the Candidate or Petition Process |
|            Rana boylii             |          Foothill Yellow-legged Frog          |    5170    | Under Review in the Candidate or Petition Process |
|           Rana cascadae            |                 Cascades Frog                 |    2643    | Under Review in the Candidate or Petition Process |
|        Relictus solitarius         |                  relict dace                  |     67     | Under Review in the Candidate or Petition Process |
|       Rhaphiomidas trochilus       |               Valley mydas fly                |     1      | Under Review in the Candidate or Petition Process |
|           Spea hammondii           |               Western Spadefoot               |    2701    | Under Review in the Candidate or Petition Process |
|      Spirinchus thaleichthys       |                 longfin smelt                 |    1220    |                     Candidate                     |
|  Strix occidentalis occidentalis   |            California Spotted Owl             |    4125    | Under Review in the Candidate or Petition Process |
|         Trilobopsis roperi         |               Shasta chaparral                |     9      | Under Review in the Candidate or Petition Process |
|           Tryonia elata            |            Point of Rocks tryonia             |     17     | Under Review in the Candidate or Petition Process |
|           Tryonia ericae           |                minute tryonia                 |     17     | Under Review in the Candidate or Petition Process |
|         Tryonia variegata          |               Amargosa tryonia                |    220     | Under Review in the Candidate or Petition Process |
|       Vespericola pressleyi        |               Big Bar hesperian               |     12     | Under Review in the Candidate or Petition Process |
|         Vespericola shasta         |               Shasta hesperian                |     14     | Under Review in the Candidate or Petition Process |
|       Vulpes vulpes necator        |             Sierra Nevada Red Fox             |    273     |                     Candidate                     |

