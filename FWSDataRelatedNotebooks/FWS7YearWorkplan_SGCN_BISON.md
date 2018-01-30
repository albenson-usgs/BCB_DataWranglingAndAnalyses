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

## Plot of the number of occurrences from BISON for the SGCN that are FWS 7 year workplan species


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

## Tables of SGCN + FWS National Working Plan species by proposed time frames
SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2017


```r
fy17 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|       Common Name       |      ScientificName      | ActionType | LeadRO | PriorityBin | bisontotal |
|:-----------------------:|:------------------------:|:----------:|:------:|:-----------:|:----------:|
|     Blue Point pyrg     | Pyrgulopsis coloradensis |    12M     |   R8   |      5      |     0      |
|  Spring Mountains pyrg  |   Pyrgulopsis deaconi    |    12M     |   R8   |      5      |     0      |
|     Corn Creek pyrg     |    Pyrgulopsis fausta    |    12M     |   R8   |      5      |     0      |
|       Hubbs pyrg        |    Pyrgulopsis hubbsi    |    12M     |   R8   |      5      |     0      |
|    Butterfield pyrg     |     Pyrgulopsis lata     |    12M     |   R8   |      5      |     0      |
|       Hardy pyrg        |   Pyrgulopsis marcida    |    12M     |   R8   |     N/A     |     0      |
| White River Valley pyrg |    Pyrgulopsis sathos    |    12M     |   R8   |      5      |     0      |
|    Lake Valley pyrg     |   Pyrgulopsis sublata    |    12M     |   R8   |     N/A     |     0      |
|   San Felipe gambusia   |   Gambusia clarkhubbsi   |    12M     |   R2   |      2      |     45     |
|  Panama City crayfish   |  Procambarus econfinae   |    12M     |   R4   |     N/A     |     50     |
|    Moapa pebblesnail    |  Pyrgulopsis avernalis   |    12M     |   R8   |      5      |     50     |
| Pahranagat pebblesnail  |   Pyrgulopsis merriami   |    12M     |   R8   |      5      |     51     |
|    Moapa Valley pyrg    |  Pyrgulopsis carinifera  |    12M     |   R8   |      5      |     53     |
|     blackfin sucker     |   Thoburnia atripinnis   |    12M     |   R4   |      2      |     89     |
|      candy darter       |    Etheostoma osburni    |    12M     |   R5   |     N/A     |    108     |
|     grated tryonia      |    Tryonia clathrata     |    12M     |   R8   |      5      |    115     |
|     Carolina madtom     |     Noturus furiosus     |    12M     |   R4   |      1      |    128     |
|     Atlantic pigtoe     |     Fusconaia masoni     |    12M     |   R4   |     N/A     |    259     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2018


```r
fy18 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|     Common Name      |     ScientificName     |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:--------------------:|:----------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|   Texas pimpleback   |   Amphinaias petrina   | Proposed Listing or Determination |   R2   |    LPN 2    |     0      |
| Apalachicola floater |   Utterbackia heardi   |                12M                |   R4   |      4      |     0      |
|      Ozark pyrg      | Pyrgulopsis ozarkensis |                12M                |   R4   |      1      |     6      |
|  Elk River crayfish  |   Cambarus elkensis    |                12M                |   R5   |      2      |     13     |
|     false spike      |  Fusconaia mitchelli   |                12M                |   R2   |      1      |     13     |
|   Arkansas mudalia   |  Leptoxis arkansensis  |                12M                |   R4   |      1      |     14     |
|   Texas fatmucket    |  Lampsilis bracteata   | Proposed Listing or Determination |   R2   |    LPN 2    |     71     |
|   Texas fawnsfoot    |   Truncilla macrodon   | Proposed Listing or Determination |   R2   |    LPN 2    |     99     |
|    seaside alder     |     Alnus maritima     |                12M                |   R5   |      2      |    115     |
|  Tippecanoe darter   | Etheostoma tippecanoe  |                12M                |   R5   |      2      |    156     |
|     ashy darter      |  Etheostoma cinereum   |                12M                |   R4   |     N/A     |    330     |
|    brook floater     |  Alasmidonta varicosa  |                12M                |   R5   |      2      |    630     |
|   round hickorynut   |  Obovaria subrotunda   |                12M                |   R4   |      2      |    908     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2019


```r
fy19 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|        Common Name         |       ScientificName        |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:--------------------------:|:---------------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|      Arapahoe snowfly      | Arsapnia [=Capnia] arapahoe | Proposed Listing or Determination |   R6   |    LPN 5    |     0      |
|      Arapahoe snowfly      |       Capnia arapahoe       | Proposed Listing or Determination |   R6   |    LPN 5    |     3      |
|     bushy whitlow-wort     |     Paronychia congesta     |                12M                |   R2   |      3      |     4      |
|  Navasota false foxglove   |    Agalinis navasotensis    |                12M                |   R2   |      3      |     8      |
|   Berry Cave Salamander    |  Gyrinophilus gulolineatus  | Proposed Listing or Determination |   R4   |    LPN 8    |     11     |
|     prostrate milkweed     |     Asclepias prostrata     |                12M                |   R2   |      3      |     12     |
|    bearded red crayfish    |      Procambarus pogum      |                12M                |   R4   |      2      |     16     |
|      triangle pigtoe       |    Fusconaia lananensis     |                12M                |   R2   |      3      |     28     |
|     Ocmulgee skullcap      |    Scutellaria ocmulgee     |                12M                |   R4   |      2      |     29     |
|      Texas screwstem       |       Bartonia texana       |                12M                |   R2   |      3      |     35     |
|     Chowanoke crayfish     |   Orconectes virginiensis   |                12M                |   R5   |      3      |     38     |
|     coldwater crayfish     |    Orconectes eupunctus     |                12M                |   R3   |      3      |     53     |
|     Texas heelsplitter     |   Potamilus amphichaenus    |                12M                |   R2   |      3      |     69     |
| St. Francis River crayfish |    Orconectes quadruncus    |  Discretionary Status or Review   |   R3   |      3      |     73     |
|   Carolina pygmy sunfish   |      Elassoma boehlkei      |                12M                |   R4   |      2      |     81     |
|      Louisiana pigtoe      |    Pleurobema riddellii     |                12M                |   R2   |      3      |     81     |
|     Big Creek crayfish     |     Orconectes peruncus     |  Discretionary Status or Review   |   R3   |      3      |    105     |
|  purpledisk honeycombhead  |    Balduina atropurpurea    |                12M                |   R4   |      2      |    140     |
|         Ozark chub         |      Erimystax harryi       |                12M                |   R4   |      2      |    193     |
|       Hall's bulrush       |    Schoenoplectus hallii    |                12M                |   R3   |      3      |    261     |
|      longhead darter       |    Percina macrocephala     |                12M                |   R5   |     N/A     |    376     |
|      purple lilliput       |      Toxolasma lividum      |                12M                |   R4   |      2      |    656     |
|     monarch butterfly      |      Danaus plexippus       |                12M                |   R3   |     N/A     |   52404    |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2020


```r
fy20 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|           Common Name           |         ScientificName          |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:-------------------------------:|:-------------------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|           golden orb            |        Amphinaias aurea         | Proposed Listing or Determination |   R2   |    LPN 8    |     0      |
|        smooth pimpleback        |     Amphinaias houstonensis     | Proposed Listing or Determination |   R2   |    LPN 8    |     0      |
|       Avernus cave beetle       |    Pseudanophthalmus avernus    |                12M                |   R5   |      3      |     1      |
|     Hubricht's cave beetle      |   Pseudanophthalmus hubrichti   |                12M                |   R5   |      3      |     1      |
|      thin-neck cave beetle      |  Pseudanophthalmus parvicollis  |  Discretionary Status or Review   |   R5   |      3      |     1      |
|     overlooked cave beetle      | Pseudanophthalmus praetermissus |                12M                |   R5   |      3      |     1      |
|       silken cave beetle        |    Pseudanophthalmus sericus    |                12M                |   R5   |      3      |     1      |
|    Maiden Spring cave beetle    |  Pseudanophthalmus virginicus   |                12M                |   R5   |      3      |     1      |
|   Natural Bridge cave beetle    |    Pseudanophthalmus pontis     |                12M                |   R5   |      3      |     2      |
|     Saint Paul cave beetle      |  Pseudanophthalmus sanctipauli  |                12M                |   R5   |      3      |     2      |
|       Thomas' cave beetle       |    Pseudanophthalmus thomasi    |                12M                |   R5   |      3      |     2      |
|      Hubbard's cave beetle      |   Pseudanophthalmus hubbardi    |                12M                |   R5   |      3      |     4      |
|     Holsinger's cave beetle     |  Pseudanophthalmus holsingeri   |                12M                |   R5   |      3      |     5      |
| South Branch Valley cave beetle |   Pseudanophthalmus potomacus   |                12M                |   R5   |      3      |     6      |
|      Wet Canyon talussnail      |     Sonorella macrophallus      |                12M                |   R2   |      4      |     8      |
|   elongate-gland springsnail    |       Pyrgulopsis isolata       |                12M                |   R8   |      5      |     12     |
|      Fairbanks springsnail      |   Pyrgulopsis fairbanksensis    |                12M                |   R8   |      5      |     13     |
|       Pinaleno talussnail       |      Sonorella grahamensis      |                12M                |   R2   |      4      |     13     |
|      Quitobaquito tryonia       |      Tryonia quitobaquitae      |                12M                |   R2   |      4      |     16     |
|     Point of Rocks tryonia      |          Tryonia elata          |                12M                |   R8   |      5      |     17     |
|         minute tryonia          |         Tryonia ericae          |                12M                |   R8   |      5      |     17     |
|       Crystal springsnail       |      Pyrgulopsis crystalis      |                12M                |   R8   |      5      |     18     |
|      San Xavier talussnail      |        Sonorella eremita        |                12M                |   R2   |      4      |     18     |
|   speckled burrowing crayfish   |     Fallicambarus danielae      |                12M                |   R4   |      3      |     24     |
|       spinytail crayfish        |    Procambarus fitzpatricki     |                12M                |   R4   |      3      |     26     |
|          Obey crayfish          |       Cambarus obeyensis        |                12M                |   R4   |      2      |     30     |
|   Rocky Mountain monkeyflower   |       Mimulus gemmiparus        |                12M                |   R6   |      4      |     39     |
|    distal-gland springsnail     |        Pyrgulopsis nanus        |                12M                |   R8   |      5      |     51     |
|     Ash Meadows pebblesnail     |     Pyrgulopsis erythropoma     |                12M                |   R8   |      5      |     55     |
|           relict dace           |       Relictus solitarius       |                12M                |   R8   |      3      |     67     |
|         southern elktoe         |     Alasmidonta triangulata     |                12M                |   R4   |      2      |     75     |
|        Savannah lilliput        |        Toxolasma pullus         |                12M                |   R4   |      2      |     84     |
|         longnose darter         |         Percina nasuta          |                12M                |   R4   |      2      |    147     |
|       Kentucky creekshell       |        Villosa ortmanni         |                12M                |   R4   |      3      |    149     |
|        Amargosa tryonia         |        Tryonia variegata        |                12M                |   R8   |      5      |    220     |
|        western fanshell         |        Cyprogenia aberti        |                12M                |   R3   |      3      |    298     |
|       frecklebelly madtom       |         Noturus munitus         |                12M                |   R4   |     N/A     |    359     |
|          green floater          |      Lasmigona subviridis       |                12M                |   R5   |      3      |    467     |
|        Tennessee pigtoe         |      Pleuronaia barnesiana      |                12M                |   R4   |      2      |    474     |
|          Pecos pupfish          |      Cyprinodon pecosensis      |                12M                |   R2   |      3      |    518     |
|    Cumberland moccasinshell     |      Medionidus conradicus      |                12M                |   R4   |      2      |    533     |
|       Tennessee clubshell       |       Pleurobema oviforme       |                12M                |   R4   |      2      |    613     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2021


```r
fy21 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|          Common Name           |       ScientificName       | ActionType | LeadRO | PriorityBin | bisontotal |
|:------------------------------:|:--------------------------:|:----------:|:------:|:-----------:|:----------:|
|          Landyes pyrg          |    Pyrgulopsis landyei     |    12M     |   R8   |      5      |     0      |
| neritiform Steptoe Ranch pyrg  |   Pyrgulopsis neritella    |    12M     |   R8   |      5      |     0      |
| sub-globose Steptoe Ranch pyrg |   Pyrgulopsis orbiculata   |    12M     |   R8   |      5      |     0      |
|    flat-topped Steptoe pyrg    |   Pyrgulopsis planulata    |    12M     |   R8   |      5      |     0      |
|     northern Steptoe pyrg      |    Pyrgulopsis serrata     |    12M     |   R8   |      5      |     0      |
|     southern Steptoe pyrg      |    Pyrgulopsis sulcata     |    12M     |   R8   |      5      |     0      |
|        mimic cavesnail         |   Phreatodrobia imitata    |    12M     |   R2   |      3      |     2      |
|  Dry Fork Valley cave beetle   | Pseudanophthalmus montanus |    12M     |   R5   |      2      |     4      |
|       widemouth blindcat       |      Satan eurystomus      |    12M     |   R2   |      3      |     19     |
|  Ouachita burrowing crayfish   |    Fallicambarus harpi     |    12M     |   R4   |      3      |     22     |
|      Miami cave crayfish       |    Procambarus milleri     |    12M     |   R4   |      2      |     27     |
|       toothless blindcat       |  Trogloglanis pattersoni   |    12M     |   R2   |      3      |     32     |
|      Coosawattae crayfish      |    Cambarus coosawattae    |    12M     |   R4   |      3      |     48     |
|       northern cavefish        |     Amblyopsis spelaea     |    12M     |   R4   |      3      |    292     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2022


```r
fy22 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|      Common Name       |      ScientificName      | ActionType | LeadRO | PriorityBin | bisontotal |
|:----------------------:|:------------------------:|:----------:|:------:|:-----------:|:----------:|
|      big red sage      |  Salvia penstemonoides   |    12M     |   R2   |      4      |     11     |
|     Salina mucket      |  Potamilus metnecktayi   |    12M     |   R2   |      3      |     16     |
|   Mexican fawnsfoot    |    Truncilla cognata     |    12M     |   R2   |      3      |     25     |
|      Caddo madtom      |     Noturus taylori      |    12M     |   R4   |      3      |     43     |
|   evening fieldslug    |   Deroceras hesperium    |    12M     |   R1   |      5      |     47     |
|    paleback darter     | Etheostoma pallididorsum |    12M     |   R4   |      3      |     76     |
|    orangefin madtom    |     Noturus gilberti     |    12M     |   R5   |      3      |    100     |
|   Waccamaw fatmucket   |   Lampsilis fullerkati   |    12M     |   R4   |      3      |    113     |
|  saltmarsh topminnow   |    Fundulus jenkinsi     |    12M     |   R4   |      3      |    117     |
|     popeye shiner      |    Notropis ariommus     |    12M     |   R4   |      3      |    322     |
| Tennessee heelsplitter |   Lasmigona holstonia    |    12M     |   R4   |      3      |    381     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2023


```r
fy23 %>%
  kable(align = 'c', row.names = F) %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|           Common Name           |      ScientificName      |           ActionType           | LeadRO | PriorityBin | bisontotal |
|:-------------------------------:|:------------------------:|:------------------------------:|:------:|:-----------:|:----------:|
|    Sangre de Cristo peaclam     | Pisidium sanguinichristi |              12M               |   R2   |      3      |     1      |
| Northern Virginia well amphipod |  Stygobromus phreaticus  | Discretionary Status or Review |   R5   |      4      |     8      |
|  Delaware County cave crayfish  |  Cambarus subterraneus   |              12M               |   R2   |      2      |     9      |
|        Shasta chaparral         |    Trilobopsis roperi    |              12M               |   R8   |      5      |     9      |
|     Oklahoma cave crayfish      |    Cambarus tartarus     |              12M               |   R2   |      2      |     12     |
|        Big Bar hesperian        |  Vespericola pressleyi   |              12M               |   R8   |      5      |     12     |
|       White Sands pupfish       |   Cyprinodon tularosa    |              12M               |   R2   |      4      |     15     |
|    Little Tennessee crayfish    |    Cambarus georgiae     |              12M               |   R4   |      3      |     52     |
|    Apalachicola wild indigo     |    Baptisia megacarpa    |              12M               |   R4   |      3      |     68     |
|         Chauga crayfish         |   Cambarus chaugaensis   |              12M               |   R4   |      3      |     74     |
|        smallscale darter        | Etheostoma microlepidum  |              12M               |   R4   |      3      |     77     |
|        bluestripe darter        |   Percina cymatotaenia   |              12M               |   R3   |      5      |    144     |
|          bog spicebush          |   Lindera subcoriacea    |              12M               |   R4   |      3      |    178     |
|          Ozark shiner           |    Notropis ozarcanus    |              12M               |   R4   |      3      |    220     |
|       Alabama hickorynut        |    Obovaria unicolor     |              12M               |   R4   |      3      |    230     |
|        salamander mussel        |   Simpsonaias ambigua    |              12M               |   R3   |      3      |    349     |
|        little brown bat         |     Myotis lucifugus     | Discretionary Status or Review |   R3   |      4      |    8716    |

