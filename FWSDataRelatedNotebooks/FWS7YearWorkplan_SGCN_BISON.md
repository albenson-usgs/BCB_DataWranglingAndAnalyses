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

## Tables of SGCN + FWS National Working Plan species by proposed time frames
SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2017


```r
fy17 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |       Common Name       |      ScientificName      | ActionType | LeadRO | PriorityBin | bisontotal |
|:---|:-----------------------:|:------------------------:|:----------:|:------:|:-----------:|:----------:|
|93  |     Blue Point pyrg     | Pyrgulopsis coloradensis |    12M     |   R8   |      5      |     0      |
|95  |  Spring Mountains pyrg  |   Pyrgulopsis deaconi    |    12M     |   R8   |      5      |     0      |
|98  |     Corn Creek pyrg     |    Pyrgulopsis fausta    |    12M     |   R8   |      5      |     0      |
|99  |       Hubbs pyrg        |    Pyrgulopsis hubbsi    |    12M     |   R8   |      5      |     0      |
|102 |    Butterfield pyrg     |     Pyrgulopsis lata     |    12M     |   R8   |      5      |     0      |
|103 |       Hardy pyrg        |   Pyrgulopsis marcida    |    12M     |   R8   |     N/A     |     0      |
|110 | White River Valley pyrg |    Pyrgulopsis sathos    |    12M     |   R8   |      5      |     0      |
|112 |    Lake Valley pyrg     |   Pyrgulopsis sublata    |    12M     |   R8   |     N/A     |     0      |
|40  |   San Felipe gambusia   |   Gambusia clarkhubbsi   |    12M     |   R2   |      2      |     45     |
|74  |  Panama City crayfish   |  Procambarus econfinae   |    12M     |   R4   |     N/A     |     50     |
|91  |    Moapa pebblesnail    |  Pyrgulopsis avernalis   |    12M     |   R8   |      5      |     50     |
|104 | Pahranagat pebblesnail  |   Pyrgulopsis merriami   |    12M     |   R8   |      5      |     51     |
|92  |    Moapa Valley pyrg    |  Pyrgulopsis carinifera  |    12M     |   R8   |      5      |     53     |
|124 |     blackfin sucker     |   Thoburnia atripinnis   |    12M     |   R4   |      2      |     89     |
|31  |      candy darter       |    Etheostoma osburni    |    12M     |   R5   |     N/A     |    108     |
|131 |     grated tryonia      |    Tryonia clathrata     |    12M     |   R8   |      5      |    115     |
|53  |     Carolina madtom     |     Noturus furiosus     |    12M     |   R4   |      1      |    128     |
|38  |     Atlantic pigtoe     |     Fusconaia masoni     |    12M     |   R4   |     N/A     |    259     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2018


```r
fy18 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |     Common Name      |     ScientificName     |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:---|:--------------------:|:----------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|8   |   Texas pimpleback   |   Amphinaias petrina   | Proposed Listing or Determination |   R2   |    LPN 2    |     0      |
|136 | Apalachicola floater |   Utterbackia heardi   |                12M                |   R4   |      4      |     0      |
|108 |      Ozark pyrg      | Pyrgulopsis ozarkensis |                12M                |   R4   |      1      |     6      |
|16  |  Elk River crayfish  |   Cambarus elkensis    |                12M                |   R5   |      2      |     13     |
|39  |     false spike      |  Fusconaia mitchelli   |                12M                |   R2   |      1      |     13     |
|46  |   Arkansas mudalia   |  Leptoxis arkansensis  |                12M                |   R4   |      1      |     14     |
|42  |   Texas fatmucket    |  Lampsilis bracteata   | Proposed Listing or Determination |   R2   |    LPN 2    |     71     |
|130 |   Texas fawnsfoot    |   Truncilla macrodon   | Proposed Listing or Determination |   R2   |    LPN 2    |     99     |
|4   |    seaside alder     |     Alnus maritima     |                12M                |   R5   |      2      |    115     |
|33  |  Tippecanoe darter   | Etheostoma tippecanoe  |                12M                |   R5   |      2      |    156     |
|29  |     ashy darter      |  Etheostoma cinereum   |                12M                |   R4   |     N/A     |    330     |
|3   |    brook floater     |  Alasmidonta varicosa  |                12M                |   R5   |      2      |    630     |
|57  |   round hickorynut   |  Obovaria subrotunda   |                12M                |   R4   |      2      |    908     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2019


```r
fy19 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |        Common Name         |       ScientificName        |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:---|:--------------------------:|:---------------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|9   |      Arapahoe snowfly      | Arsapnia [=Capnia] arapahoe | Proposed Listing or Determination |   R6   |    LPN 5    |     0      |
|21  |      Arapahoe snowfly      |       Capnia arapahoe       | Proposed Listing or Determination |   R6   |    LPN 5    |     3      |
|63  |     bushy whitlow-wort     |     Paronychia congesta     |                12M                |   R2   |      3      |     4      |
|1   |  Navasota false foxglove   |    Agalinis navasotensis    |                12M                |   R2   |      3      |     8      |
|41  |   Berry Cave Salamander    |  Gyrinophilus gulolineatus  | Proposed Listing or Determination |   R4   |    LPN 8    |     11     |
|10  |     prostrate milkweed     |     Asclepias prostrata     |                12M                |   R2   |      3      |     12     |
|77  |    bearded red crayfish    |      Procambarus pogum      |                12M                |   R4   |      2      |     16     |
|37  |      triangle pigtoe       |    Fusconaia lananensis     |                12M                |   R2   |      3      |     28     |
|118 |     Ocmulgee skullcap      |    Scutellaria ocmulgee     |                12M                |   R4   |      2      |     29     |
|13  |      Texas screwstem       |       Bartonia texana       |                12M                |   R2   |      3      |     35     |
|62  |     Chowanoke crayfish     |   Orconectes virginiensis   |                12M                |   R5   |      3      |     38     |
|59  |     coldwater crayfish     |    Orconectes eupunctus     |                12M                |   R3   |      3      |     53     |
|72  |     Texas heelsplitter     |   Potamilus amphichaenus    |                12M                |   R2   |      3      |     69     |
|61  | St. Francis River crayfish |    Orconectes quadruncus    |  Discretionary Status or Review   |   R3   |      3      |     73     |
|27  |   Carolina pygmy sunfish   |      Elassoma boehlkei      |                12M                |   R4   |      2      |     81     |
|70  |      Louisiana pigtoe      |    Pleurobema riddellii     |                12M                |   R2   |      3      |     81     |
|60  |     Big Creek crayfish     |     Orconectes peruncus     |  Discretionary Status or Review   |   R3   |      3      |    105     |
|11  |  purpledisk honeycombhead  |    Balduina atropurpurea    |                12M                |   R4   |      2      |    140     |
|28  |         Ozark chub         |      Erimystax harryi       |                12M                |   R4   |      2      |    193     |
|117 |       Hall's bulrush       |    Schoenoplectus hallii    |                12M                |   R3   |      3      |    261     |
|65  |      longhead darter       |    Percina macrocephala     |                12M                |   R5   |     N/A     |    376     |
|125 |      purple lilliput       |      Toxolasma lividum      |                12M                |   R4   |      2      |    656     |
|25  |     monarch butterfly      |      Danaus plexippus       |                12M                |   R3   |     N/A     |   52404    |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2020


```r
fy20 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |           Common Name           |         ScientificName          |            ActionType             | LeadRO | PriorityBin | bisontotal |
|:---|:-------------------------------:|:-------------------------------:|:---------------------------------:|:------:|:-----------:|:----------:|
|6   |           golden orb            |        Amphinaias aurea         | Proposed Listing or Determination |   R2   |    LPN 8    |     0      |
|7   |        smooth pimpleback        |     Amphinaias houstonensis     | Proposed Listing or Determination |   R2   |    LPN 8    |     0      |
|78  |       Avernus cave beetle       |    Pseudanophthalmus avernus    |                12M                |   R5   |      3      |     1      |
|81  |     Hubricht's cave beetle      |   Pseudanophthalmus hubrichti   |                12M                |   R5   |      3      |     1      |
|83  |      thin-neck cave beetle      |  Pseudanophthalmus parvicollis  |  Discretionary Status or Review   |   R5   |      3      |     1      |
|86  |     overlooked cave beetle      | Pseudanophthalmus praetermissus |                12M                |   R5   |      3      |     1      |
|88  |       silken cave beetle        |    Pseudanophthalmus sericus    |                12M                |   R5   |      3      |     1      |
|90  |    Maiden Spring cave beetle    |  Pseudanophthalmus virginicus   |                12M                |   R5   |      3      |     1      |
|84  |   Natural Bridge cave beetle    |    Pseudanophthalmus pontis     |                12M                |   R5   |      3      |     2      |
|87  |     Saint Paul cave beetle      |  Pseudanophthalmus sanctipauli  |                12M                |   R5   |      3      |     2      |
|89  |       Thomas' cave beetle       |    Pseudanophthalmus thomasi    |                12M                |   R5   |      3      |     2      |
|80  |      Hubbard's cave beetle      |   Pseudanophthalmus hubbardi    |                12M                |   R5   |      3      |     4      |
|79  |     Holsinger's cave beetle     |  Pseudanophthalmus holsingeri   |                12M                |   R5   |      3      |     5      |
|85  | South Branch Valley cave beetle |   Pseudanophthalmus potomacus   |                12M                |   R5   |      3      |     6      |
|122 |      Wet Canyon talussnail      |     Sonorella macrophallus      |                12M                |   R2   |      4      |     8      |
|100 |   elongate-gland springsnail    |       Pyrgulopsis isolata       |                12M                |   R8   |      5      |     12     |
|97  |      Fairbanks springsnail      |   Pyrgulopsis fairbanksensis    |                12M                |   R8   |      5      |     13     |
|121 |       Pinaleno talussnail       |      Sonorella grahamensis      |                12M                |   R2   |      4      |     13     |
|134 |      Quitobaquito tryonia       |      Tryonia quitobaquitae      |                12M                |   R2   |      4      |     16     |
|132 |     Point of Rocks tryonia      |          Tryonia elata          |                12M                |   R8   |      5      |     17     |
|133 |         minute tryonia          |         Tryonia ericae          |                12M                |   R8   |      5      |     17     |
|94  |       Crystal springsnail       |      Pyrgulopsis crystalis      |                12M                |   R8   |      5      |     18     |
|120 |      San Xavier talussnail      |        Sonorella eremita        |                12M                |   R2   |      4      |     18     |
|34  |   speckled burrowing crayfish   |     Fallicambarus danielae      |                12M                |   R4   |      3      |     24     |
|75  |       spinytail crayfish        |    Procambarus fitzpatricki     |                12M                |   R4   |      3      |     26     |
|18  |          Obey crayfish          |       Cambarus obeyensis        |                12M                |   R4   |      2      |     30     |
|49  |   Rocky Mountain monkeyflower   |       Mimulus gemmiparus        |                12M                |   R6   |      4      |     39     |
|105 |    distal-gland springsnail     |        Pyrgulopsis nanus        |                12M                |   R8   |      5      |     51     |
|96  |     Ash Meadows pebblesnail     |     Pyrgulopsis erythropoma     |                12M                |   R8   |      5      |     55     |
|114 |           relict dace           |       Relictus solitarius       |                12M                |   R8   |      3      |     67     |
|2   |         southern elktoe         |     Alasmidonta triangulata     |                12M                |   R4   |      2      |     75     |
|126 |        Savannah lilliput        |        Toxolasma pullus         |                12M                |   R4   |      2      |     84     |
|66  |         longnose darter         |         Percina nasuta          |                12M                |   R4   |      2      |    147     |
|138 |       Kentucky creekshell       |        Villosa ortmanni         |                12M                |   R4   |      3      |    149     |
|135 |        Amargosa tryonia         |        Tryonia variegata        |                12M                |   R8   |      5      |    220     |
|24  |        western fanshell         |        Cyprogenia aberti        |                12M                |   R3   |      3      |    298     |
|55  |       frecklebelly madtom       |         Noturus munitus         |                12M                |   R4   |     N/A     |    359     |
|45  |          green floater          |      Lasmigona subviridis       |                12M                |   R5   |      3      |    467     |
|71  |        Tennessee pigtoe         |      Pleuronaia barnesiana      |                12M                |   R4   |      2      |    474     |
|22  |          Pecos pupfish          |      Cyprinodon pecosensis      |                12M                |   R2   |      3      |    518     |
|48  |    Cumberland moccasinshell     |      Medionidus conradicus      |                12M                |   R4   |      2      |    533     |
|69  |       Tennessee clubshell       |       Pleurobema oviforme       |                12M                |   R4   |      2      |    613     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2021


```r
fy21 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |          Common Name           |       ScientificName       | ActionType | LeadRO | PriorityBin | bisontotal |
|:---|:------------------------------:|:--------------------------:|:----------:|:------:|:-----------:|:----------:|
|101 |          Landyes pyrg          |    Pyrgulopsis landyei     |    12M     |   R8   |      5      |     0      |
|106 | neritiform Steptoe Ranch pyrg  |   Pyrgulopsis neritella    |    12M     |   R8   |      5      |     0      |
|107 | sub-globose Steptoe Ranch pyrg |   Pyrgulopsis orbiculata   |    12M     |   R8   |      5      |     0      |
|109 |    flat-topped Steptoe pyrg    |   Pyrgulopsis planulata    |    12M     |   R8   |      5      |     0      |
|111 |     northern Steptoe pyrg      |    Pyrgulopsis serrata     |    12M     |   R8   |      5      |     0      |
|113 |     southern Steptoe pyrg      |    Pyrgulopsis sulcata     |    12M     |   R8   |      5      |     0      |
|67  |        mimic cavesnail         |   Phreatodrobia imitata    |    12M     |   R2   |      3      |     2      |
|82  |  Dry Fork Valley cave beetle   | Pseudanophthalmus montanus |    12M     |   R5   |      2      |     4      |
|116 |       widemouth blindcat       |      Satan eurystomus      |    12M     |   R2   |      3      |     19     |
|35  |  Ouachita burrowing crayfish   |    Fallicambarus harpi     |    12M     |   R4   |      3      |     22     |
|76  |      Miami cave crayfish       |    Procambarus milleri     |    12M     |   R4   |      2      |     27     |
|128 |       toothless blindcat       |  Trogloglanis pattersoni   |    12M     |   R2   |      3      |     32     |
|15  |      Coosawattae crayfish      |    Cambarus coosawattae    |    12M     |   R4   |      3      |     48     |
|5   |       northern cavefish        |     Amblyopsis spelaea     |    12M     |   R4   |      3      |    292     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2022


```r
fy22 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |      Common Name       |      ScientificName      | ActionType | LeadRO | PriorityBin | bisontotal |
|:---|:----------------------:|:------------------------:|:----------:|:------:|:-----------:|:----------:|
|115 |      big red sage      |  Salvia penstemonoides   |    12M     |   R2   |      4      |     11     |
|73  |     Salina mucket      |  Potamilus metnecktayi   |    12M     |   R2   |      3      |     16     |
|129 |   Mexican fawnsfoot    |    Truncilla cognata     |    12M     |   R2   |      3      |     25     |
|56  |      Caddo madtom      |     Noturus taylori      |    12M     |   R4   |      3      |     43     |
|26  |   evening fieldslug    |   Deroceras hesperium    |    12M     |   R1   |      5      |     47     |
|32  |    paleback darter     | Etheostoma pallididorsum |    12M     |   R4   |      3      |     76     |
|54  |    orangefin madtom    |     Noturus gilberti     |    12M     |   R5   |      3      |    100     |
|43  |   Waccamaw fatmucket   |   Lampsilis fullerkati   |    12M     |   R4   |      3      |    113     |
|36  |  saltmarsh topminnow   |    Fundulus jenkinsi     |    12M     |   R4   |      3      |    117     |
|51  |     popeye shiner      |    Notropis ariommus     |    12M     |   R4   |      3      |    322     |
|44  | Tennessee heelsplitter |   Lasmigona holstonia    |    12M     |   R4   |      3      |    381     |

SGCN also identified by FWS in their 7 Year Workplan for Fiscal Year 2023


```r
fy23 %>%
  kable(align = 'c') %>%
  kable_styling(bootstrap_options = c("striped","hover"))
```

```
## Currently generic markdown table using pandoc is not supported.
```



|    |           Common Name           |      ScientificName      |           ActionType           | LeadRO | PriorityBin | bisontotal |
|:---|:-------------------------------:|:------------------------:|:------------------------------:|:------:|:-----------:|:----------:|
|68  |    Sangre de Cristo peaclam     | Pisidium sanguinichristi |              12M               |   R2   |      3      |     1      |
|123 | Northern Virginia well amphipod |  Stygobromus phreaticus  | Discretionary Status or Review |   R5   |      4      |     8      |
|19  |  Delaware County cave crayfish  |  Cambarus subterraneus   |              12M               |   R2   |      2      |     9      |
|127 |        Shasta chaparral         |    Trilobopsis roperi    |              12M               |   R8   |      5      |     9      |
|20  |     Oklahoma cave crayfish      |    Cambarus tartarus     |              12M               |   R2   |      2      |     12     |
|137 |        Big Bar hesperian        |  Vespericola pressleyi   |              12M               |   R8   |      5      |     12     |
|23  |       White Sands pupfish       |   Cyprinodon tularosa    |              12M               |   R2   |      4      |     15     |
|17  |    Little Tennessee crayfish    |    Cambarus georgiae     |              12M               |   R4   |      3      |     52     |
|12  |    Apalachicola wild indigo     |    Baptisia megacarpa    |              12M               |   R4   |      3      |     68     |
|14  |         Chauga crayfish         |   Cambarus chaugaensis   |              12M               |   R4   |      3      |     74     |
|30  |        smallscale darter        | Etheostoma microlepidum  |              12M               |   R4   |      3      |     77     |
|64  |        bluestripe darter        |   Percina cymatotaenia   |              12M               |   R3   |      5      |    144     |
|47  |          bog spicebush          |   Lindera subcoriacea    |              12M               |   R4   |      3      |    178     |
|52  |          Ozark shiner           |    Notropis ozarcanus    |              12M               |   R4   |      3      |    220     |
|58  |       Alabama hickorynut        |    Obovaria unicolor     |              12M               |   R4   |      3      |    230     |
|119 |        salamander mussel        |   Simpsonaias ambigua    |              12M               |   R3   |      3      |    349     |
|50  |        little brown bat         |     Myotis lucifugus     | Discretionary Status or Review |   R3   |      4      |    8716    |

```r
#knitr::spin('C:/Users/albenson/Documents/SWAPs/DataAnalysis/FWSNationalListingWorkplan/FWS7YearWorkplan_SGCN_BISON.R')
```

