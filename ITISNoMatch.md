SGCN Names with No ITIS Match
================
Abby Benson
September 19, 2017

Scientific Name Matching Process
================================

For this process, scientific names are collected from the 56 States and U.S. Territories that have been identified as the Species of Greatest Conservation Need (SGCN) for that State or Territory. Once the scientific names are collated they are added to the Taxa Information Registry and the ITIS processor is run. The ITIS processor first looks for an exact match on the scientific name, if one is not found then a fuzzy match is run using a 0.5 level. This process was run on both the 2005 and 2015 SGCN names. All of the data in this notebook comes from the Biogeographic Characterization Branch's data distillery. This analysis is currently missing nine of the 56 SGCN lists.

``` r
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 3.3.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
data <- fromJSON("https://gc2.datadistillery.org/api/v1/sql/bcb?q=SELECT%20*%20FROM%20sgcn.sgcn_statelists%20WHERE%20matchmethod=%27Not%20Matched%27")
noITISmatch <- as_tibble(data$features$properties)
```

2015 SGCN Unmatched Names
-------------------------

``` r
sgcn2015 <- noITISmatch[which(noITISmatch$sgcn2015 == 1),]
sgcn2015grouped = sgcn2015 %>%
  group_by(taxonomicgroup, scientificname, commonname) %>%
  summarize(
    n   = n())
```

### Amphibians

``` r
Amphibians2015 <- sgcn2015grouped[which(sgcn2015grouped == "Amphibians"),] %>%
  arrange(desc(n))
Amphibians2015 %>%
  knitr::kable(col.names = c("Taxonomic Group", "Scientific Name", "Common Name", "N"))
```

| Taxonomic Group | Scientific Name                | Common Name                         |    N|
|:----------------|:-------------------------------|:------------------------------------|----:|
| Amphibians      | Desmognathus organi            | Northern Pygmy salamander           |    3|
| Amphibians      | Lithobates kauffeldi           | Atlantic Coast leopard frog         |    3|
| Amphibians      | Urspelerpes brucei             | Patch-nosed Salamander              |    2|
| Amphibians      | Eurycea spelaea eastern        | Grotto Salamander ''eastern clade'' |    1|
| Amphibians      | Eurycea spelaea northern       | Grotto Salamander 'northern clade'  |    1|
| Amphibians      | Eurycea spelaea western        | Grotto Salamander ''western clade'' |    1|
| Amphibians      | Eurycea subfluvicola           | Ouachita Streambed Salamander       |    1|
| Amphibians      | Pseudacris triseriata maculata | Boreal chorus frog                  |    1|

### Arachnids

| Taxonomic Group | Scientific Name                    | Common Name                  |    N|
|:----------------|:-----------------------------------|:-----------------------------|----:|
| Arachnids       | Aceria chrondriphora               | Ashleaf gall mite            |    1|
| Arachnids       | Aceria fraxiniflora                | Ash flowergall mite          |    1|
| Arachnids       | Aceria fraxinivorus                | Ash key gall mite            |    1|
| Arachnids       | Amblyomma tubercluatum             | Gopher Tortoise Tick         |    1|
| Arachnids       | Amblyomma tuberculatum             | Gopher tortoise tick         |    1|
| Arachnids       | Erebomaster nr. acanthina          | A Cave Spider                |    1|
| Arachnids       | Mundochthonius Caves/Karstrnicolus | Cavernicolous pseudoscorpion |    1|

### Birds

| Taxonomic Group | Scientific Name                 | Common Name                           |    N|
|:----------------|:--------------------------------|:--------------------------------------|----:|
| Birds           | Gallinula galeata               | Common Gallinule                      |   16|
| Birds           | Antrostomus vociferus           | Eastern Whip-poor-will                |   12|
| Birds           | Antrostomus carolinensis        | Chuck-will''s-widow                   |    6|
| Birds           | Setophaga virens waynei         | Wayne''s black-throated green warbler |    4|
| Birds           | Psiloscops flammeolus           | Flammulated Owl                       |    3|
| Birds           | Rallus crepitans                | Clapper Rail                          |    2|
| Birds           | Antigone canadensis tabida      | Greater Sandhill Crane                |    1|
| Birds           | Antrostomus vociferus           | Eastern Whip-poor-will                |    1|
| Birds           | Antrostomus arizonae            | Mexican Whip-poor-will                |    1|
| Birds           | Calidris virgata                | Surfbird                              |    1|
| Birds           | Caprimulgus arizonae            | Mexican Whip-poor-will                |    1|
| Birds           | Dengragapus fuliginosus         | Sooty Grouse                          |    1|
| Birds           | Gallinago gallinagodelicata     | Wilson''s Snipe                       |    1|
| Birds           | Leucosticte arata               | Black Rosy-Finch                      |    1|
| Birds           | Loxia sinesciuris               | Cassia Crossbill                      |    1|
| Birds           | Mississippi unnamed species 1   | Migrant Songbirds                     |    1|
| Birds           | Mississippi unnamed species 2   | Pelagic Birds                         |    1|
| Birds           | Otus kennicotii macfarlanei     | Western screech owl                   |    1|
| Birds           | Setophaga petechial banksi      | Yellow Warbler                        |    1|
| Birds           | Setophaga petechial rubiginosa  | Yellow Warbler                        |    1|
| Birds           | Todiramphus albicilla albicilla | Mariana Kingfisher ssp. albicilla     |    1|
| Birds           | Todiramphus albicilla orii      | Mariana Kingfisher ssp. orii          |    1|
| Birds           | Todiramphus albicilla owstoni   | Mariana Kingfisher ssp. owstoni       |    1|

### Fish

| Taxonomic Group | Scientific Name                         | Common Name                                                                          |    N|
|:----------------|:----------------------------------------|:-------------------------------------------------------------------------------------|----:|
| Fish            | 2                                       | ''Food Fish''                                                                        |    1|
| Fish            | Coregonus bartlettii                    | Siskiwit lake cisco                                                                  |    1|
| Fish            | Coregonus hubbsi                        | Ives lake cisco                                                                      |    1|
| Fish            | Cottus bairdii complex                  | 'Smoky' Sculpin                                                                      |    1|
| Fish            | Cyprinodon variegatus hubbsi            | Lake Eustis Pupfish                                                                  |    1|
| Fish            | Dionda (Notropis) nubilus               | Ozark Minnow                                                                         |    1|
| Fish            | Erimystax insignis eristigma            | Southern Blotched Chub                                                               |    1|
| Fish            | Etheostoma maydeni                      | Redlips Darter                                                                       |    1|
| Fish            | Etheostoma olmstedi maculaticeps        | Southern Tessellated Darter                                                          |    1|
| Fish            | Galeorhinus cuvier                      | Tiger                                                                                |    1|
| Fish            | Lepidomeda aliciae                      | Southern Leatherside Chub                                                            |    1|
| Fish            | Lythroughrus lirus                      | Mountain shiner                                                                      |    1|
| Fish            | Manta alfredi or birostris              | manta ray                                                                            |    1|
| Fish            | Micropterus chattahoochee               | Chattahoochee Bass                                                                   |    1|
| Fish            | Notropis amplamala                      | Longjaw Minnow                                                                       |    1|
| Fish            | Noturus crypticus                       | Chucky Madtom                                                                        |    1|
| Fish            | Oncorhynchus mykiss newberrii 29        | Great Basin Redband Trout - Catlow Valley SMU                                        |    1|
| Fish            | Oncorhynchus mykiss newberrii 30        | Great Basin Redband Trout - Chewaucan SMU                                            |    1|
| Fish            | Oncorhynchus mykiss newberrii 31        | Great Basin Redband Trout - Fort Rock SMU                                            |    1|
| Fish            | Oncorhynchus mykiss newberrii 33        | Great Basin Redband Trout - Malheur Lakes SMU                                        |    1|
| Fish            | Oncorhynchus mykiss newberrii 34        | Great Basin Redband Trout - Upper Klamath Basin SMU (Klamath Mountains Province ESU) |    1|
| Fish            | Oncorhynchus mykiss newberrii/stonei 32 | Great Basin Redband Trout - Goose Lake SMU                                           |    1|
| Fish            | Oncorhynchus mykiss newberrii/stonei 35 | Great Basin Redband Trout - Warner Lakes SMU                                         |    1|
| Fish            | Percina apristis                        | Guadalupe darter                                                                     |    1|
| Fish            | Percina fulvitaenia                     | Ozark Logperch                                                                       |    1|
| Fish            | Pugnose shiner                          | Pugnose shiner                                                                       |    1|
| Fish            | Sebastes diaconus                       | Deacon Rockfish                                                                      |    1|
| Fish            | Syngnathus texanus                      | Texas Pipefish                                                                       |    1|
| Fish            | Tonguetied minnow                       | Tonguetied minnow                                                                    |    1|
| Fish            | Troglichthys rosae                      | Ozark Cavefish                                                                       |    1|

### Insects

| Taxonomic Group | Scientific Name                                   | Common Name                               |    N|
|:----------------|:--------------------------------------------------|:------------------------------------------|----:|
| Insects         | Aphodius aegrotus                                 | A dung beetle                             |    2|
| Insects         | Aphodius baileyi                                  | A dung beetle                             |    2|
| Insects         | Aphodius bakeri                                   | Baker''s Pocket Gopher Aphodius Beetle    |    2|
| Insects         | Aphodius dyspistus                                | Surprising Pocket Gopher Aphodius Beetle  |    2|
| Insects         | Aphodius gambrinus                                | Amber pocket gopher Aphodius beetle       |    2|
| Insects         | Aphodius hubbelli                                 | Hubbell''s Pocket Gopher Aphodius Beetle  |    2|
| Insects         | Aphodius laevigatus                               | Large pocket gopher Aphodius beetle       |    2|
| Insects         | Aphodius platypleurus                             | Broad-sided pocket gopher Aphodius beetle |    2|
| Insects         | Aphodius tanytarsus                               | Long-clawed pocket gopher Aphodius beetle |    2|
| Insects         | Carmenta anthracipennis                           | Blazing Star Clearwing Moth               |    2|
| Insects         | Fixsenia favonius ontario                         | northern hairstreak                       |    2|
| Insects         | Incisalia irus                                    | Frosted Elfin                             |    2|
| Insects         | Lasioglossum arantium                             | A hairy-tongue bee                        |    2|
| Insects         | Palpita magniferalis                              | Splendid Palpita                          |    2|
| Insects         | Philonthus testudo                                | A Rove Beetle                             |    2|
| Insects         | Sweltsa palearata                                 | Shenandoah sallfly                        |    2|
| Insects         | Acrolophus pholeter                               | Gopher Tortoise Acrolophus Moth           |    1|
| Insects         | Acrotrichis lucidula                              | Feather-winged Beetle                     |    1|
| Insects         | Allopentarthrum n. sp. TAC 1                      | A weevil                                  |    1|
| Insects         | Allopentarthrum n. sp. TAC 2                      | A weevil                                  |    1|
| Insects         | Alloperla prognoides                              | A Stonefly                                |    1|
| Insects         | Aloconota insecta                                 | (a rove beetle)                           |    1|
| Insects         | Aloconota laurentiana                             | (a rove beetle)                           |    1|
| Insects         | Anacampsis wikeri                                 | a moth                                    |    1|
| Insects         | Anafroptilum album                                | a mayfly                                  |    1|
| Insects         | Anafroptilum semirufum                            | A Mayfly                                  |    1|
| Insects         | Andranthobius n. sp. TAC 1                        | A weevil                                  |    1|
| Insects         | Antistrophus silphii                              | Silphium Terminal Gall Wasp               |    1|
| Insects         | Aphodius alabama                                  | A dung beetle                             |    1|
| Insects         | Apion porosicolle                                 | A weevil                                  |    1|
| Insects         | Aptenopedes apalachee                             | Apalachee linear-winged grasshopper       |    1|
| Insects         | Argyria critica                                   | Straight-Lined Argyria Moth               |    1|
| Insects         | Arrhopalitesjay                                   | a cave springtail                         |    1|
| Insects         | Arsapnia \[=Capnia\] arapahoe                     | Arapahoe snowfly                          |    1|
| Insects         | Athysanella incongrua                             | a leafhopper                              |    1|
| Insects         | Batrisodes cryptotexanus                          | A cave obligate beetle                    |    1|
| Insects         | Batrisodes dentifrons                             | A cave obligate beetle                    |    1|
| Insects         | Batrisodes fanti                                  | A cave obligate beetle                    |    1|
| Insects         | Batrisodes feminiclypeus                          | A cave obligate beetle                    |    1|
| Insects         | Batrisodes gravesi                                | A cave obligate beetle                    |    1|
| Insects         | Batrisodes incisipes                              | A cave obligate beetle                    |    1|
| Insects         | Batrisodes pekinsi                                | A cave obligate beetle                    |    1|
| Insects         | Batrisodes shadeae                                | A cave obligate beetle                    |    1|
| Insects         | Beckerus barri                                    | A Click Beetle                            |    1|
| Insects         | Branchus floridanus                               | South Florida Beach Darkling Beetle       |    1|
| Insects         | Bryophaenocladius chrissichuckorum                | Midge (Heggie''s Rock)                    |    1|
| Insects         | Callophrys gryneus Columbia Basin segregate       | Juniper Hairstreak                        |    1|
| Insects         | Callophrys polios Puget Trough segregate          | Hoary elfin                               |    1|
| Insects         | Capnia kersti                                     | A Stonefly (no common name)               |    1|
| Insects         | Ceraclea limnetes                                 | Sandhill Lake Caddisfly                   |    1|
| Insects         | Ceratophysella undescribed sp., denticulata group | springtail                                |    1|
| Insects         | Ceuthophilus gracilipes vs. stygius               | cave cricket                              |    1|
| Insects         | Cicindela scabrosa floridana                      | Miami Tiger Beetle                        |    1|
| Insects         | Cicindela togata 'playa'                          | White-cloaked tiger beetle                |    1|
| Insects         | Coelocephalapion decoloratum                      | A Pear-shaped Weevil                      |    1|
| Insects         | Continaria canadensis                             | Ash midrib gall midge                     |    1|
| Insects         | Cordulegaster sarracenia                          | Pitcher Plant Spiketail                   |    1|
| Insects         | Crossidius grahami                                | Ohoopee dunes Crossidius beetle           |    1|
| Insects         | Cuerna alpina                                     | a leafhopper                              |    1|
| Insects         | Dasymutilla archboldi                             | Lake Wales Ridge Velvet Ant               |    1|
| Insects         | Desoria trispinata                                | Three-spined springtail                   |    1|
| Insects         | Desoria truncata                                  | Truncate springtail                       |    1|
| Insects         | Dichopetala seeversi                              | A katydid                                 |    1|
| Insects         | Dorymyrmex bureni                                 | 'A Pyramid Ant'                           |    1|
| Insects         | Dorymyrmex medeis                                 | ''A Pyramid Ant''                         |    1|
| Insects         | Draeculacephala inscripta                         | a leafhopper                              |    1|
| Insects         | Eana georgiella                                   | A tortricid moth                          |    1|
| Insects         | Elleschus n. sp. TAC 1                            | A weevil                                  |    1|
| Insects         | Enodia portlandia floralae                        | Florida Pearly Eye                        |    1|
| Insects         | Epitragosoma arenaria                             | A darkling beetle                         |    1|
| Insects         | Erythroneura carbonata                            | Yellow Loosestrife Leafhopper             |    1|
| Insects         | Eucosma giganteana                                | Giant eucosma moth                        |    1|
| Insects         | Eurystrymon favonius ontario                      | Northern hairstreak                       |    1|
| Insects         | Eutrichapion huron                                | A Straight-snouted Weevil                 |    1|
| Insects         | Fallapion bischoffi                               | A Pear-shaped Weevil                      |    1|
| Insects         | Fallapion impeditum                               | A Pear-shaped Weevil                      |    1|
| Insects         | Fitchiella robertsonii                            | An Issid Planthopper                      |    1|
| Insects         | Flexamia pectinata                                | a leafhopper                              |    1|
| Insects         | Floritettix borealis                              | A grasshopper                             |    1|
| Insects         | Helops cisteloides                                | A tenebrionid beetle                      |    1|
| Insects         | Hesperapis kayella                                | A Miner Bee                               |    1|
| Insects         | Hesperia colorado Salish Sea segregate            | Oregon branded skipper                    |    1|
| Insects         | Hoplitis producta subgracilis                     | A Mason Bee                               |    1|
| Insects         | Hydroptila bribriae                               | Kriebel''s Hydroptila Caddisfly           |    1|
| Insects         | Hydroptila eglinensis                             | Saberlike Hydroptila Caddisfly            |    1|
| Insects         | Hydroptila hamiltoni                              | Hamilton''s Hydroptila Caddisfly          |    1|
| Insects         | Hydroptila okaloosa                               | Rogue Creek Hydroptila Caddisfly          |    1|
| Insects         | Hydroptila sarahae                                | Sarah''s Hydroptila Caddisfly             |    1|
| Insects         | Hydroptila sykorai                                | Sykora''s Hydroptila Caddisfly            |    1|
| Insects         | Hydroscapha redfordi                              | A Skiff Beetle                            |    1|
| Insects         | Hypogastrura undescribed sp.                      | springtails                               |    1|
| Insects         | Icaricia acmon ssp.                               | Straits Acmon blue                        |    1|
| Insects         | Icaricia shasta                                   | Shasta Blue                               |    1|
| Insects         | Incisalia augustinus croesoides                   | Brown Elfin                               |    1|
| Insects         | Incisalia henrici                                 | Henry's Elfin                             |    1|
| Insects         | Incisalia niphon                                  | Eastern Pine Elfin                        |    1|
| Insects         | Ischnura luta                                     | Rota Damselfly                            |    1|
| Insects         | Isocapnia palousa                                 | Palouse Snowfly                           |    1|
| Insects         | Lasioglossum georgeickworti                       | A hairy-tongue bee                        |    1|
| Insects         | Lednia borealis                                   | Northern forestfly                        |    1|
| Insects         | Lesteva pallipes                                  | rove beetle                               |    1|
| Insects         | Litocampa barryi                                  | (a bristletail)                           |    1|
| Insects         | Litocampa cherokeensis                            | (a bristletail)                           |    1|
| Insects         | Litocampa davisi                                  | (a bristletail)                           |    1|
| Insects         | Litocampa inexspectata                            | (a bristletail)                           |    1|
| Insects         | Lomachaeta hicksi                                 | A Velvet Ant                              |    1|
| Insects         | Lycosa ericeticola                                | Rosemary Wolf Spider                      |    1|
| Insects         | Lymantes nadineae                                 | A cave obligate beetle                    |    1|
| Insects         | Macrosteles clavatus                              | Caped Leafhopper                          |    1|
| Insects         | Melanoplus nossi                                  | Noss'' spur-throat grasshopper            |    1|
| Insects         | Meropleon diversicolor sullivani                  | Sullivan''s Meropleon                     |    1|
| Insects         | Mitoura grynea                                    | Olive Hairstreak                          |    1|
| Insects         | Mitoura hesseli                                   | Hessel''s hairstreak                      |    1|
| Insects         | Myndus ovatus                                     | A Planthopper                             |    1|
| Insects         | Neodactria murellus                               | Prairie Sedge Moth                        |    1|
| Insects         | Neotrichia rasmusseni                             | Rasmussen''s Neotrichia Caddisfly         |    1|
| Insects         | Olceclostera angelica                             | The Angel                                 |    1|
| Insects         | Oncopodura fenestra                               | A cave obligate springtail                |    1|
| Insects         | Onychomira floridensis                            | A Comb-clawed Beetle                      |    1|
| Insects         | Oxyethira chrysocara                              | Gold Head Branch Caddisfly                |    1|
| Insects         | Oxyethira pescadori                               | Pescador''s Bottle-cased Caddisfly        |    1|
| Insects         | Papilio bairdii                                   | Western Black Swallowtail                 |    1|
| Insects         | Pendarus magnus                                   | a leafhopper                              |    1|
| Insects         | Perdita fraticincta                               | A mining bee                              |    1|
| Insects         | Perdita salicis euxantha                          | A Miner Bee                               |    1|
| Insects         | Perdita wyomingensis sculleni                     | A Miner Bee                               |    1|
| Insects         | Petrophila daemonalis                             | A snout moth                              |    1|
| Insects         | Philonthus gopheri                                | A Rove Beetle                             |    1|
| Insects         | Photomorphus archboldi                            | Nocturnal Scrub Velvet Ant                |    1|
| Insects         | Photuris brunnipennis floridana                   | Everglades Brownwing Firefly              |    1|
| Insects         | Phyciodes pratensis                               | Field Crescentspot                        |    1|
| Insects         | Pictetiella lechleitneri                          | Talol springfly                           |    1|
| Insects         | Platyomus flexicaulis                             | A weevil                                  |    1|
| Insects         | Plocetes versicolor                               | A weevil                                  |    1|
| Insects         | Podosesia syringae                                | Lilac borer                               |    1|
| Insects         | Polycentropus chenoides                           | A Caddisfly                               |    1|
| Insects         | Polyergus longicornis                             | Long-horned shining amazon ant            |    1|
| Insects         | Polyphylla starkae                                | Auburndale Scrub Scarab Beetle            |    1|
| Insects         | Prostoia ozarkensis                               | Ozark Forestfly                           |    1|
| Insects         | Pseudataenius waltherhorni                        | Pseudataenius Beetle                      |    1|
| Insects         | Pseudosinella cf. christianseni                   | (a cave springtail)                       |    1|
| Insects         | Pseudosinella cf. gisini                          | (a cave springtail)                       |    1|
| Insects         | Pseudosinella cf. nata                            | (a cave springtail)                       |    1|
| Insects         | Pseudosinella undescribed sp. near espanita       | cave springtail                           |    1|
| Insects         | Psorthaspis mariae                                | A Spider Wasp                             |    1|
| Insects         | Psorthaspis sanguinea                             | A Spider Wasp                             |    1|
| Insects         | Pyrgus (centaureae) wyandot                       | Grizzled Skipper                          |    1|
| Insects         | Rhynchomitra microrhina                           | A Planthopper                             |    1|
| Insects         | Rhypasma n. sp. EGR 1                             | A darkling beetle                         |    1|
| Insects         | Sayapion segnipes                                 | A Pear-shaped Weevil                      |    1|
| Insects         | Schoenicus puberulus                              | A tenebrionid beetle                      |    1|
| Insects         | Sciota dammersi                                   | Leadplant Leafwebber Moth                 |    1|
| Insects         | Selonodon archboldi                               | Archbold Cebrionid Beetle                 |    1|
| Insects         | Selonodon ferrugineus                             | Rusty Cebrionid Beetle                    |    1|
| Insects         | Selonodon floridensis                             | Florida Cebrionid Beetle                  |    1|
| Insects         | Selonodon mandibularis                            | Large-jawed Cebrionid Beetle              |    1|
| Insects         | Selonodon santarosae                              | Santa Rosa Cebrionid Beetle               |    1|
| Insects         | Selonodon similis                                 | Similar Cebrionid Beetle                  |    1|
| Insects         | Selonodon simplex                                 | Simple Cebrionid Beetle                   |    1|
| Insects         | Sensillanura barberi                              | Barber's Springtail                       |    1|
| Insects         | Setodes chipolanus                                | Chipola River Caddisfly                   |    1|
| Insects         | Speleochus steevesi                               | (a cave obligate beetle)                  |    1|
| Insects         | Speleochus undescribed species                    | (a beetle)                                |    1|
| Insects         | Speyeria mormonia kimemela                        | Mormon Fritillary                         |    1|
| Insects         | Stenoponia americana                              | flea                                      |    1|
| Insects         | Sweltsa durfeei                                   | Lolo Sawfly                               |    1|
| Insects         | Talanus mecoselis                                 | A darkling beetle                         |    1|
| Insects         | Telamona archboldi                                | Archbold''s Treehopper                    |    1|
| Insects         | Temnothorax pergandei                             | A temnothorax ant                         |    1|
| Insects         | Temnothorax pilagens                              | A Slave-making Ant                        |    1|
| Insects         | Temnothorax texanus                               | Texas temnothorax ant                     |    1|
| Insects         | Temnothorax\_GA\_01                               | Temnothorax new species                   |    1|
| Insects         | Texaponium triplehorni                            | A darkling beetle                         |    1|
| Insects         | Thessalia fulvia                                  | Fulvia Checkerspot                        |    1|
| Insects         | Thysanocnemis bischoffi                           | Ash seed weevil                           |    1|
| Insects         | Triaenodes bicornis                               | A Caddisfly                               |    1|
| Insects         | Triaenodes dendyi                                 | A Caddisfly                               |    1|
| Insects         | Triaenodes lagarto                                | A Caddisfly                               |    1|
| Insects         | Trichapion perforicolle                           | A Pear-shaped Weevil                      |    1|
| Insects         | Undetermined species                              | sepsid Black scavenger fly                |    1|

### Mammals

| Taxonomic Group | Scientific Name                | Common Name                         |    N|
|:----------------|:-------------------------------|:------------------------------------|----:|
| Mammals         | Pekania pennanti               | Fisher                              |    2|
| Mammals         | Peromyscus maniculatus bairdii | Prairie Deer Mouse                  |    2|
| Mammals         | Balaena acutorostrata          | Minke whale                         |    1|
| Mammals         | Blarina shermani               | Sherman''s Short-tailed Shrew       |    1|
| Mammals         | Dipodomys ordii parvabullatus  | Ord''s kangaroo rat                 |    1|
| Mammals         | Lasiurus borealis borealis     | Red Bat                             |    1|
| Mammals         | Martes caurina pop. 3          | Pacific marten (Coastal population) |    1|
| Mammals         | Myodes gapperi carolinensis    | Carolina Red-backed Vole            |    1|
| Mammals         | Myotis subulatus leibii        | Eastern Small-footed Bat            |    1|
| Mammals         | Neotamias amoenus              | Yellow-pine Chipmunk                |    1|
| Mammals         | Neotamias amoenus celeris      | Humboldt yellow-pine chipmunk       |    1|
| Mammals         | Neotamias dorsalis             | Cliff Chipmunk                      |    1|
| Mammals         | Neotamias palmeri              | Palmer''s chipmunk                  |    1|
| Mammals         | Neotamias senex                | shadow (Allen's) chipmunk           |    1|
| Mammals         | Neotamias umbrinus             | Uinta Chipmunk                      |    1|
| Mammals         | P. polionotus ammobates        | Alabama Beach Mouse                 |    1|
| Mammals         | P. polionotus trissylepsis     | Perdido Key Beach Mouse             |    1|
| Mammals         | Peromyscus leucopus buxtoni    | Buxton Woods White-footed Deermouse |    1|
| Mammals         | Sigmodon arizonae cienegae     | Arizona Cotton Rat                  |    1|
| Mammals         | Urocitellus endemicus          | Southern Idaho Ground Squirrel      |    1|

### Mollusks

| Taxonomic Group | Scientific Name                    | Common Name                   |    N|
|:----------------|:-----------------------------------|:------------------------------|----:|
| Mollusks        | Glyphyalinia solida                | A land snail (no common name) |    2|
| Mollusks        | Pilsbryna vanattai                 | Honey Glyph                   |    2|
| Mollusks        | Vespericola depressa               | Dalles hesperian              |    2|
| Mollusks        | Adonata adionata kennerlyi         | Western Floater Mussel        |    1|
| Mollusks        | Adonata beringiana                 | Yukon Floater Mussel          |    1|
| Mollusks        | Anguispira clarki                  | no common name                |    1|
| Mollusks        | Combined account 2                 | Aquatic Snails                |    1|
| Mollusks        | Combined account 3                 | Land Snails                   |    1|
| Mollusks        | Combined account 4                 | Oreohelix Mountain Snails     |    1|
| Mollusks        | Combined account 5                 | Pill Clams                    |    1|
| Mollusks        | Combined account 6                 | Stagnicola Pondsnails         |    1|
| Mollusks        | Cryptomastix mullani hemphilli     | no common name                |    1|
| Mollusks        | Drymaeus multilineatus latizonatus | Wide-banded Forest Snail      |    1|
| Mollusks        | Elimia darwini                     | Pup Elimia                    |    1|
| Mollusks        | Elimia induta                      | Gem Elimia                    |    1|
| Mollusks        | Elimia timida                      | Timid Elimia                  |    1|
| Mollusks        | Floridobia petrifons               | Rock Springs Siltsnail        |    1|
| Mollusks        | Floridobia porterae                | Green Cove Spring Siltsnail   |    1|
| Mollusks        | Fumonelix cherohalaensis           | Roan Covert                   |    1|
| Mollusks        | Fumonelix langdoni                 | Talus Covert                  |    1|
| Mollusks        | Fumonelix roanensis                | Rock-loving Covert            |    1|
| Mollusks        | Juga hemphilli dallesensis         | Dalles juga                   |    1|
| Mollusks        | Juga hemphilli maupinensis         | Purple-lipped Juga            |    1|
| Mollusks        | Marstonia gaddisorum               | Emily's Marstonia             |    1|
| Mollusks        | Mesodon aff. Andrewsae             | Balsam Globe                  |    1|
| Mollusks        | Physa carolinae                    | Carolina Physa                |    1|
| Mollusks        | Physa gyrina utahensis             | Utah Physa                    |    1|
| Mollusks        | Pilsbryna nodopalma                | Oar Tooth Bud                 |    1|
| Mollusks        | Pleurocera proxima                 | Sprite Elimia                 |    1|
| Mollusks        | Pristiloma crateris                | Crater Lake Tightcoil         |    1|
| Mollusks        | Pyrgulopsis coloradensis           | Blue Point pyrg               |    1|
| Mollusks        | Pyrgulopsis landyei                | Landyes pyrg                  |    1|
| Mollusks        | Sonorella huecoensis               | Hueco Mountains talussnail    |    1|
| Mollusks        | Stenotrema macgregori              | Fraudulent Slitmouth          |    1|
| Mollusks        | Triodopsis juxtidens robinae       | no common name                |    1|
| Mollusks        | Vertigo malleata                   | Malleated Vertigo             |    1|
| Mollusks        | Vertigo modesta modesta            | A land snail (no common name) |    1|
| Mollusks        | Vorticifex effusa dalli            | Dall's Ramshorn               |    1|
| Mollusks        | Vorticifex effusa diagonalis       | Lined Ramshorn                |    1|

### Other

| Taxonomic Group | Scientific Name                          | Common Name                     |    N|
|:----------------|:-----------------------------------------|:--------------------------------|----:|
| Other           |                                          | Atlantic Brant                  |    1|
| Other           | Achaearanea porteri                      | Porter’s cave spider            |    1|
| Other           | Achatinella apexfulva                    | O‘ahu Tree Snail                |    1|
| Other           | Achatinella bulimoides                   | O‘ahu Tree Snail                |    1|
| Other           | Achatinella byronii/ decepiens           | O‘ahu Tree Snail                |    1|
| Other           | Achatinella concavospira                 | O‘ahu Tree Snail                |    1|
| Other           | Achatinella fulgens                      | O‘ahu Tree Snail                |    1|
| Other           | Achatinella fuscobasis                   | O‘ahu Tree Snail                |    1|
| Other           | Achatinella lila                         | O‘ahu Tree Snail                |    1|
| Other           | Achatinella livida                       | O‘ahu Tree Snail                |    1|
| Other           | Achatinella mustelina                    | O‘ahu Tree Snail                |    1|
| Other           | Achatinella pupukanioe                   | O‘ahu Tree Snail                |    1|
| Other           | Achatinella sowerbyana                   | O‘ahu Tree Snail                |    1|
| Other           | Amastra cylindrica                       | O‘ahu Terrestrial Snail         |    1|
| Other           | Amastra spirizona                        | O‘ahu Terrestrial Snail         |    1|
| Other           | Andrena screpteropsis                    | no common name                  |    1|
| Other           | Ashmunella amblya cornudasensis          | Woodlandsnail                   |    1|
| Other           | Asphenium dielerectum form erecta        | No Common Name                  |    1|
| Other           | Auriculella pulchra                      | O‘ahu Tree Snail                |    1|
| Other           | Bactrurus undescribed sp.                | groundwater amphipod            |    1|
| Other           | Branchinecta mediospinosa                | Kansas Fairy Shrimp             |    1|
| Other           | Branchinecta raptor                      | Raptor Fairy Shrimp             |    1|
| Other           | Caecidotea mausi                         | Maus'' cave isopod              |    1|
| Other           | Camberus callainus                       | Big Sandy Crayfish              |    1|
| Other           | Ceratina zadontomerus                    | no common name                  |    1|
| Other           | Combined account 1                       | Shrimp (Fairy and Tadpole)      |    1|
| Other           | Cookeconcha hystricella                  | O‘ahu Tree Snail                |    1|
| Other           | Dendrocephalus acacioidea                | Acacia fairy shrimp             |    1|
| Other           | Drosophila differens                     | Picture-wing Fly                |    1|
| Other           | Drosophila digressa                      | Hawaiian Picture-wing Fly       |    1|
| Other           | Drosophila hemipeza                      | Picture-wing Fly                |    1|
| Other           | Drosophila heteroneura                   | Picture-wing Fly                |    1|
| Other           | Drosophila montgomeryi                   | Picture-wing Fly                |    1|
| Other           | Drosophila neoclavisetae                 | Picture-wing Fly                |    1|
| Other           | Drosophila obatai                        | Picture-wing Fly                |    1|
| Other           | Drosophila ochrobasis                    | Picture-wing Fly                |    1|
| Other           | Drosophila substenoptera                 | Picture-wing Fly                |    1|
| Other           | Dubautia imbricata var. imbricata        | Na<U+02BB>ena<U+02BB>e          |    1|
| Other           | Dubius B                                 | Meadow River Burrowing Crayfish |    1|
| Other           | Hesperomannia oahuensis                  | No Common Name                  |    1|
| Other           | Hibiscadelphus stellatus                 | No Common Name                  |    1|
| Other           | Itocyclops undescribed sp.               | cave copepods                   |    1|
| Other           | Kaala subrutila                          | O‘ahu Terrestrial Snail         |    1|
| Other           | Kadua mannii                             | Pilo                            |    1|
| Other           | Kadua schlechtendahliana var. remyi      | Kopa                            |    1|
| Other           | Kadua terminalis                         | Manono                          |    1|
| Other           | Kleptochthonius undescribed sp.          | cave pseudoscorpions            |    1|
| Other           | Laminella aspera                         | Maui Terrestrial Snail          |    1|
| Other           | Laminella sanguinea                      | O‘ahu Terrestrial Snail         |    1|
| Other           | Lasioglossum anomalum                    | no common name                  |    1|
| Other           | Lasioglossum atwoodi                     | no common name                  |    1|
| Other           | Lasioglossum ellisiae                    | no common name                  |    1|
| Other           | Lasioglossum katherineae                 | no common name                  |    1|
| Other           | Lasioglossum planatum                    | no common name                  |    1|
| Other           | Lasioglossum rozeni                      | no common name                  |    1|
| Other           | Lasioglossum smilacinae                  | no common name                  |    1|
| Other           | Lasioglossum taylorae                    | no common name                  |    1|
| Other           | Lipochaeta lobata ssp. leptophylla       | Nehe                            |    1|
| Other           | Macrocheles sp. 1                        | mite                            |    1|
| Other           | Macrocheles sp. 2                        | mite                            |    1|
| Other           | Macrocheles sp. 3                        | mite                            |    1|
| Other           | Melicope cf. knudsenii                   | No Common Name                  |    1|
| Other           | Nemasatosoma sp.                         | harvestmen                      |    1|
| Other           | Newcombia cumingi                        | Newcomb's Tree Snail            |    1|
| Other           | Orconectes diogenes                      | Devil Crayfish                  |    1|
| Other           | Paralictus cephalotes                    | no common name                  |    1|
| Other           | Partulina mighelsiana                    | Molokai Tree Snail              |    1|
| Other           | Partulina proxima                        | Molokai Tree Snail              |    1|
| Other           | Partulina redfieldi                      | Molokai Tree Snail              |    1|
| Other           | Partulina semicarinata                   | Lana‘i tree snail               |    1|
| Other           | Partulina tessellata                     | Molokai Tree Snail              |    1|
| Other           | Partulina variabilis                     | Lana‘i tree snail               |    1|
| Other           | Phallocryptus sublettei                  | Salt playa fairy shrimp         |    1|
| Other           | Polyscias lydgatei                       | No Common Name                  |    1|
| Other           | Pritchardia bakeri                       | No Common Name                  |    1|
| Other           | Pritchardia gordonii                     | No Common Name                  |    1|
| Other           | Pseudoanthidium nanum                    | no common name                  |    1|
| Other           | Pseudotremia undescribed sp. 28          | Hoffman’s cave milliped         |    1|
| Other           | Pseudotremia undescribed sp. 29          | Shear’s cave milliped           |    1|
| Other           | Rana kauffeldi                           | Atlantic Coast Leopard Frog     |    1|
| Other           | Rhagidia species s. latu                 | (a mite)                        |    1|
| Other           | Scoterpes blountensis                    | (a cave obligate millipede)     |    1|
| Other           | Scoterpes hesperus                       | (a cave obligate millipede)     |    1|
| Other           | Scoterpes jackdanieli                    | (a cave obligate millipede)     |    1|
| Other           | Scoterpes musicarustica                  | (a cave obligate millipede)     |    1|
| Other           | Scoterpes tricorner                      | (a cave obligate millipede)     |    1|
| Other           | Scoterpes undescribed sp.                | (a cave milliped)               |    1|
| Other           | Sonorella painteri                       | Lang Canyon Talussnail          |    1|
| Other           | Stauroneus maunakeaensis                 | No Common Name                  |    1|
| Other           | Stenogyne angustifolia ssp. augustifolia | No Common Name                  |    1|
| Other           | Streptocephalus henridumontis            | Dumont''s Fairy Shrimp          |    1|
| Other           | Streptocephalus thomasbowmani            | Bowman''s Fairy Shrimp          |    1|
| Other           | Trigenotylablacki                        | a cave obligate millipede       |    1|
| Other           | Trigenotylavaga                          | a cave obligate millipede       |    1|
| Other           | Venustaconcha sima                       | (a mussel)                      |    1|
| Other           | Viola kauaensis var. hosakae             | No Common Name                  |    1|

### Other Invertebrates

| Taxonomic Group     | Scientific Name                 | Common Name                       |    N|
|:--------------------|:--------------------------------|:----------------------------------|----:|
| Other Invertebrates | Oreohelix yavapai cummingsi     | Cummings Mountainsnail            |    2|
| Other Invertebrates | 1                               | All Staghorn Corals               |    1|
| Other Invertebrates | Cicurina elliotti               | A cave obligate spider            |    1|
| Other Invertebrates | Daedalochila peregrina          | White Liptooth                    |    1|
| Other Invertebrates | Diplocentrus diablo             | A scorpion                        |    1|
| Other Invertebrates | Elimia broccata                 | Brooch Elimia                     |    1|
| Other Invertebrates | Elimia exusta                   | Fire Elimia                       |    1|
| Other Invertebrates | Elimia mihalcikae               | Latticed Elimia                   |    1|
| Other Invertebrates | Elimia teretria                 | Auger Elimia                      |    1|
| Other Invertebrates | Gammarus minus tenuipes         | An Amphipod                       |    1|
| Other Invertebrates | Holothuria rowei                | A Sea Cucumber                    |    1|
| Other Invertebrates | Marstonia angulobasis           | Angled Marstonia                  |    1|
| Other Invertebrates | Marstonia hershleri             | Coosa Pyrg                        |    1|
| Other Invertebrates | Oreohelix strigosa meridionalis | Rocky Mountainsnail               |    1|
| Other Invertebrates | Palaemonias poulsoin            | Alabama Cave Shrimp               |    1|
| Other Invertebrates | Paruroctonus williamsi          | A scorpion                        |    1|
| Other Invertebrates | Pericelis hymanae Poulter       | Hyman''s flatworm                 |    1|
| Other Invertebrates | Phagocata dissimilis sp. nov.   | A planarian                       |    1|
| Other Invertebrates | Phagocata projecta sp. nov.     | A planarian                       |    1|
| Other Invertebrates | Placobdella biannulata          | Biannulate Leech                  |    1|
| Other Invertebrates | Pleurocera striatum             | Striate Hornsnail                 |    1|
| Other Invertebrates | Pseudosinella gisini gisini     | A Cave Springtail                 |    1|
| Other Invertebrates | Pseudotryonia grahamae          | Salt Spring Hydrobe               |    1|
| Other Invertebrates | Pupoides nitidulus              | no common name                    |    1|
| Other Invertebrates | Pygmarrhopalites clarus         | Springtail                        |    1|
| Other Invertebrates | Pygmarrhopalites madonnensis    | Madonna Cave Springtail           |    1|
| Other Invertebrates | Rhodacmea cahawbensis           | Cahaba Ancylid                    |    1|
| Other Invertebrates | Sonorella coloradoensis         | Grand Canyon Talussnail           |    1|
| Other Invertebrates | Sonorella pedregosensis         | Leslie Canyon Talussnail          |    1|
| Other Invertebrates | Sphalloplana cava sp. nov.      | A planarian                       |    1|
| Other Invertebrates | Tartarocreagris reddelli        | A cave obligate pseudoscorpion    |    1|
| Other Invertebrates | Tayshaneta anopica              | A cave obligate spider            |    1|
| Other Invertebrates | Tayshaneta bullis               | A cave obligate spider            |    1|
| Other Invertebrates | Tayshaneta concinna             | A cave obligate spider            |    1|
| Other Invertebrates | Tayshaneta devia                | A cave obligate spider            |    1|
| Other Invertebrates | Tayshaneta microps              | Government Canyon Bat Cave spider |    1|
| Other Invertebrates | Tayshaneta myopica              | Tooth Cave spider                 |    1|
| Other Invertebrates | Tayshaneta valverde             | A cave obligate spider            |    1|
| Other Invertebrates | Tyrannochthonius muchmoreorum   | A cave obligate pseudoscorpion    |    1|
| Other Invertebrates | Vaejovis chisos                 | A scorpion                        |    1|

### Plants

| Taxonomic Group | Scientific Name                                 | Common Name                            |    N|
|:----------------|:------------------------------------------------|:---------------------------------------|----:|
| Plants          | Cardamine dentata                               | Cuckoo Flower                          |    2|
| Plants          | Crepidomanes intricatum                         | Weft Fern                              |    2|
| Plants          | Huperzia appressa                               | Mountain Fir Clubmoss                  |    2|
| Plants          | Linnaea borealis ssp. americana                 | American Twinflower                    |    2|
| Plants          | Lophocolea appalachiana                         | (a liverwort)                          |    2|
| Plants          | Ageratina aromatica var. aromatica              | Small White Snakeroot                  |    1|
| Plants          | Allium allegheniense                            | Allegheny Onion                        |    1|
| Plants          | Allium oxyphilum                                | Nodding Wild Onion                     |    1|
| Plants          | Alnus maritima ssp. georgiensis                 | Georgia Alder                          |    1|
| Plants          | Amsonia tabernaemontana var. gattingeri         | Limestone Blue Star                    |    1|
| Plants          | Andromeda polifolia var. glaucophylla           | Bog-rosemary                           |    1|
| Plants          | Andropogon glomeratus var. glomeratus           | Bushy Broom-sedge                      |    1|
| Plants          | Anemone quinquefolia var. minima                | Dwarf Anemone                          |    1|
| Plants          | Anticlea glauca                                 | White Camas                            |    1|
| Plants          | Arabis hirsuta ssp. pycnocarpa                  | Hairy Rockcress                        |    1|
| Plants          | Arabis serotina                                 | Shalebarren Rockcress                  |    1|
| Plants          | Arundinaria gigantea ssp. gigantea              | Giant Cane                             |    1|
| Plants          | Asplenium scolopendrium var. americanum         | Hart''s-tongue Fern                    |    1|
| Plants          | Astragalus crassicarpus var. trichocalyx        | Ground-plum                            |    1|
| Plants          | Astragalus distortus var. distortus             | Bent Milkvetch                         |    1|
| Plants          | Astranthium integrifolium ssp. integrifolium    | Daisy                                  |    1|
| Plants          | Athyrium filix-femina ssp. angustum             | Northern Lady Fern                     |    1|
| Plants          | Baptisia australis var. australis               | False Blue Indigo                      |    1|
| Plants          | Baptisia bracteata var. leucophaea              | Cream Wild Indigo                      |    1|
| Plants          | Betula papyrifera var. cordifolia               | Heart-leaved Paper Birch               |    1|
| Plants          | Borodinia missouriensis                         | Green Rock-cress                       |    1|
| Plants          | Botrychium lanceolatum var. angustisegmentum    | Lanceolate Grapefern                   |    1|
| Plants          | Bouteloua curtipendula var. curtipendula        | Sideoats Grama                         |    1|
| Plants          | Brachythecium falcatum                          | Falcate Feather Moss                   |    1|
| Plants          | Bucklandiella microcarpa                        | A Moss                                 |    1|
| Plants          | Bulbostylis ciliatifolia var. coarctata         | Capillary Hairsedge                    |    1|
| Plants          | Calamagrostis porteri ssp. porteri              | Reedgrass                              |    1|
| Plants          | Calamagrostis stricta ssp. stricta var. stricta | Reedgrass                              |    1|
| Plants          | Calliphysalis carpenteri                        | Carpenter’s Ground-Cherry              |    1|
| Plants          | Calochortus ciscoensis                          | Cisco sego lily                        |    1|
| Plants          | Calopogon tuberosus var. tuberosus              | Tuberous Grass-pink                    |    1|
| Plants          | Calycanthus floridus var. glaucus               | Carolina Allspice, Strawberry-shrub    |    1|
| Plants          | Calystegia catesbeiana ssp. sericata            | Silky Bindweed                         |    1|
| Plants          | Calystegia spithamaea ssp. purshiana            | Shale Bindweed                         |    1|
| Plants          | Campylopus flexuosus                            | Campylopus Moss                        |    1|
| Plants          | Cardamine flagellifera var. flagellifera        | Bittercress                            |    1|
| Plants          | Carex atlantica ssp atlantica                   | A sedge                                |    1|
| Plants          | Carex atlantica var. atlantica                  | Eastern Sedge                          |    1|
| Plants          | Carex bromoides ssp bromoides                   | A sedge                                |    1|
| Plants          | Carex bromoides ssp. montana                    | Blue Ridge Brome Sedge                 |    1|
| Plants          | Carex capillaris ssp. capillaris                | Capillary Sedge                        |    1|
| Plants          | Carex echinata ssp. echinata                    | Little Prickly Sedge                   |    1|
| Plants          | Carex echinodes                                 | Urchin Sedge                           |    1|
| Plants          | Carex laxiculmis var. copulata                  | Spreading Sedge                        |    1|
| Plants          | Carex lucorum var. austrolucorum                | Blue Ridge Sedge                       |    1|
| Plants          | Carex oligosperma var. oligosperma              | Fewseed Sedge                          |    1|
| Plants          | Carex oxylepis var. pubescens                   | Hairy Sharp-scaled Sedge               |    1|
| Plants          | Cerastium nutans ssp. nutans                    | Nodding Chickweed                      |    1|
| Plants          | Cerastium velutinum var. velutinum              | Velvety Cerastium                      |    1|
| Plants          | Chamaesyce vermiculata                          | Hairy Spurge                           |    1|
| Plants          | Chenopodium rubrum var. humile                  | Alkali Blite                           |    1|
| Plants          | Chrysopsis gossypina ssp. hyssopifolia          | Cottony Goldenaster                    |    1|
| Plants          | Circaea lutetiana ssp. canadensis               | Canada Enchanter's-nightshade          |    1|
| Plants          | Clematis carrizoanus                            | Carrizo sands leather-flower           |    1|
| Plants          | Clematis occidentalis var. occidentalis         | Purple Virgin's Bower                  |    1|
| Plants          | Codriophorus aduncoides                         | A Moss                                 |    1|
| Plants          | Codriophorus fascicularis                       | A Moss                                 |    1|
| Plants          | Crataegus irrasa var. irrasa                    | Zigzag Hawthorn                        |    1|
| Plants          | Crataegus pringlei                              | Pringle''s Hawthorn                    |    1|
| Plants          | Croton glandulosus var. septentrionalis         | Vente-conmigo                          |    1|
| Plants          | Cuscuta indecora var. neuropetala               | Dodder                                 |    1|
| Plants          | Dalea austrotexana                              | Dune dalea                             |    1|
| Plants          | Dalea compacta var. pubescens                   | Compact Prairie-clover                 |    1|
| Plants          | Dalea villosa var. grisea                       | Silky Prairie-clover                   |    1|
| Plants          | Dalibarda repens                                | Robin-run-away                         |    1|
| Plants          | Descurainia pinnata ssp. brachycarpa            | Tansy Mustard                          |    1|
| Plants          | Diapensia lapponica ssp. lapponica              | Diapensia                              |    1|
| Plants          | Dichanthelium acuminatum ssp. acuminatum        | Hairy Panicgrass                       |    1|
| Plants          | Dichanthelium acuminatum ssp. columbianum       | District Of Columbia Panicgrass        |    1|
| Plants          | Dichanthelium acuminatum ssp. leucothrix        | Roughish Witchgrass                    |    1|
| Plants          | Dichanthelium acuminatum ssp. spretum           | Eaton's Witchgrass                     |    1|
| Plants          | Dichanthelium ensifolium ssp. curtifolium       | Short-leaved Panic Grass               |    1|
| Plants          | Dichanthelium ensifolium ssp. ensifolium        | Small-leaved Panicgrass                |    1|
| Plants          | Dichanthelium ovale ssp. pseudopubescens        | Commons'' Panic-grass                  |    1|
| Plants          | Dichanthelium strigosum var. glabrescens        | Rough-hair Witchgrass                  |    1|
| Plants          | Dichanthelium strigosum var. leucoblepharis     | Rough-hair Witch Grass                 |    1|
| Plants          | Diervilla sessilifolia var. rivularis           | Mountain Bush-honeysuckle              |    1|
| Plants          | Diphasiastrum sabinifolium                      | Ground-fir                             |    1|
| Plants          | Dirca decipiens                                 | Leatherwood                            |    1|
| Plants          | Drepanocladus longifolius                       | A Moss                                 |    1|
| Plants          | Drosera rotundifolia var. rotundifolia          | Roundleaf Sundew                       |    1|
| Plants          | Echinocereus milleri                            | Miller''s hedgehog cactus              |    1|
| Plants          | Echinocereus viridiflorus var. canus            | Graybeard cactus                       |    1|
| Plants          | Elymus trachycaulus ssp. trachycaulus           | Slender Wild Rye                       |    1|
| Plants          | Eriogonum longifolium var. harperi              | Harper''s Umbrella-plant               |    1|
| Plants          | Eriophorum angustifolium var. angustifolium     | Tall Cotton-grass                      |    1|
| Plants          | Eupatorium hyssopifolium var. hyssopifolium     | Hyssopleaf Thoroughwort                |    1|
| Plants          | Eupatorium hyssopifolium var. laciniatum        | Hyssopleaf Thoroughwort                |    1|
| Plants          | Eupatorium maculatum var. maculatum             | Spotted Joe-pyeweed                    |    1|
| Plants          | Eupatorium novae-angliae                        | New England Boneset                    |    1|
| Plants          | Forestiera segregata var. segregata             | Florida Wild Privet                    |    1|
| Plants          | Gaura neomexicana coloradensis                  | Colorado Butterfly Plant               |    1|
| Plants          | Geum fragarioides                               | Barren Strawberry                      |    1|
| Plants          | Glyceria grandis var. grandis                   | American Mannagrass                    |    1|
| Plants          | Grimmia longirostris                            | A Moss                                 |    1|
| Plants          | Grimmia muehlenbeckii                           | A Moss                                 |    1|
| Plants          | Grindelia lanceolata var. lanceolata            | Narrowleaf Gumweed                     |    1|
| Plants          | Gutierrezia elegans                             | Lone Mesa snakeweed                    |    1|
| Plants          | Hedyotis purpurea var. montana                  | Roan Mountain Bluet                    |    1|
| Plants          | Helianthus occidentalis ssp. occidentalis       | Western Sunflower                      |    1|
| Plants          | Hesperaloe funifera subsp. funifera             | Mexican hesperaloe                     |    1|
| Plants          | Heuchera americana var. hispida                 | Rough Alumroot, Rough Heuchera         |    1|
| Plants          | Heuchera longiflora var. aceroides              | Maple-leaf Alumroot                    |    1|
| Plants          | Hexalectris spicata var. spicata                | Spiked Crested Coralroot               |    1|
| Plants          | Hierochloe hirta ssp. arctica                   | Holy Grass, Sweetgrass                 |    1|
| Plants          | Hudsonia tomentosa var. tomentosa               | False Heather, Wooly Hudsonia          |    1|
| Plants          | Hymenoxys pygmea                                | Pygmy prairie dawn                     |    1|
| Plants          | Hypericum erythraeae                            | Georgia St.-John's-Wort                |    1|
| Plants          | Hypericum mutilum var. mutilum                  | Dwarf St. John''s-wort                 |    1|
| Plants          | Hypericum virgatum                              | Sharpleaf St. John''s-wort             |    1|
| Plants          | Hypnum fauriei                                  | A Moss                                 |    1|
| Plants          | Isoetes riparia var. riparia                    | Shore Quillwort                        |    1|
| Plants          | Isoetes valida                                  | True Quillwort                         |    1|
| Plants          | Isoetes viridimontana                           | Green mountain Quillwort               |    1|
| Plants          | Juncus nodosus var. nodosus                     | Knotted Rush                           |    1|
| Plants          | Kalmia angustifolia var. carolina               | Lambkill                               |    1|
| Plants          | Leavenworthia exigua var. lutea                 | Pasture Glade-cress                    |    1|
| Plants          | Lechea pulchella var. pulchella                 | Pinweed                                |    1|
| Plants          | Lejeunea lamacerina ssp. gemminata              | A Liverwort                            |    1|
| Plants          | Lejeunea sharpii                                | Sharp's Lejeunea                       |    1|
| Plants          | Leptodontium viticulosoides var. sulphureum     | Grandfather Mountain Leptodontium      |    1|
| Plants          | Leucothoe recurva                               | Red-twig Doghobble                     |    1|
| Plants          | Liatris turgida                                 | Turgid Gayfeather                      |    1|
| Plants          | Lilium philadelphicum var. philadelphicum       | Wood Lily                              |    1|
| Plants          | Lindernia dubia var. anagallidea                | Yellowseed False Pimpernel             |    1|
| Plants          | Listera cordata var. cordata                    | Heartleaf Twayblade                    |    1|
| Plants          | Lithospermum occidentale                        | Western Marble-Seed                    |    1|
| Plants          | Lupinus perennis ssp. perennis                  | Wild Lupine, Sundial Lupine            |    1|
| Plants          | Luzula acuminata var. carolinae                 | Southern Hairy Woodrush                |    1|
| Plants          | Luzula parviflora ssp. melanocarpa              | Black-fruited Woodrush                 |    1|
| Plants          | Lycopodiella cernua var. cernua                 | Staghorn Clubmoss                      |    1|
| Plants          | Lythrum alatum ssp. alatum                      | Winged-loosestrife                     |    1|
| Plants          | Lythrum alatum var. alatum                      | Winged Loosestrife                     |    1|
| Plants          | Maianthemum racemosum ssp. racemosum            | Solomon's-plume                        |    1|
| Plants          | Melanelia stygia                                | Stygian Black-parmelia                 |    1|
| Plants          | Melothria pendula var. pendula                  | Creeping Cucumber, Guadeloupe Cucumber |    1|
| Plants          | Mentzelia paradoxensis                          | Paradox stickleaf                      |    1|
| Plants          | Mentzelia rhizomata                             | Roan Cliffs blazing star               |    1|
| Plants          | Microbryum davallianum                          | A Moss                                 |    1|
| Plants          | Microlejeunea globosa                           | Cardot''s Lejeunea                     |    1|
| Plants          | Molendoa ogalalensis                            | A Moss                                 |    1|
| Plants          | Monarda fistulosa ssp. brevis                   | Smoke Hole Bergamot                    |    1|
| Plants          | Mortonia sempervirens var. sempervirens         | Smooth mortonia                        |    1|
| Plants          | Muhlenbergia capillaris var. capillaris         | Hair-awn Muhly                         |    1|
| Plants          | Muhlenbergia capillaris var. filipes            | Southern Hairgrass                     |    1|
| Plants          | Nardia scalaris ssp. scalaris                   | A Liverwort                            |    1|
| Plants          | Nemophila sayersensis                           | Sayersville blue eyes                  |    1|
| Plants          | Niphotrichum canescens ssp. canescens           | A Moss                                 |    1|
| Plants          | Oenothera pilosella ssp. pilosella              | Meadow Sundrops                        |    1|
| Plants          | Onosmodium molle ssp. subsetosum                | Smooth False Gromwell                  |    1|
| Plants          | Oreocarya revealii                              | Gypsum Valley cat’s- eye               |    1|
| Plants          | Packera mancosana                               | Mancos shale packera                   |    1|
| Plants          | Packera texensis                                | Llano butterweed                       |    1|
| Plants          | Palafoxia texana var. ambigua                   | Texas Palafoxia                        |    1|
| Plants          | Panicum hallii var. filipes                     | Hall's Panic Grass                     |    1|
| Plants          | Panicum philadelphicum ssp. philadelphicum      | Philadelphia Panic-grass               |    1|
| Plants          | Parathelypteris simulata                        | Massachusetts Fern                     |    1|
| Plants          | Paronychia jamesii var. jamesii                 | James'' Nailwort                       |    1|
| Plants          | Pedicularis canadensis var. canadensis          | Canada Lousewort                       |    1|
| Plants          | Pediomelum hypogaeum var. subulatum             | Awl-shaped Scurfpea                    |    1|
| Plants          | Pellaea glabella ssp. glabella                  | Smooth Cliffbrake                      |    1|
| Plants          | Pellaea glabella var. glabella                  | Smooth Cliff-brake                     |    1|
| Plants          | Phacelia gina-glenneae                          | Troublesome phacelia                   |    1|
| Plants          | Phacelia petiolata                              | Stalk-leaf phacelia                    |    1|
| Plants          | Phaseolus texensis                              | Canyon bean                            |    1|
| Plants          | Phlox pilosa ssp. ozarkana                      | Ozark Downy Phlox                      |    1|
| Plants          | Phragmites australis ssp. americanus            | American Reedgrass                     |    1|
| Plants          | Physaria arenosa var. arenosa                   | Secund Bladder-Pod                     |    1|
| Plants          | Physaria ovalifolia var. ovalifolia             | Round-leaf Bladder-pod                 |    1|
| Plants          | Piptatheropsis pungens                          | Slender Mountain-rice                  |    1|
| Plants          | Plagiochila exigua                              | (a liverwort)                          |    1|
| Plants          | Plagiochila punctata                            | Spotty featherwort                     |    1|
| Plants          | Plagiochila retrorsa                            | (a liverwort)                          |    1|
| Plants          | Platanthera shriveri                            | Shriver's Frilly Orchid                |    1|
| Plants          | Pohlia rabunbaldensis                           | Rabun Bald Feather-Moss                |    1|
| Plants          | Polygala cruciata var. aquilonia                | Cross-leaved Milkwort                  |    1|
| Plants          | Polygonum amphibium                             | Water Knotweed                         |    1|
| Plants          | Populus balsamifera ssp. balsamifera            | Balsam Poplar                          |    1|
| Plants          | Porella gracillima                              | Hot Porella                            |    1|
| Plants          | Porella japonica ssp. appalachiana              | ''A Liverwort''                        |    1|
| Plants          | Potamogeton pusillus var. tenuissimus           | Slender Pondweed                       |    1|
| Plants          | Prunus alleghaniensis var. alleghaniensis       | Allegheny Plum                         |    1|
| Plants          | Prunus angustifolia var. angustifolia           | Chickasaw Plum                         |    1|
| Plants          | Ptilimnium fluviatile                           | Harperella                             |    1|
| Plants          | Quercus sinuata var. sinuata                    | Durand Oak                             |    1|
| Plants          | Ranunculus aquatilis var. diffusus              | White Water-buttercup                  |    1|
| Plants          | Ranunculus flammula var. filiformis             | Greater Creeping Spearwort             |    1|
| Plants          | Ranunculus pusillus var. pusillus               | Low Spearwort                          |    1|
| Plants          | Ranunculus sceleratus var. sceleratus           | Cursed Crowfoot                        |    1|
| Plants          | Ranunculus trichophyllus var. trichophyllus     | Threadleaf Water Crowfoot              |    1|
| Plants          | Rhamnus lanceolata ssp. lanceolata              | Lance-leaved Buckthorn                 |    1|
| Plants          | Rhexia mariana var. mariana                     | Maryland Meadowbeauty                  |    1|
| Plants          | Rosa blanda var. blanda                         | Smooth Rose                            |    1|
| Plants          | Rubus pubescens var. pubescens                  | Dwarf Red Bramble                      |    1|
| Plants          | Rudbeckia fulgida var. fulgida                  | Orange Coneflower                      |    1|
| Plants          | Rudbeckia triloba var. pinnatiloba              | Pinnate-lobe Black-eyed-susan          |    1|
| Plants          | Saccharum brevibarbe var. brevibarbe            | Short-beard Plumegrass                 |    1|
| Plants          | Sagittaria calycina var. calycina               | Long-lobe Arrowhead                    |    1|
| Plants          | Salix humilis var. tristis                      | Dwarf Gray Willow                      |    1|
| Plants          | Salix lucida ssp. lucida                        | Shining Willow                         |    1|
| Plants          | Salvia azurea var. grandiflora                  | Blue Sage                              |    1|
| Plants          | Samolus valerandi ssp. parviflorus              | Seaside Brookweed                      |    1|
| Plants          | Sarracenia purpurea var. montana                | Mountain Purple Pitcherplant           |    1|
| Plants          | Sarracenia purpurea var. venosa                 | Lowland Purple Pitcherplant            |    1|
| Plants          | Sarracenia rubra aff. gulfensis                 | Sweet Pitcherplant                     |    1|
| Plants          | Sarracenia rubra ssp. rubra                     | Sweet Pitcherplant                     |    1|
| Plants          | Saxifraga michauxii                             | Cliff Saxifrage                        |    1|
| Plants          | Scapania irrigua ssp. irrigua                   | A Liverwort                            |    1|
| Plants          | Scapania mucronata ssp. mucronata               | A Liverwort                            |    1|
| Plants          | Schistidium papillosum                          | A Moss                                 |    1|
| Plants          | Schistidium viride                              | A Moss                                 |    1|
| Plants          | Schoenoplectiella smithii var. smithii          | Smith''s Bulrush                       |    1|
| Plants          | Schoenoplectus acutus var. acutus               | Hardstem Bulrush                       |    1|
| Plants          | Scleria pauciflora var. pauciflora              | Nutrush                                |    1|
| Plants          | Scutellaria ovata ssp. ovata                    | Heart-leaved Skullcap                  |    1|
| Plants          | Scutellaria ovata ssp. virginiana               | Heart-leaved Skullcap                  |    1|
| Plants          | Selaginella arenicola ssp. riddellii            | Sand Spikemoss                         |    1|
| Plants          | Sibara virginica                                | Virginia Cress                         |    1|
| Plants          | Sidalcea neomexicana var. neomexicana           | New Mexico Checker-mallow              |    1|
| Plants          | Silene caroliniana ssp. wherryi                 | Sticky Catchfly                        |    1|
| Plants          | Silene virginica var. robusta                   | Fire Pink                              |    1|
| Plants          | Silphium compositum var. reniforme              | Rosinweed                              |    1|
| Plants          | Silphium perfoliatum var. connatum              | Cup-plant                              |    1|
| Plants          | Solidago arguta var. harrisii                   | Shalebarren Goldenrod                  |    1|
| Plants          | Solidago patula var. patula                     | Roundleaf Goldenrod                    |    1|
| Plants          | Solidago simplex ssp. randii                    | Rand's Goldenrod                       |    1|
| Plants          | Spinulum canadense                              | Stiff Clubmoss                         |    1|
| Plants          | Spiranthes lacera var. lacera                   | Northern Slender Ladies' Tresses       |    1|
| Plants          | Spiranthes ovalis var. erostellata              | Oval Ladies' Tresses                   |    1|
| Plants          | Stachys nuttallii                               | Heartleaf Hedge-nettle                 |    1|
| Plants          | Stellaria borealis ssp. borealis                | Northern Stitchwort                    |    1|
| Plants          | Stenanthium gramineum var. gramineum            | Featherbells                           |    1|
| Plants          | Stenanthium gramineum var. micranthum           | Tiny-flower Featherbells               |    1|
| Plants          | Stenanthium gramineum var. robustum             | Stout Featherbells                     |    1|
| Plants          | Streptopus lanceolatus var. lanceolatus         | Rosy Twisted-Stalk                     |    1|
| Plants          | Stuckenia x fennica                             | Hybrid Thread-leaved Pondweed          |    1|
| Plants          | Symphyotrichum ericoides var. ericoides         | White Heath Aster                      |    1|
| Plants          | Symphyotrichum laeve var. concinnum             | Smooth Blue American-aster             |    1|
| Plants          | Symphyotrichum lanceolatum var. interior        | Inland Lance-leaf Aster                |    1|
| Plants          | Timmia megapolitana ssp. megapolitana           | A Moss                                 |    1|
| Plants          | Torreyochloa pallida var. fernaldii             | Mannagrass                             |    1|
| Plants          | Tortella inclinata var. densa                   | A Moss                                 |    1|
| Plants          | Tortella inclinata var. inclinata               | A Moss                                 |    1|
| Plants          | Trillium pusillum var. virginianum              | Dwarf Wakerobin                        |    1|
| Plants          | Tritomaria exsectiformis ssp. exsectiformis     | A Liverwort                            |    1|
| Plants          | Viburnum opulus var. americanum                 | Highbush Cranberry                     |    1|
| Plants          | Viola appalachiensis                            | Appalachian Blue Violet                |    1|
| Plants          | Viola blanda var. palustriformis                | Violet                                 |    1|
| Plants          | Viola lanceolata var. lanceolata                | Bog White Violet                       |    1|
| Plants          | Viola tripartita var. tripartita                | Three-parted Violet                    |    1|
| Plants          | Woodsia appalachiana                            | Allegheny Cliff Fern                   |    1|
| Plants          | Woodsia scopulina ssp. appalachiana             | Alleghany Cliff-fern                   |    1|
| Plants          | Xyris laxifolia var. iridifolia                 | Wide-leaved Yellow-eyed Grass          |    1|
| Plants          | Xyris stricta var. stricta                      | Pineland Yellow-eyed-grass             |    1|

### Reptiles

| Taxonomic Group | Scientific Name                                 | Common Name                   |    N|
|:----------------|:------------------------------------------------|:------------------------------|----:|
| Reptiles        | Pantherophis guttatus                           | Red Cornsnake                 |    4|
| Reptiles        | Plestiodon laticeps                             | Broadhead Skink               |    4|
| Reptiles        | Pantherophis alleghaniensis                     | Eastern Ratsnake              |    3|
| Reptiles        | Plestiodon anthracinus anthracinus              | Northern Coal Skink           |    3|
| Reptiles        | Plestiodon fasciatus                            | Common Five-lined Skink       |    3|
| Reptiles        | Crotalus cerberus                               | Arizona Black Rattlesnake     |    2|
| Reptiles        | Hypsiglena chlorophaea                          | Desert nightsnake             |    2|
| Reptiles        | Lampropeltis californiae                        | California Kingsnake          |    2|
| Reptiles        | Lampropeltis holbrooki                          | Speckled Kingsnake            |    2|
| Reptiles        | Pantherophis ramspotti                          | Western Fox Snake             |    2|
| Reptiles        | Plestiodon anthracinus pluvialis                | Southern Coal Skink           |    2|
| Reptiles        | Plestiodon septentrionalis obtusirostris        | Southern Prairie Skink        |    2|
| Reptiles        | Anolis carolinensis seminolus                   | Southern Green Anole          |    1|
| Reptiles        | Aspidoscelis burti                              | Giant Spotted Whiptail        |    1|
| Reptiles        | Aspidoscelis exsanguis                          | Chihuahuan Spotted Whiptail   |    1|
| Reptiles        | Aspidoscelis flagellicauda                      | Gila Spotted Whiptail         |    1|
| Reptiles        | Aspidoscelis neotesselata                       | Colorado checkered whiptail   |    1|
| Reptiles        | Aspidoscelis sexlineatus viridis                | Prairie Racerunner            |    1|
| Reptiles        | Aspidoscelis stictogramma                       | Giant Spotted Whiptail        |    1|
| Reptiles        | Aspidoscelis xanthonota                         | Red-back Whiptail             |    1|
| Reptiles        | Graptemys sabinensis                            | Sabine Map Turtle             |    1|
| Reptiles        | Heterodon gloydi                                | Dusty hog-nosed snake         |    1|
| Reptiles        | Hypsiglena jani                                 | Chihuahuan Night Snake        |    1|
| Reptiles        | Hypsiglena species novum                        | Hooded Nightsnake             |    1|
| Reptiles        | Lampropeltis elapsoides                         | Scarlet kingsnake             |    1|
| Reptiles        | Lampropeltis extenuata                          | Short-tailed Snake            |    1|
| Reptiles        | Lampropeltis gentilis                           | Western Milksnake             |    1|
| Reptiles        | Lampropeltis nigra                              | Eastern black kingsnake       |    1|
| Reptiles        | Lampropeltis nigra holbrooki                    | Eastern Speckled Kingsnake    |    1|
| Reptiles        | Lampropeltis rhombomaculata                     | Northern Mole Kingsnake       |    1|
| Reptiles        | Lampropeltis triangulum elapsoides X triangulum | Coastal Plain milksnake       |    1|
| Reptiles        | Liodytes (Seminatrix) pygaea                    | North Florida Swapsnake       |    1|
| Reptiles        | Liodytes rigida                                 | Glossy Swampsnake             |    1|
| Reptiles        | Pantherophis gloydi                             | Eastern fox snake             |    1|
| Reptiles        | Pantherophis obsoletus                          | Western Ratsnake              |    1|
| Reptiles        | Phrynosoma goodei                               | Goode''s Horned Lizard        |    1|
| Reptiles        | Plestiodon callicephalus                        | Mountain Skink                |    1|
| Reptiles        | Plestiodon egregius egregius                    | Florida Keys Mole Skink       |    1|
| Reptiles        | Plestiodon egregius insularis                   | Cedar Key Mole Skink          |    1|
| Reptiles        | Plestiodon egregius lividus                     | Blue-tailed Mole Skink        |    1|
| Reptiles        | Plestiodon egregius onocrepis                   | Peninsula Mole Skink          |    1|
| Reptiles        | Plestiodon gilberti arizonensis                 | Arizona Skink                 |    1|
| Reptiles        | Plestiodon gilberti rubricaudata                | Western Red-tailed Skink      |    1|
| Reptiles        | Plestiodon gilberti rubricaudatus               | western red-tailed skink      |    1|
| Reptiles        | Plestiodon inexpectatus                         | Southeastern Five-Lined Skink |    1|
| Reptiles        | Plestiodon multivirgatus multivirgatus          | Northern Many-lined Skink     |    1|
| Reptiles        | Plestiodon reynoldsi                            | Florida Sand Skink            |    1|
| Reptiles        | Plestiodon septentrionalis septentrionalis      | Northern prairie skink        |    1|
| Reptiles        | Plestiodon skiltonianus utahensis               | Great Basin Skink             |    1|
| Reptiles        | Rena dissecta                                   | New Mexico Threadsnake        |    1|
| Reptiles        | Rena dissectus                                  | New Mexico threadsnake        |    1|
| Reptiles        | Sistrurus tergeminus tergeminus                 | Prairie massasauga            |    1|
| Reptiles        | Sistrurus turgeminus                            | Western Massasauga            |    1|
| Reptiles        | Trimorphodon vilkinsonii                        | Chihuahuan Desert lyre snake  |    1|

2005 SGCN Unmatched Names
-------------------------

``` r
sgcn2005 <- noITISmatch[which(noITISmatch$sgcn2005 == 1),]
sgcn2005grouped = sgcn2005 %>%
  group_by(taxonomicgroup, scientificname, commonname) %>%
  summarize(
    n   = n())
```

### Amphibians

| Taxonomic Group | Scientific Name                               | Common Name               |    N|
|:----------------|:----------------------------------------------|:--------------------------|----:|
| Amphibians      | Bufo fowleri formerly Bufo woodhousii fowleri | Fowler''s Toad            |    1|
| Amphibians      | Plethodon glutinosus sensustricto             | Northern Slimy Salamander |    1|
| Amphibians      | Pseudacris triseriata maculata                | Boreal chorus frog        |    1|

### Arachnids

Taxonomic Group Scientific Name Common Name N ---------------- ---------------- ------------ ---

### Birds

| Taxonomic Group | Scientific Name                 | Common Name              |    N|
|:----------------|:--------------------------------|:-------------------------|----:|
| Birds           | Acrocephalus luscinia luscinia  | Nightingale reed-warbler |    1|
| Birds           | Anas discolor                   | Green-winged Teal        |    1|
| Birds           | Gallinago gallinagodelicata     | Wilson''s Snipe          |    1|
| Birds           | Hydranassa caerulea             | Little Blue Heron        |    1|
| Birds           | Laterallus jamaicensis coturnic | California Black Rail    |    1|
| Birds           | Thryothorus bewickii            | Bewick''s Wren           |    1|

### Fish

| Taxonomic Group | Scientific Name                  | Common Name                              |    N|
|:----------------|:---------------------------------|:-----------------------------------------|----:|
| Fish            | \[no scientific name\] 1         | Intertidal and shallow subtidal bivalves |    3|
| Fish            | Cottid Family                    | Forage fish                              |    1|
| Fish            | Galeorhinus cuvier               | Tiger                                    |    1|
| Fish            | Hemipterid Family                | Forage fish                              |    1|
| Fish            | Macrhybopsis aestivalis hyostoma | Shoal Chub                               |    1|
| Fish            | Manta alfredi or birostris       | manta ray                                |    1|
| Fish            | Pholid Family                    | Forage fish                              |    1|
| Fish            | Rhamphocottid Family             | Forage fish                              |    1|
| Fish            | Stichaeid Family                 | Forage fish                              |    1|

### Insects

| Taxonomic Group | Scientific Name                                 | Common Name                     |    N|
|:----------------|:------------------------------------------------|:--------------------------------|----:|
| Insects         | Fixsenia favonius ontario                       | northern hairstreak             |    4|
| Insects         | Incisalia irus                                  | Frosted Elfin                   |    3|
| Insects         | Mitoura hesseli                                 | Hessel''s hairstreak            |    2|
| Insects         | \[not yet named\]                               | Platte River Caddisfly          |    1|
| Insects         | Artogeia virginiensis                           | West Virginia White             |    1|
| Insects         | Celastrina ebenina                              | Dusky azure                     |    1|
| Insects         | Cincindela d. dorsalis                          | Northeastern beach tiger beetle |    1|
| Insects         | Deronectes neomexicanus                         | Bonita Diving Beetle            |    1|
| Insects         | Epidemia epixanthe michiganensis                | Bog Copper                      |    1|
| Insects         | Eurystrymon favonius Ontario                    | Northern hairstreak             |    1|
| Insects         | Fixsenia (Satyrium) ilavia                      | Ilavia Hairstreak               |    1|
| Insects         | Fixsenia favonius                               | Northern Hairstreak             |    1|
| Insects         | Fixsenia ontario                                | Northern hairstreak             |    1|
| Insects         | Fixsenia polingi                                | no common name                  |    1|
| Insects         | Nicrophorus mericanus                           | American burying beetle         |    1|
| Insects         | Orosagrotis perpolita                           | no common name                  |    1|
| Insects         | Paraphlepsius lupalus 1                         | leafhopper                      |    1|
| Insects         | Paraphlepsius lupalus 2                         | no common name                  |    1|
| Insects         | Perdita (Cockerellia) fraticincta (Timberlake)  | no common name                  |    1|
| Insects         | Protandrena (Heterosarus) subglaber (Timberlake | no common name                  |    1|
| Insects         | Satyrium edwardsii Fontigens bottimeri          | Edward''s Hairstreak            |    1|

### Mammals

| Taxonomic Group | Scientific Name                  | Common Name                 |    N|
|:----------------|:---------------------------------|:----------------------------|----:|
| Mammals         | B. bonaerensis                   | Minke whale                 |    1|
| Mammals         | C.e. nelsoni, roosevelti         | Elk (Nooksack herd, mixed)  |    1|
| Mammals         | Eutamias cinereicollis           | Gray-collared Chipmunk      |    1|
| Mammals         | Eutamias quadrivittatus          | Colorado Chipmunk           |    1|
| Mammals         | Eutamias umbrinus                | Uinta Chipmunk              |    1|
| Mammals         | Felis lynx                       | Lynx                        |    1|
| Mammals         | Microtus mexicanus mogollonensis | Mogollon Vole               |    1|
| Mammals         | Mustela vision mink              | Common Mink                 |    1|
| Mammals         | Myotis lebii                     | Eastern Small-footed Myotis |    1|
| Mammals         | Myotis subulatus leibii          | Eastern Small-footed Bat    |    1|
| Mammals         | Neotamias dorsalis               | Cliff Chipmunk              |    1|
| Mammals         | Neotamias minimus atristriatus   | Penasco Least Chipmunk      |    1|
| Mammals         | Neotamias ruficaudus             | Red-tailed Chipmunk         |    1|
| Mammals         | Neotamias umbrinus               | Uinta Chipmunk              |    1|
| Mammals         | Neotoma floridana magister       | Allegheny woodrat           |    1|

### Mollusks

| Taxonomic Group | Scientific Name        | Common Name         |    N|
|:----------------|:-----------------------|:--------------------|----:|
| Mollusks        | D. schmeltziana        | sisi                |    1|
| Mollusks        | Ipnobius robustus      | Robust Tryonia      |    1|
| Mollusks        | Physa utahensis        | Banded Physa        |    1|
| Mollusks        | Plectomerus sloatianus | Purple Bankclimber  |    1|
| Mollusks        | S. caperata            | Wrinkled Marshsnail |    1|
| Mollusks        | S. catascopium         | Woodland Pondsnail  |    1|
| Mollusks        | S. conica              | sisi                |    1|
| Mollusks        | S. thurstoni           | sisi                |    1|
| Mollusks        | S. traski              | Widelip Pondsnail   |    1|
| Mollusks        | V. sincera             | Mossy Valvata       |    1|

### Other

| Taxonomic Group | Scientific Name         | Common Name      |    N|
|:----------------|:------------------------|:-----------------|----:|
| Other           | Family Xanthidae        | Mud Crabs        |    1|
| Other           | Orconectes diogenes     | Devil Crayfish   |    1|
| Other           | Order Anostraca         | no common name   |    1|
| Other           | Stauroneus maunakeensis | no common name   |    1|
| Other           | Varunid crabs           | Freshwater crabs |    1|

### Other Invertebrates

| Taxonomic Group     | Scientific Name             | Common Name                |    N|
|:--------------------|:----------------------------|:---------------------------|----:|
| Other Invertebrates | \[no scientific name\] 2    | Soft coral                 |    3|
| Other Invertebrates | \[no scientific name\] 3    | Corals, tunicates, sponges |    2|
| Other Invertebrates | \[no scientific name\] 4    | Fire corals                |    2|
| Other Invertebrates | \[no scientific name\] 5    | Hydrocorals                |    2|
| Other Invertebrates | \[no scientific name\] 6    | Benthic grazers            |    2|
| Other Invertebrates | \[no scientific name\] 7    | Stony corals               |    2|
| Other Invertebrates | Family porferia             | sponge beds                |    1|
| Other Invertebrates | Oreohelix yavapai cummingsi | Cummings Mountainsnail     |    1|
| Other Invertebrates | Pericelis hymanae Poulter   | Hyman''s flatworm          |    1|
| Other Invertebrates | Subclass Scleractinia       | Hard coral                 |    1|

### Plants

| Taxonomic Group | Scientific Name                    | Common Name              |    N|
|:----------------|:-----------------------------------|:-------------------------|----:|
| Plants          | Gaura neomexicana ssp coloradensis | Colorado Butterfly Plant |    1|
| Plants          | Potamogeton pusillus var. pusillus | Slender Pondweed         |    1|

### Reptiles

| Taxonomic Group | Scientific Name                            | Common Name                  |    N|
|:----------------|:-------------------------------------------|:-----------------------------|----:|
| Reptiles        | Agkistrodon c. mokasen                     | Northern Copperhead          |    1|
| Reptiles        | Aspidoscelis burti                         | Giant Spotted Whiptail       |    1|
| Reptiles        | Cemophora lineri                           | Texas scarlet snake          |    1|
| Reptiles        | Chrysemys guttata                          | Spotted Turtle               |    1|
| Reptiles        | Emys (=Clemmys) marmorata marmorata        | Northwestern Pond Turtle     |    1|
| Reptiles        | Emys (=Clemmys) marmorata pallida          | Southwestern Pond Turtle     |    1|
| Reptiles        | Emys marmorata marmorata                   | northwestern pond turtle     |    1|
| Reptiles        | Heterodon gloydi                           | Dusty hog-nosed snake        |    1|
| Reptiles        | Pituophis melanoleucus melanol             | pine snake                   |    1|
| Reptiles        | Plestiodon septentrionalis obtusirostris   | Southern Prairie Skink       |    1|
| Reptiles        | Plestiodon septentrionalis septentrionalis | Northern prairie skink       |    1|
| Reptiles        | Sceloporus u. hyacinthinus                 | Northern Fence Lizard        |    1|
| Reptiles        | Trimorphodon vilkinsonii                   | Chihuahuan Desert lyre snake |    1|
