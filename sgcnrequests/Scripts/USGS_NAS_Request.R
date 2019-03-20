# Data request from USGS Non-indigenous Aquatic Species Database

# Use case from Wesley Daniel on 2019-03-20:
# The NAS Database group is creating a set of impact tables to help inform management of the potential consequences 
# (ecological and economic impacts) of non-native aquatic species. In these tables, we are tracking native species that have 
# been or potentially been impacted by the introduction of a non-native aquatic species. We would like a list of aquatic SGCN 
# species for the US, so we could reference if any of these interactions are with a management priority species.

# For this request I think we should include species that were not able to find a match in ITIS or WoRMS but have been identified
# by either FWS or NatureServe and limit to the 2015 species which had slightly more rigorous conditions applied to creating the 
# lists and the data collection process for USGS is less error prone.

# Note the sgcn_search table needs to be created first using the BuildGC2Tables.R script.

NAS_subset <- sgcn_search

# Limit to only 2015 species
NAS_subset <- NAS_subset[which(NAS_subset$statelist_2015 != ""),]

# Set aside the species that were matched in either ITIS or WoRMS but do not have a FWS listing status or NatureServe status
matched_NAS_subset <- NAS_subset[which(NAS_subset$matchmethod != "Not Matched"),]
matched_NAS_subset <- matched_NAS_subset[which(is.na(matched_NAS_subset$ListingStatus) & 
                                                         is.na(matched_NAS_subset$nsNationalStatus)),]

# Filter out any species that were not found in FWS or NatureServe
NAS_subset <- NAS_subset[which(!is.na(NAS_subset$ListingStatus) | !is.na(NAS_subset$nsNationalStatus)),]

# Add the matched subset back in
NAS_subset <- rbind(NAS_subset, matched_NAS_subset)

# Take out the NatureServe status since the data sharing policy for that information still needs to be worked out
NAS_subset <- NAS_subset[c(-6,-7)]

write.csv(NAS_subset, file = "USGS_2015_SGCN_Species_20190320.csv", fileEncoding = "UTF-8", row.names = F)
