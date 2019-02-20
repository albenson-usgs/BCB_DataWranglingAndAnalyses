# Need BuildGC2Tables.R in the DataChecking R Project to be run first
# The sgcn_search table should give us the information we need just need to limit it to SEAFWA

ColoRiverBasin <- c("Utah","Nevada","Arizona","Colorado","New Mexico","California","Wyoming")
CORivBasin_natlist <- sgcn_search[grep(paste(ColoRiverBasin,collapse = "|"),sgcn_search$statelist_2015),]
CORivBasin_birds <- CORivBasin_natlist[which(CORivBasin_natlist$taxonomicgroup == "Birds"),]
write.csv(CORivBasin_birds, file = "ColoRiverBasin_birds.csv", fileEncoding = "UTF-8", row.names = F)
