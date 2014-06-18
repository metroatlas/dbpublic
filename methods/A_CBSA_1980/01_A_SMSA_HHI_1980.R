# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1980 census.

A_SMSA_HHI_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  places    <- dbReadTable(db, "C_SF1_Places_1980")
  counties  <- dbReadTable(db, "C_SF1_Counties_1980")
  placelist <- dbReadTable(db, "C_PlaceList_1980")
  pcodes    <- dbReadTable(db, "C_PlacesVintageCodes")
  smsa      <- dbReadTable(db, "C_MetroNames_1981")
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$STATEA != "15" & places$STATEA != "72", ]
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  
  pcodes$statefp <- sapply(pcodes$census, substr, start = 1, stop = 2)
  colnames(pcodes)[2] <- "placefp"
  pcodes <- pcodes[pcodes$statefp != "15" & pcodes$statefp != "72", ]
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  
  # Merge places and pcodes to get fips codes for 1980 places
  places$census <- mapply(paste0, places$STATEA, places$PLACEA, MoreArgs = list(collapse = ""))
  places <- merge(places, pcodes,
                  by="census")
  places$fips <- mapply(paste0, places$statefp, places$placefp, MoreArgs = list(collapse = ""))
  
  isNumber <- function(x){
    if(length(grep("[0-9]+",x) > 0)) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  
  # Places with SMSA and population
  # Only Incorporated places
  places <- places[places$type == 1,]
  # Sort placelist by allocation factor to assign a place to the county where it has the most population
  placelist <- placelist[with(placelist, order(cntyallocfactor, decreasing = TRUE)),]
  # Remove duplicate place
  # Because the dataframe is now sorted, it will get rid of duplicates with the smallest share of population
  placelist <- placelist[!duplicated(placelist$fips), ]
  # Merge placelist (with county information) with places
  pl <- merge(places, placelist, by = c("fips", "placefp", "statefp"))
  
  # Clean the columns
  pl <- pl[, c("fips","statefp","placefp","countyfp", "STATE", "countyname","placename.x","totalpop1980")]
  colnames(pl)[5] <- "statename"
  colnames(pl)[7] <- "placename"
  
  # Counties with SMSA
  ctytomerge <- counties[,c("fips", "SMSAA")]
  colnames(ctytomerge) <- c("countyfp", "SMSA")
  
  pl <- merge(pl, ctytomerge, by = "countyfp")
  rm(ctytomerge)
    
  # Counties with SMSA and population
  co <- counties[,c("fipsstate", "fipscounty", "fips", "STATE", "COUNTY", "totalpop1980", "SMSAA")]
  colnames(co) <- c("fipsstate", "fipscounty", "fips", "statename", "countyname", "totalpop1980", "SMSA")
  
  # Aggregated population by SMSA
  plco <- rbind(pl[,c("SMSA", "totalpop1980")], co[,c("SMSA", "totalpop1980")])
  smsa.pop.aggr <- aggregate(plco$totalpop1980, by=list(plco$SMSA), FUN=sum, na.rm=FALSE)
  names(smsa.pop.aggr)  <- c("SMSA","SMSApop.aggr")
  smsa.pop.aggr <- smsa.pop.aggr[smsa.pop.aggr$SMSA != "", ]
  
  # Consolidation share of counties
  co <- merge(co, smsa.pop.aggr,
              by="SMSA",
              all = FALSE)
  
  co$HHI.smsa <- (co$totalpop1980 / co$SMSApop.aggr)^2
  
  #Consolidation share of places
  pl <- merge(pl, smsa.pop.aggr,
              by="SMSA",
              all = FALSE)
  
  pl$HHI.smsa <- (pl$totalpop1980 / pl$SMSApop.aggr)^2
  
  # Sums of squared shares
  plco <- rbind(pl[,c("SMSA", "HHI.smsa")], co[,c("SMSA", "HHI.smsa")])
  
  # Consolidation index HHI by SMSA
  smsa.hhi <- aggregate(plco$HHI.smsa, by=list(plco$SMSA), FUN=sum, na.rm=FALSE)
  names(smsa.hhi)  <- c("SMSA","SMSAhhi")
  
  # Total population of CBSA
  smsa.pop <- aggregate(co$totalpop1980, by=list(co$SMSA), FUN=sum, na.rm=FALSE)
  names(smsa.pop)  <- c("SMSA","SMSApop")
  
  smsa.temp <- merge(smsa.pop, smsa.hhi, by = "SMSA")
  colnames(smsa) <- c("SMSA", "SCSA", "SMSAname")
  smsa <- merge(smsa, smsa.temp, by = "SMSA")
  
  #smsa[smsa$SMSAhhi == 1, ]
  #plot(log(smsa$SMSApop), smsa$SMSAhhi, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}