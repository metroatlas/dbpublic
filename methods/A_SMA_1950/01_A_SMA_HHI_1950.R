# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1950 census.

A_SMA_HHI_1950 <- function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_1950")
  counties  <- dbReadTable(db, "C_SF1_Counties_1950")
  delin     <- dbReadTable(db, "C_MetroDelineations_1950")
  sma.names <- dbReadTable(db, "C_MetroNames_1950")
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$statefp != "15" & places$statefp != "72", ]
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Retain only one county by place
  places <- places[places$icounties == 1,]
  
  # Places with SMA and population
  # Only Incorporated places
  places <- places[places$Type == 1,]
  
  pl <- places[, c("statefp","countyfp","State","countyname","Place","Population")]
  colnames(pl) <- c("statefp","countyfp","stateabbr","countyname","placename","totalpop1950")
  
  pl <- merge(pl, delin[,1:3], 
              by = c("statefp", "countyfp"))
    
  # Counties with SMA and population
  counties <- counties[,c("fipsstate","fipscounty","STATE","COUNTY","totalpop1950")]
  colnames(counties) <- c("statefp","countyfp","statename","countyname","totalpop1950")
  
  co <- merge(counties, delin[,1:3],
              by = c("statefp", "countyfp"))
    
  # Aggregated population by SMA
  plco <- rbind(pl[,c("sma", "totalpop1950")], co[,c("sma", "totalpop1950")])
  sma.pop.aggr <- aggregate(plco$totalpop1950, by=list(plco$sma), FUN=sum, na.rm=FALSE)
  names(sma.pop.aggr)  <- c("sma","smapop.aggr")
  
  # Consolidation share of counties
  co <- merge(co, sma.pop.aggr,
              by="sma",
              all = FALSE)
  
  co$HHI.sma <- (co$totalpop1950 / co$smapop.aggr)^2
  
  #Consolidation share of places
  pl <- merge(pl, sma.pop.aggr,
              by="sma",
              all = FALSE)
  
  pl$HHI.sma <- (pl$totalpop1950 / pl$smapop.aggr)^2
  
  # Sums of squared shares
  plco <- rbind(pl[,c("sma", "HHI.sma")], co[,c("sma", "HHI.sma")])
  
  # Consolidation index HHI by SMA
  sma.hhi <- aggregate(plco$HHI.sma, by=list(plco$sma), FUN=sum, na.rm=FALSE)
  names(sma.hhi)  <- c("sma","smahhi")
  
  # Total population of SMA
  sma.pop <- aggregate(co$totalpop1950, by=list(co$sma), FUN=sum, na.rm=FALSE)
  names(sma.pop)  <- c("sma","smapop")
  
  sma <- merge(sma.pop, sma.hhi, by = "sma")
  sma <- merge(sma, sma.names, by = "sma")
  
  #sma[sma$smahhi == 1, ]
  #plot(log(sma$smapop), sma$smahhi, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMA_1950", value=sma, overwrite=TRUE)
  dbDisconnect(db)
}