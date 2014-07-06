# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1950 census, for Incorporated Places only.

A_SMA_HHI_Places_1950 <- function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_1950")
  delin     <- dbReadTable(db, "C_MetroDelineations_1950")
  sma <- dbReadTable(db, "A_SMA_1950")
  dbDisconnect(db)
  
  # Delete the column if it alread exists
  if("SMAhhipl" %in% names(sma)) {
    sma$SMAhhipl  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$statefp != "15" & places$statefp != "72", ]
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Retain only one county by place
  places <- places[places$icounties == 1,]
  
  # Places with SMA
  # Only Incorporated places
  places <- places[places$Type == 1,]
  
  pl <- places[, c("statefp","countyfp","State","countyname","Place","Population")]
  colnames(pl) <- c("statefp","countyfp","stateabbr","countyname","placename","totalpop1950")
  
  pl <- merge(pl, delin[,1:3], 
              by = c("statefp", "countyfp"))
  
  #Consolidation share of places
  pl <- merge(pl, sma,
              by="sma",
              all = FALSE)
  
  pl$HHI.sma <- (pl$totalpop1950 / pl$smapop)^2
  
  # Consolidation index HHI by SMA
  sma.hhi <- aggregate(pl$HHI.sma, by=list(pl$sma), FUN=sum, na.rm=FALSE)
  names(sma.hhi)  <- c("sma","SMAhhipl")
  
  sma <- merge(sma, sma.hhi, by = "sma")
  
  #sma[sma$SMAhhipl == 1, ]
  #plot(log(sma$smapop), sma$SMAhhipl, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMA_1950", value=sma, overwrite=TRUE)
  dbDisconnect(db)
}