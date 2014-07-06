# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1950 census, for counties only.

A_SMA_HHI_Counties_1950 <- function() {
  
  # Import data
  db <- conma()
  counties  <- dbReadTable(db, "C_SF1_Counties_1950")
  delin     <- dbReadTable(db, "C_MetroDelineations_1950")
  sma       <- dbReadTable(db, "A_SMA_1950")
  dbDisconnect(db)
  
  # Delete the column if it alread exists
  if("SMAhhico" %in% names(sma)) {
    sma$SMAhhico  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$statefp != "15" & places$statefp != "72", ]
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Counties with SMA
  counties <- counties[,c("fipsstate","fipscounty","STATE","COUNTY","totalpop1950")]
  colnames(counties) <- c("statefp","countyfp","statename","countyname","totalpop1950")
  
  co <- merge(counties, delin[,1:3],
              by = c("statefp", "countyfp"))
  
  # Consolidation share of counties
  co <- merge(co, sma,
              by="sma",
              all = FALSE)
  
  co$HHI.sma <- (co$totalpop1950 / co$smapop)^2
  
  # Consolidation index HHI by SMA
  sma.hhi <- aggregate(co$HHI.sma, by=list(co$sma), FUN=sum, na.rm=FALSE)
  names(sma.hhi)  <- c("sma","SMAhhico")
  
  
  sma <- merge(sma, sma.hhi, by = "sma")
  
  #sma[sma$SMAhhico == 1, ]
  #plot(log(sma$smapop), sma$SMAhhico, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMA_1950", value=sma, overwrite=TRUE)
  dbDisconnect(db)
}