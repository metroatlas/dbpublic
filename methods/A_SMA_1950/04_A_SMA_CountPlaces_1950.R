# Count the number of incorporated places in a SMSA

A_SMA_CountPlaces_1950 <- function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_1950")
  delin     <- dbReadTable(db, "C_MetroDelineations_1950")
  sma       <- dbReadTable(db, "A_SMA_1950")
  dbDisconnect(db)
  
  # Delete the column if it alread exists
  if("SMAplcount" %in% names(sma)) {
    smsa$SMSAplcount  <- NULL
  }
  
  if("SMAplby10000" %in% names(sma)) {
    smsa$SMSAplby10000  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$statefp != "15" & places$statefp != "72", ]
  
  # Retain only one county by place
  places <- places[places$icounties == 1,]
  
  # Places with SMA
  # Only Incorporated places
  places <- places[places$Type == 1,]
  
  pl <- places[, c("statefp","countyfp","State","countyname","Place","Population")]
  colnames(pl) <- c("statefp","countyfp","stateabbr","countyname","placename","totalpop1950")
  
  pl <- merge(pl, delin[,1:3], 
              by = c("statefp", "countyfp"))
  
  # Count places by SMA
  pl.count <- aggregate(pl, by = list(pl$sma), FUN = length)[,c(1,2)]
  colnames(pl.count) <- c("sma", "SMAplcount")
  
  # Merge the count into the SMA dataframe
  sma <- merge(sma, pl.count, by ="sma", all.x = T)
  
  # Replace missing values with zeros
  sma$SMAplcount[is.na(sma$SMAplcount)]  <- 0
  sma$SMAplcount <- as.integer(sma$SMAplcount)
  
  # Number of incorporated places by 10'000 inhabitants
  sma$SMAplby10000 <- sma$SMAplcount / sma$smapop * 10000
  
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMA_1950", value=sma, overwrite=TRUE)
  dbDisconnect(db)
}