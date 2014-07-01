# Merge data on SMA with data on counties

A_SMA_ByCounties_1950 <- function() {
  
  # Import data
  db <- conma()
  counties  <- dbReadTable(db, "C_SF1_Counties_1950")
  delin     <- dbReadTable(db, "C_MetroDelineations_1950")
  sma      <- dbReadTable(db, "A_SMA_1950")
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Merge  
  co <- merge(
    counties[,c("COUNTY","fipsstate", "fipscounty", "fips", "totalpop1950")],
    delin[,c("sma", "statefp", "countyfp")],
    by.x = c("fipsstate", "fipscounty"),
    by.y = c("statefp","countyfp"),
    all.x = TRUE
  )
  
  co <- merge(co, sma, by = "sma", all.x = T)
  
  # Change column names
  colnames(co)[4] <- "countyname"
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMA_ByCounty_1950", value=co, overwrite=TRUE)
  dbDisconnect(db)
}