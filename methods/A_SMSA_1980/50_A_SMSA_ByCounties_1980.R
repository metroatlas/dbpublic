# Merge data on SMSA with data on counties

A_SMSA_ByCounties_1980 <- function() {
  
  # Import data
  db <- conma()
  counties  <- dbReadTable(db, "C_SF1_Counties_1980")
  delin     <- dbReadTable(db, "C_MetroDelineations_1981")
  smsa      <- dbReadTable(db, "A_SMSA_1980")
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Merge  
  co <- merge(
    counties[,c("COUNTY","fipsstate", "fipscounty", "fips", "totalpop1980")],
    delin[,c("smsa", "statefp", "countyfp")],
    by.x = c("fipsstate", "fipscounty"),
    by.y = c("statefp","countyfp"),
    all.x = TRUE
  )
  
  co <- merge(co, smsa, by = "smsa", all.x = T)
  
  # Change column names
  colnames(co)[4] <- "countyname"
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMSA_ByCounty_1980", value=co, overwrite=TRUE)
  dbDisconnect(db)
}