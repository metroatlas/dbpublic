# Merge data on CBSA with data on counties
A_CBSA_ByCounties_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  counties  <- dbReadTable(db, "C_SF1_Counties_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  dbDisconnect(db)
  
  counties <- counties[counties$state != "15" & counties$state != "72", ]
  
  # Merge  
  co <- merge(
    counties,
    delin[,c("CBSACode", "FIPSStateCode", "FIPSCountyCode")],
    by.x = c("fipsstate", "fipscounty"),
    by.y = c("FIPSStateCode","FIPSCountyCode"),
    all.x = TRUE
  )
  
  co <- merge(co, cbsa, by = "CBSACode", all.x = T)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_CBSA_ByCounty_2010", value=co, overwrite=TRUE)
  dbDisconnect(db)
}