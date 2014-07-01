A_MetroByCounty() <- function() {
  
  # Import data
  db <- conma()
  co1950 <- dbReadTable(db, "A_SMA_ByCounty_1950")
  co1980 <- dbReadTable(db, "A_SMSA_ByCounty_1980")
  co2010 <- dbReadTable(db, "A_CBSA_ByCounty_2010")
  dbDisconnect(db)
  
  
}