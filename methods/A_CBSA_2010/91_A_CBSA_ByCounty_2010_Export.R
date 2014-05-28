A_CBSA_ByCounty_2010_Export <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  co    <- dbReadTable(db, "A_CBSA_ByCounty_2010")
  
  # Close database connection
  dbDisconnect(db)
  
  co$id <- co$fips
  
  # Reverse HHI to get dilution of power instead of concentration
  co$CBSAhhi  <- 1 - co$CBSAhhi
  co$CBSAhhico  <- 1 - co$CBSAhhico
  co$CBSAhhipl  <- 1 - co$CBSAhhipl
  
  # Export data
  write.table(co,
              file = "export/A_CBSA_ByCounty_2010.csv",
              sep = ",",
              row.names = FALSE,
              col.names = TRUE,
              fileEncoding = "UTF-8")
}
