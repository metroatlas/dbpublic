# Export the table of CBSA 2010 values to csv

A_CBSA_2010_Export <- function() {
  # Connect to database
  db <- conma()

  # Import data
  cbsa    <- dbReadTable(db, "A_CBSA_2010")
  
  # Export data
  write.table(cbsa,
              file = "export/A_CBSA_2010.csv",
              sep = ",",
              row.names = FALSE,
              col.names = TRUE,
              fileEncoding = "UTF-8")
  
  # Close database connection
  dbDisconnect(db)
}
