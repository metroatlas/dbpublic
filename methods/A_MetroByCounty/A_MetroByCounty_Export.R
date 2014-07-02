A_MetroByCounty_Export <- function() {
  
  # Import data
  db <- conma()
  co <- dbReadTable(db, "A_MetroByCounty")
  dbDisconnect(db)
  
  co$id <- co$fips
  
  # Reverse HHI to get dilution of power instead of concentration
  co$CBSAhhi_1950  <- 1 - co$CBSAhhi_1950
  co$CBSAhhi_1980  <- 1 - co$CBSAhhi_1980
  co$CBSAhhi_2010  <- 1 - co$CBSAhhi_2010
  
  co$Deltahhi30y_1980  <-  co$Deltahhi30y_1980 * -1
  co$Deltahhi30y_2010  <-  co$Deltahhi30y_2010 * -1
  
  # Export data
  write.table(co,
              file = "export/A_MetroByCounty.csv",
              sep = ",",
              row.names = FALSE,
              col.names = TRUE,
              fileEncoding = "UTF-8") 
}