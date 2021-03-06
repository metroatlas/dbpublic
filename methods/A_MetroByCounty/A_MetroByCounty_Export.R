A_MetroByCounty_Export <- function() {
  
  # Import data
  db <- conma()
  co <- dbReadTable(db, "A_MetroByCounty")
  dbDisconnect(db)
  
  co$id <- co$fips
  
  co <- co[!is.na(co$countyname),]
  
  # Reverse HHI to get dilution of power instead of concentration
  co$CBSAhhi_1950  <- 1 - co$CBSAhhi_1950
  co$CBSAhhi_1980  <- 1 - co$CBSAhhi_1980
  co$CBSAhhi_2010  <- 1 - co$CBSAhhi_2010
  
  co$CBSAhhico_1950  <- 1 - co$CBSAhhico_1950
  co$CBSAhhico_1980  <- 1 - co$CBSAhhico_1980
  co$CBSAhhico_2010  <- 1 - co$CBSAhhico_2010
  
  co$CBSAhhipl_1950  <- 1 - co$CBSAhhipl_1950
  co$CBSAhhipl_1980  <- 1 - co$CBSAhhipl_1980
  co$CBSAhhipl_2010  <- 1 - co$CBSAhhipl_2010
  
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