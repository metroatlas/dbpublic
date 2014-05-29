# Add an MDPI2007 column to the CBSA table

A_CBSA_AddMDPI_2007 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  mdpi      <- dbReadTable(db, "R_CBSA_MDPI_2007")[,c(1,3)]
  dbDisconnect(db)
  
  # Delete column if exists
  if("CBSAmdpi2007" %in% names(cbsa)) {
    cbsa$CBSAmdpi2007  <- NULL
  }
  
  colnames(mdpi) <- c("CBSACode", "CBSAmdpi2007")
  
  # Merge the mdpi colum n the cbsa dataframe
  cbsa <- merge(cbsa, mdpi, by="CBSACode", all.x = T)
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}
