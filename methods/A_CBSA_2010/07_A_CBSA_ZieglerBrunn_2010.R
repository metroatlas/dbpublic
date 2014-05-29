# Ziegler-Brunn Index

A_CBSA_ZieglerBrunn_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  
  # Delete column if exists
  if("CBSAzb" %in% names(cbsa)) {
    cbsa$CBSAzb  <- NULL
  }
  
  # Compute Ziegler-Brunn
  cbsa$CBSAzb <- cbsa$CBSAplby10000 / cbsa$CBSApcprop
  
  ####### WRITE TABLE ############
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  
  dbDisconnect(db)
}
