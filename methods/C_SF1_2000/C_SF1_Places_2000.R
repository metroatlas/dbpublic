C_SF1_Places_2000 <- function(d = TRUE) {
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  # Data is not dowloadable by script.
  
  # Load data
  places <- read.csv("data/C_SF1_Places_2000/DEC_00_SF1_P001.csv",
                       colClasses = "character",
                       header = TRUE,
                       fileEncoding="UTF-8")
  
  # Transform data
  places$VD01 <- as.integer(places$VD01)
  places$fips <- places$GEO.id2
  places$GEO.id2 <- NULL
  places$fipsstate  <- sapply(places$fips, substr, start = 1, stop = 2)
  places$fipsplace <- sapply(places$fips, substr, start = 3, stop = 7)
  
  # Extract only the name of the places
  lastElement <- function(x){tail(x, n=1)}
  butLast <- function(x){head(x, -1)}
  
  pnameslist  <- sapply(places$GEO.display.label, strsplit, split = ",")
  places$statename <- sapply(pnameslist, lastElement)
  pnameslist <- sapply(pnameslist, butLast)
  
  pnameslist <- sapply(pnameslist, strsplit, split = " ")
  
  places$placename  <- sapply((sapply(pnameslist, butLast)), paste, collapse = " ")
  places$placetype  <- sapply((sapply(pnameslist, lastElement)), paste, collapse = " ")
  
  # Change columns names
  colnames(places)[3] <- "totalpop2000"
  names(places) <- gsub("\\.","",names(places))
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_SF1_Places_2000", value=places, overwrite=TRUE)
  dbDisconnect(db)
}