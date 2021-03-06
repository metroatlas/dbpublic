C_SF1_Counties_2000 <- function(d = TRUE) {
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  # Data is not dowloadable by script.
  
  # Load data
  counties <- read.csv("data/C_SF1_Counties_2000/DEC_00_SF1_P001.csv",
                       colClasses = "character",
                       header = TRUE,
                       fileEncoding="UTF-8") 
  
  # Transform data
  counties$VD01 <- as.integer(counties$VD01)
  counties$fips <- counties$GEO.id2
  counties$GEO.id2 <- NULL
  counties$fipsstate <- sapply(counties$fips, substr, start = 1, stop = 2)
  counties$fipscounty <- sapply(counties$fips, substr, start = 3, stop = 5)
  
  # Extract only the name of the county
  lastElement <- function(x){x[-1]}
  butLast <- function(x){head(x, -1)}
  
  cnameslist  <- sapply(counties$GEO.display.label, strsplit, split = ",")
  counties$statename <- sapply(cnameslist, lastElement)
  cnameslist <- sapply(cnameslist, butLast)
  
  counties$countyname  <- sapply(cnameslist, paste, collapse = " ")
  
  # Change columns names
  colnames(counties)[3] <- "totalpop2000"
  names(counties) <- gsub("\\.","",names(counties))
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_SF1_Counties_2000", value=counties, overwrite=TRUE)
  dbDisconnect(db)
}