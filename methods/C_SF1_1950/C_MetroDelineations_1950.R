C_MetroDelineations_1950 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "https://www.census.gov/population/metro/files/lists/historical/50mfips.txt"
    download.file(fileUrl, destfile="data/C_MetroDelineations_1950.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_MetroDelineations_1950.txt.date.txt")
  }
  
  # Import data
  sma <- read.fwf(file="data/C_MetroDelineations_1950.txt",
                  skip = 14,
                  n = 861,
                  colClasses = "character",
                  strip.white = TRUE,
                  fill = TRUE,
                  widths = c(4,8,2,3,3,5,9,65))
  
  # Get SMSA lines only (not county delineations)
  sma <- sma[,c(1,3,4,6,8)]
  colnames(sma) <- c("sma", "statefp", "countyfp", "entityfp", "countyname")
  sma <- sma[complete.cases(sma),]
  sma <- sma[sma$countyfp != "" & sma$entityfp == "",]
  sma <-sma[,c(1:3,5)]
  
  butLast <- function(x){head(x, -1)}
  snameslist <- sapply(sma.names$smaname, strsplit, split = " ")
  snameslist <- sapply(snameslist, butLast)
  sma.names$smaname  <- sapply(snameslist, paste, collapse = " ")
  
  # Compound FIPS for counties
  sma$fips <- mapply(paste0, sma$statefp, sma$countyfp, MoreArgs = list(collapse = ""))
  
  # Remove states from county names
  firstElement <- function(x){return(x[1])}
  sma$countyname <- sapply(sma$countyname, strsplit, split = ",")
  sma$countyname <- sapply(sma$countyname, firstElement)
  
  # Remove duplicate counties
  sma <- sma[!duplicated(sma$fips),]
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_MetroDelineations_1950", value=sma, overwrite=TRUE)
  dbDisconnect(db)
}