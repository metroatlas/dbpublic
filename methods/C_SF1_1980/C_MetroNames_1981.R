C_MetroNames_1981 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "http://www.census.gov/population/metro/files/lists/historical/80mfips.txt"
    download.file(fileUrl, destfile="data/C_MetroDelineations_1981.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_MetroDelineations_1981.txt.date.txt")
  }
  
  # Import data
  smsa <- read.fwf(file="data/C_MetroDelineations_1981.txt",
                   skip = 17,
                   n = 1868,
                   widths = c(4,4,2,6,2,3,3,5,7,63))
  
  # Get SMSA lines only (not county delineations)
  smsa <- smsa[,c(1,3,5,6,10)]
  colnames(smsa) <- c("smsa", "scsa", "statefp", "countyfp", "smsaname")
  smsa <- smsa[complete.cases(smsa),]
  smsa[] <- lapply(smsa, as.character)
  
  smsa.names <- smsa[smsa$countyfp == "   ",c(1,2,5)]
  
  butLast <- function(x){head(x, -1)}
  snameslist <- sapply(smsa.names$smsaname, strsplit, split = " ")
  snameslist <- sapply(snameslist, butLast)
  smsa.names$smsaname  <- sapply(snameslist, paste, collapse = " ")
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_MetroNames_1981", value=smsa.names, overwrite=TRUE)
  dbDisconnect(db)
  
}