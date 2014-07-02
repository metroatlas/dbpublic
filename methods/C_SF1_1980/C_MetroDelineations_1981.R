C_MetroDelineations_1981 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "http://www.census.gov/population/metro/files/lists/historical/80mfips.txt"
    download.file(fileUrl, destfile="data/C_MetroDelineations_1981.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_MetroDelineations_1981.txt.date.txt")
  }
  
  # Import data
  db <- conma()
  smsa      <- dbReadTable(db, "C_MetroNames_1981")
  dbDisconnect(db)
  
  # Import data
  delin <- read.fwf(file="data/C_MetroDelineations_1981.txt",
                   skip = 19,
                   n = 1865,
                   colClasses = "character",
                   widths = c(4,4,2,6,2,3,3,5,9,61))
  
  # Get county lines only (not SMSA names)
  delin <- delin[,c(1,3,5,6,8,10)]
  colnames(delin) <- c("smsa", "scsa", "statefp", "countyfp", "placefp", "countyname")
  delin <- delin[complete.cases(delin),]
  delin <- delin[delin$placefp == "     ",]
  delin$placefp <- NULL
  
  delin <- delin[delin$countyfp != "   ",]
  
  delin[] <- lapply(delin, as.character)
  delin <- delin[!is.na(delin$countyfp),]
  delin <- delin[delin$countyfp != "   ",]
  
  # Merge with SMSA names
  delin <- merge(delin, smsa[,c(1,3)], by = "smsa")
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_MetroDelineations_1981", value=delin, overwrite=TRUE)
  dbDisconnect(db)
  
}