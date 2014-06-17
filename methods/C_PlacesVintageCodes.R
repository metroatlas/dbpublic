C_PlacesVintageCodes <- function(d = TRUE) {
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  
  # Correspondance file between vintage census codes for places and new FIPS codes
  
  # Download data
  if(d) {
    fileUrl <- "http://www2.census.gov/geo/tiger/PREVGENZ/pl/us_places.txt"
    download.file(fileUrl, destfile="data/C_PlacesVintageCodes.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_PlacesVintageCodes.txt.date.txt")
  }
  
  # Read data
  pcodes <- read.fwf("data/C_PlacesVintageCodes.txt",
                        header = FALSE,
                        skip = 1,
                        fileEncoding="UTF-8",
                        widths = c(1,6,1,5,1,100),
                        colClasses="character")
  
  # Select data columns
  pcodes <- pcodes[,c(2,4,6)]
  
  # Rename columns
  colnames(pcodes) <- c("census", "placefips", "placename")
  
  # Code type of place (incorporated or not)
  lastElement <- function(x){tail(x, n=1)}
  pcodes$type  <- as.character(sapply(strsplit(pcodes$placename," "), lastElement))
  pcodes$type <- as.integer(with(pcodes, pcodes$type != "CDP"))
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_PlaceVintageCodes", value=pcodes, overwrite=TRUE)
  dbDisconnect(db) 
}