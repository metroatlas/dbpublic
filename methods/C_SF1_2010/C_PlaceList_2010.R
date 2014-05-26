C_PlaceList_2010 <- function(){
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  
  # Download data
  fileUrl <- "http://www.census.gov/geo/reference/codes/files/national_places.txt"
  download.file(fileUrl, destfile="data/C_national_places_2010.txt", method="curl")
  dateDownloaded <- date()
  write(dateDownloaded,file="data/C_national_places_2010.txt.date.txt")
  
  # Load data
  
  placelist <- read.csv("data/C_national_places_2010.txt",
                        sep="|", header = TRUE,
                        colClasses="character",
                        fileEncoding="UTF-8")
  
  # Transform data
  
  colnames(placelist) <-  tolower(names(placelist))
  
  butLast <- function(x){head(x, -1)}
  placelist$countyname <- as.character(sapply((sapply(strsplit(placelist$county," "), butLast)) , paste, sep=" "))
  placelist$county <- NULL
  
  lastElement <- function(x){x[-1]}
  
  placelist$status  <- as.character(sapply(strsplit(placelist$placename," "), lastElement))
  placelist$placename <- as.character(sapply((sapply(strsplit(placelist$placename," "), butLast)) , paste, sep=" "))
  
  placelist$type <- as.numeric(with(placelist, placelist$type == "Incorporated Place"))
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_PlaceList_2010", value=placelist, overwrite=TRUE)
  dbDisconnect(db) 
  
}