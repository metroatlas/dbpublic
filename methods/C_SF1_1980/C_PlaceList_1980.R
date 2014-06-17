C_PlaceList_1980 <- function(d = TRUE){
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  
  # Download data
  if(d) {
    fileUrl <- "http://mcdc2.missouri.edu/tmpscratch/17JUN0852628.geocorr90/geocorr90.csv"
    download.file(fileUrl, destfile="data/C_PlaceList_1980.csv", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_PlaceList_1980.csv.zip.date.txt")
  }
  
  # Import data
  placelist <- read.csv("data/C_PlaceList_1980.csv",
                        sep=",", header = TRUE,
                        colClasses="character",
                        na.strings = " ",
                        fileEncoding="UTF-8")
  
  # Keep complete cases only
  placelist <- placelist[complete.cases(placelist),]
  
  # Rname columns
  names(placelist) <- gsub("\\.","",names(placelist))
  colnames(placelist) <- c("placefp", "countyfp", "statefp", "countyname", "placename", "totalpop1990", "cntyallocfactor")

  # Write table
  db <- conma()
  dbWriteTable(db, name="C_PlaceList_1980", value=placelist, overwrite=TRUE)
  dbDisconnect(db)
  
}