C_MetroDelineations_2003 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "http://www.census.gov/population/metro/files/lists/2003/030606OMB_CBSA_CSA.xls"
    download.file(fileUrl, destfile="data/C_MetroDelineations_2003.xls", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_MetroDelineations_2003.xls.date.txt")
  }
  
  # Load data
  library(xlsx)
  md <- read.xlsx("data/C_MetroDelineations_2003.xls",
                  sheetIndex=1,
                  colIndex=1:11,
                  rowIndex=3:1840,
                  colClasses="character",
                  header=TRUE)
  
  # Transform names
  names(md) <- gsub("\\.","",names(md))
  md[c(1:4,7:11)]  <- lapply(md[c(1:4,7:11)], as.character)
  md[c(1:3, 6)] <- lapply(md[c(1:3, 6)], as.integer)
  
  # Type
  getType <- function(x) {
    if(x == 2) {return(0)}
    else {return(x)}
  }
  md$Type <- sapply(md$Status1metro2micro, getType)
  md <- md[,c(1:4,7:12)]
  
  # Fips State and County
  md$FIPSStateCode <- sapply(md$FIPS, substr, start = 1, stop = 2)
  md$FIPSCountyCode <- sapply(md$FIPS, substr, start = 3, stop = 5)
  
  # CBSACentralCity
  firstElement <- function(x){x[1]}
  md$CBSACentralCity <- sapply(strsplit(md$CBSATitle,"[-,]"), firstElement)
  
  # CSACentralCity
  md$CSACentralCity <- sapply(strsplit(md$CSATitle,"[-,]"), firstElement)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_MetroDelineations_2003", value=md, overwrite=TRUE)
  dbDisconnect(db) 
}