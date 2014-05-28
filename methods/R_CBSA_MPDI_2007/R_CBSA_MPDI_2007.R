R_CBSA_MDPI_2007 <- function(d = TRUE) {
  
  # Download data
  if(d){
    fileUrl <- "http://www.metrostudies.pitt.edu/LinkClick.aspx?fileticket=9khw3u0Ihu8%3d&tabid=1321"
    download.file(fileUrl, destfile="data/CBSA_MDPI_2007.xlsx", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/CBSA_MDPI_2007.xlsx.date.txt")
  }
  
  # Load data
  library(xlsx)
  mdpi.2007 <- read.xlsx("data/CBSA_MDPI_2007.xlsx",
                         sheetIndex=1,
                         #colClasses="character",
                         header=TRUE)
  
  # Write table
  db <- conma()  
  dbWriteTable(db, name="R_CBSA_MDPI_2007", value=mdpi.2007, overwrite=TRUE)
  dbDisconnect(db)
}