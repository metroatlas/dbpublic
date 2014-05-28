C_CBSA_PrincipalCities_2013 <- function(d = TRUE) {
  
  # Download data
  if(d){
    fileUrl <- "http://www.census.gov/population/metro/files/lists/2013/List2.xls"
    download.file(fileUrl, destfile="data/C_CBSA_PrincipalCities_201302.xls", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_CBSA_PrincipalCities_201302.xls.date.txt")
  }
  
  # Load data
  library(xlsx)
  pcities <- read.xlsx("data/C_CBSA_PrincipalCities_201302.xls",
                       sheetIndex=1,
                       colIndex=1:6,
                       rowIndex=3:1252,
                       colClasses="character",
                       header=TRUE)
  
  # Transform names
  names(pcities) <- gsub("\\.","",names(pcities))
  pcities[c(1,2,4:6)]  <- lapply(pcities[c(1,2,4:6)], as.character)
  
  # Type
  pcities$MetropolitanMicropolitanStatisticalArea <- factor(pcities$MetropolitanMicropolitanStatisticalArea, levels = c("Micropolitan Statistical Area", "Metropolitan Statistical Area"))
  pcities$Type <- as.integer(pcities$MetropolitanMicropolitanStatisticalArea) - 1
  pcities$Type <- as.integer(pcities$Type)
  pcities$MetropolitanMicropolitanStatisticalArea <- NULL
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_CBSA_PrincipalCities_20132", value=pcities, overwrite=TRUE)
  dbDisconnect(db)
  
  
}