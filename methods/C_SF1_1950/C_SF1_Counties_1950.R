C_SF1_Counties_1950 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "https://data2.nhgis.org/extracts/101609/6/nhgis0006_csv.zip"
    download.file(fileUrl, destfile="data/C_SF1_Counties_1950.csv.zip", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_SF1_Counties_1950.csv.zip.date.txt")
    unzip(zipfile="data/C_SF1_Counties_1950.csv.zip", exdir="data")
    
    file.remove("data/C_SF1_Counties_1950/C_SF1_Counties_1950.csv", showWarnings = FALSE)
    file.remove("data/C_SF1_Counties_1950/C_SF1_Counties_1950-codebook.txt", showWarnings = FALSE)
    file.remove("data/C_SF1_Counties_1950", showWarnings = FALSE)
    
    file.rename(from="data/nhgis0006_csv", to="data/C_SF1_Counties_1950")
    file.rename(from="data/C_SF1_Counties_1950/nhgis0006_ds83_1950_county.csv", to="data/C_SF1_Counties_1950/C_SF1_Counties_1950.csv")
    file.rename(from="data/C_SF1_Counties_1950/nhgis0006_ds83_1950_county_codebook.txt", to="data/C_SF1_Counties_1950/C_SF1_Counties_1950-codebook.txt")
  }
  
  # Load data
  counties <- read.csv("data/C_SF1_Counties_1950/C_SF1_Counties_1950.csv",
                       colClasses = "character",
                       header = TRUE,
                       fileEncoding="UTF-8") 
  # Transform data
  counties$B1N001 <- as.integer(counties$B1N001)
  counties$fipsstate <- sapply(counties$STATEA, substr, start = 1, stop = 2)
  counties$fipscounty <- sapply(counties$COUNTYA, substr, start = 1, stop = 3)
  counties$fips <- mapply(paste0, counties$fipsstate, counties$fipscounty, MoreArgs = list(collapse = ""))
  colnames(counties)[8] = "totapop1950"
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_SF1_Counties_1950", value=counties, overwrite=TRUE)
  dbDisconnect(db)
}