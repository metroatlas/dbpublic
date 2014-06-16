C_SF1_Counties_1980 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "https://data2.nhgis.org/extracts/101609/7/nhgis0007_csv.zip"
    download.file(fileUrl, destfile="data/C_SF1_Counties_1980.csv.zip", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_SF1_Counties_1980.csv.zip.date.txt")
    unzip(zipfile="data/C_SF1_Counties_1980.csv.zip", exdir="data")
    
    file.remove("data/C_SF1_Counties_1980/C_SF1_Counties_1980.csv", showWarnings = FALSE)
    file.remove("data/C_SF1_Counties_1980/C_SF1_Counties_1980-codebook.txt", showWarnings = FALSE)
    file.remove("data/C_SF1_Counties_1980", showWarnings = FALSE)
    
    file.rename(from="data/nhgis0007_csv", to="data/C_SF1_Counties_1980")
    file.rename(from="data/C_SF1_Counties_1980/nhgis0007_ds104_1980_county.csv", to="data/C_SF1_Counties_1980/C_SF1_Counties_1980.csv")
    file.rename(from="data/C_SF1_Counties_1980/nhgis0007_ds104_1980_county_codebook.txt", to="data/C_SF1_Counties_1980/C_SF1_Counties_1980-codebook.txt")
  }
  
  # Load data
  counties <- read.csv("data/C_SF1_Counties_1980/C_SF1_Counties_1980.csv",
                       colClasses = "character",
                       header = TRUE,
                       fileEncoding="UTF-8") 
  # Transform data
  counties$C7L001 <- as.integer(counties$C7L001)

  colnames(counties)[31] = "totapop1950"
  colnames(counties)[8] = "fipsstate"
  colnames(counties)[12] = "fipscounty"
  
  counties$fips <- mapply(paste0, counties$fipsstate, counties$fipscounty, MoreArgs = list(collapse = ""))
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_SF1_Counties_1980", value=counties, overwrite=TRUE)
  dbDisconnect(db)
}