C_SF1_Places_1980 <- function(d = TRUE){
  # Download data
  if(d) {
    fileUrl <- "https://data2.nhgis.org/extracts/101609/8/nhgis0008_csv.zip"
    download.file(fileUrl, destfile="data/C_SF1_Places_1980.csv.zip", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_SF1_Places_1980.csv.zip.date.txt")
    unzip(zipfile="data/C_SF1_Places_1980.csv.zip", exdir="data")
    
    file.remove("data/C_SF1_Places_1980/C_SF1_Places_1980.csv", showWarnings = FALSE)
    file.remove("data/C_SF1_Places_1980/C_SF1_Places_1980-codebook.txt", showWarnings = FALSE)
    file.remove("data/C_SF1_Places_1980", showWarnings = FALSE)
    
    file.rename(from="data/nhgis0008_csv", to="data/C_SF1_Places_1980")
    file.rename(from="data/C_SF1_Places_1980/nhgis0008_ds104_1980_place.csv", to="data/C_SF1_Places_1980/C_SF1_Places_1980.csv")
    file.rename(from="data/C_SF1_Places_1980/nhgis0008_ds104_1980_place_codebook.txt", to="data/C_SF1_Places_1980/C_SF1_Places_1980-codebook.txt")
  }
  
  # Load data
  places <- read.csv("data/C_SF1_Places_1980/C_SF1_Places_1980.csv",
                       colClasses = "character",
                       header = TRUE,
                       fileEncoding="UTF-8") 
  # Transform data
  places$C7L001 <- as.integer(places$C7L001)
  
  colnames(places)[31] = "totalpop1980"
    
  # Write table
  db <- conma()
  dbWriteTable(db, name="C_SF1_Places_1980", value=places, overwrite=TRUE)
  dbDisconnect(db)
}