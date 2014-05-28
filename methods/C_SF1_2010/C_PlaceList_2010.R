C_PlaceList_2010 <- function(d = TRUE){
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  
  # Download data
  if(d){
    fileUrl <- "http://www.census.gov/geo/reference/codes/files/national_places.txt"
    download.file(fileUrl, destfile="data/C_national_places_2010.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_national_places_2010.txt.date.txt")
  }
  
  # Load data
  placelist <- read.csv("data/C_national_places_2010.txt",
                        sep="|", header = TRUE,
                        colClasses="character",
                        fileEncoding="UTF-8")
  
  colnames(placelist) <-  tolower(names(placelist))
  
  # Get rid of Puerto Rico data
  placelist <- placelist[placelist$statefp != "72",]
  
  # Transform data
  butLast <- function(x){head(x, -1)}
  lastElement <- function(x){tail(x, n=1)}
  
  # Duplicate rows by county
    placelist$counties <- sapply(placelist$county, strsplit, split=", ")
    # Number of counties
    placelist$ncounties <- sapply(placelist$counties, length)
    # Duplicate rows based on the number of counties
    placelist.exp <- placelist[rep(row.names(placelist), placelist$ncounties),]
    # Index of the county to keep for each row
    placelist.exp$icounties <- unlist(sapply(placelist$ncounties, seq_len))

    placelist <- placelist.exp
    rm(placelist.exp)
    
    getByInd <- function(list, index) {
      return(list[index])
    }

    placelist$county <- mapply(getByInd, placelist$counties, placelist$icounties)

  placelist$icounties <- NULL
  placelist$counties <- NULL
  
  placelist$status  <- as.character(sapply(strsplit(placelist$placename," "), lastElement))
  placelist$placename <- as.character(sapply((sapply(strsplit(placelist$placename," "), butLast)) , paste, collapse=" "))
  
  placelist$type <- as.integer(with(placelist, placelist$type == "Incorporated Place"))
  
  # Change column name for county
  colnames(placelist)[7] <- "countyname"
  
  # Get county fips
  db <- conma()
  counties  <- dbReadTable(db, "C_SF1_Counties_2010")
  dbDisconnect(db)

  # Reformat counties to merge with placelist
  to.keep <- c("fipsstate", "fipscounty", "countyname")
  counties <- counties[, to.keep]
  colnames(counties) <- c("statefp", "countyfp", "countyname")
  
  # Merge placelist with counties to get county fips in placelist
  placelist.m <- merge(placelist, counties, by = c("statefp", "countyname"), all.x = TRUE)

  # Write table
  db <- conma()
  dbWriteTable(db, name="C_PlaceList_2010", value=placelist.m, overwrite=TRUE)
  dbDisconnect(db) 
  
}