C_SF1_Places_1950 <- function(d = TRUE){
  # Load data
  library(xlsx)
  places <- data.frame()
  
  for(i in 1:48) {
    p <- read.xlsx("data/C_SF1_Places_1950.xlsx",
                   sheetIndex=i,
                   header=TRUE)
    print(i)
    places <- rbind(places,p)
  }
  rm(p)
  
  # Code for incorporated places
  makeType <- function(x){
    if(is.na(x)) {return(1)}
    else {
      if(x == "1" | x =="Uninc" | x == "2") {return(0)}
      else {return(1)}
    }
  }

  places$Type <- sapply(places$Uninc, makeType)
  places$Uninc <- NULL
  places <- places[complete.cases(places),]
  places$County <- as.character(places$County)
  places$Place <- as.character(places$Place)
  
  # Remove extra stars in place names
  trim.star  <- function (x) gsub("^[*]+|[*]+$", "", x)
  trim.white <- function (x) gsub("^\\s+|\\s+$", "", x)
  places$Place <- sapply(places$Place, trim.star)
  places$Place <- sapply(places$Place, trim.white)
  
  # Duplicate rows by county
    places$counties <- sapply(places$County, strsplit, split="[/,]|, ")
    # Number of counties
    places$ncounties <- sapply(places$counties, length)
    # Duplicate rows based on the number of counties
    places.exp <- places[rep(row.names(places), places$ncounties),]
    # Index of the county to keep for each row
    places.exp$icounties <- unlist(sapply(places$ncounties, seq_len))
  
    places <- places.exp
    rm(places.exp)
  
    getByInd <- function(list, index) {
      return(list[index])
    }
  
    places$County <- mapply(getByInd, places$counties, places$icounties)
    places$counties <- NULL
    places$County <- sapply(places$County, trim.white)
  
  # Get counties information
  #db <- conma()
  #counties  <- dbReadTable(db, "C_SF1_Counties_1950")
  #dbDisconnect(db)
  
  # Write table
  db <- conma()  
  dbWriteTable(db, name="C_SF1_Places_1950", value=places, overwrite=TRUE)
  dbDisconnect(db)
}