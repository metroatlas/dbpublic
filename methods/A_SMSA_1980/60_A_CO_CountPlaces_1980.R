# Count the number of incorporated places by county

A_CO_CountPlaces_1980 <- function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_1980")
  placelist <- dbReadTable(db, "C_PlaceList_1980")
  pcodes    <- dbReadTable(db, "C_PlacesVintageCodes")
  co        <- dbReadTable(db, "A_SMSA_ByCounty_1980")
  dbDisconnect(db)
  
  #Get rid of the columns of they exist already
  if("COplcount" %in% names(co)) {
    co$COplcount  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$STATEA != "15" & places$STATEA != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  
  pcodes$statefp <- sapply(pcodes$census, substr, start = 1, stop = 2)
  colnames(pcodes)[2] <- "placefp"
  pcodes <- pcodes[pcodes$statefp != "15" & pcodes$statefp != "72", ]
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  
  # Merge places and pcodes to get fips codes for 1980 places
  places$census <- mapply(paste0, places$STATEA, places$PLACEA, MoreArgs = list(collapse = ""))
  places <- merge(places, pcodes,
                  by="census")
  places$fips <- mapply(paste0, places$statefp, places$placefp, MoreArgs = list(collapse = ""))
  
  isNumber <- function(x){
    if(length(grep("[0-9]+",x) > 0)) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  
  # Only Incorporated places
  places <- places[places$type == 1,]
  # Sort placelist by allocation factor to assign a place to the county where it has the most population
  placelist <- placelist[with(placelist, order(cntyallocfactor, decreasing = TRUE)),]
  # Remove duplicate places
  # Because the dataframe is now sorted, it will get rid of duplicates with the smallest share of population
  placelist <- placelist[!duplicated(placelist$fips), ]
  # Merge placelist (with county information) with places
  pl <- merge(places, placelist, by = c("fips", "placefp", "statefp"))
  
  # Clean the columns
  pl <- pl[, c("placefp","countyfp")]
  colnames(pl)[2] <- "fips"
  
  # Count places by county
  pl.count <- aggregate(pl, by = list(pl$fips), FUN = length)[,c(1,2)]
  colnames(pl.count) <- c("fips", "COplcount")
  
  # Merge the count back into the county dataframe
  co <- merge(co, pl.count, by ="fips", all.x = T)
  
  # Replace missing values with zeros
  co$COplcount[is.na(co$COplcount)]  <- 0
  co$COplcount <- as.integer(co$COplcount)
  
  # Number of incorporated places by 10'000 inhabitants
  co$COplby10000 <- co$COplcount / co$totalpop1980 * 10000
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_SMSA_ByCounty_1980", value=co, overwrite=TRUE)
  dbDisconnect(db)
}