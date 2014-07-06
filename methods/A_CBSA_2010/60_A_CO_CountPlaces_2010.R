# Count the number of incorporated places by County

A_CO_CountPlaces_2010  <-  function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  co        <- dbReadTable(db, "A_CBSA_ByCounty_2010")
  dbDisconnect(db)
  
  # Get rid of Hawai and Puerto Rico
  places <- places[places$state != "15" & places$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
  #Get rid of the columns of they exist already
  if("COplcount" %in% names(co)) {
    co$COplcount  <- NULL
  }
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  
  # This should be done in a more robust way, i.e. not by row index
  # Tweak for Dothan, AL, that should count for Houston County and not Dale or Henry
  placelist <- placelist[-c(178,283),]
  
  # Tweak for Albany, OR, that should count for Linn County and not Benton
  placelist <- placelist[-c(29250),]
  
  # Only Incorporated places
  placelist <- placelist[placelist$type == 1,]
  # Remove duplicate place
  placelist <- placelist[!duplicated(placelist$fips), ]
  # Merge placelist (with county information) with places
  pl <- merge(placelist, places, by = "fips")
  
  pl$fips  <- paste(pl$statefp, pl$countyfp, sep="")
  pl <- pl[, c("fips","placefp")]
  
  # Count places by county
  pl.count <- aggregate(pl, by = list(pl$fips), FUN = length)[,c(1,2)]
  colnames(pl.count) <- c("fips", "COplcount")
  
  # Merge the count back into the county dataframe
  co <- merge(co, pl.count, by ="fips", all.x = T)
  
  # Replace missing values with zeros
  co$COplcount[is.na(co$COplcount)]  <- 0
  co$COplcount <- as.integer(co$COplcount)
  
  # Number of incorporated places by 10'000 inhabitants in the county
  co$COplby10000 <- co$COplcount / co$totalpop2010 * 10000
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_ByCounty_2010", value=co, overwrite=TRUE)
  dbDisconnect(db)
}