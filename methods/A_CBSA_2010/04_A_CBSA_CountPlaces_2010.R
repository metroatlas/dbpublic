# Count the number of incorporated places in a CBSA

A_CBSA_CountPlaces_2010 <- function() {

  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  dbDisconnect(db)
  
  # Get rid of Hawai and Puerto Rico
  places <- places[places$state != "15" & places$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
  #Get rid of the columns of they exist already
  if("CBSAplcount" %in% names(cbsa)) {
    cbsa$CBSAplcount  <- NULL
  }
  
  if("CBSAplby10000" %in% names(cbsa)) {
    cbsa$CBSAplby10000  <- NULL
  }
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  
  # This should be done in a more robust way, i.e. not by row index
  # Tweak for Dothan, AL, that should count for Houston County and not Dale or Henry
  placelist <- placelist[-c(178,283),]
  
  # Tweak for Albany, OR, that should count for Linn County and not Benton
  placelist <- placelist[-c(29250),]
  
  # Places with CBSA and population
  # Only Incorporated places
  placelist <- placelist[placelist$type == 1,]
  # Remove duplicate place
  placelist <- placelist[!duplicated(placelist$fips), ]
  # Merge placelist (with county information) with places
  pl <- merge(placelist, places, by = "fips")
  
  pl <- pl[, c("statefp","countyfp","placefp","statename","countyname","placename.x","totalpop2010")]
  colnames(pl)[6] <- "placename"
  
  pl <- merge(pl, delin, 
              by.x = c("statefp", "countyfp"), 
              by.y = c("FIPSStateCode", "FIPSCountyCode"),
              all.x = TRUE)
  
  pl <- pl[, c("statefp", "countyfp", "placefp", "statename", "countyname", "placename", "totalpop2010", "CBSACode")]
  
  # Count places by CBSA
  pl.count <- aggregate(pl, by = list(pl$CBSACode), FUN = length)[,c(1,2)]
  colnames(pl.count) <- c("CBSACode", "CBSAplcount")
  
  # Merge the count into the cbsa dataframe
  cbsa <- merge(cbsa, pl.count, by ="CBSACode", all.x = T)
  
  # Replace missing values with zeros
  cbsa$CBSAplcount[is.na(cbsa$CBSAplcount)]  <- 0
  cbsa$CBSAplcount <- as.integer(cbsa$CBSAplcount)
  
  # Number of incorporated places by 10'000 inhabitants
  cbsa$CBSAplby10000 <- cbsa$CBSAplcount / cbsa$CBSApop * 10000
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}