# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1980 census, for Incorporated Places only.

A_SMSA_HHI_Places_1980 <- function() {
  
  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_1980")
  placelist <- dbReadTable(db, "C_PlaceList_1980")
  pcodes    <- dbReadTable(db, "C_PlacesVintageCodes")
  smsa      <- dbReadTable(db, "A_SMSA_1980")
  delin     <- dbReadTable(db, "C_MetroDelineations_1981")
  dbDisconnect(db)
  
  # Delete the column if it alread exists
  if("SMSAhhipl" %in% names(smsa)) {
    smsa$SMSAhhipl  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$STATEA != "15" & places$STATEA != "72", ]
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
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
  
  # Places with SMSA and population
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
  pl <- pl[, c("fips","statefp","placefp","countyfp", "STATE", "countyname","placename.x","totalpop1980")]
  colnames(pl)[4] <- "countyfips"
  colnames(pl)[5] <- "statename"
  colnames(pl)[7] <- "placename"
  
  # Delineations of SMSA
  delin$countyfips <- mapply(paste0, delin$statefp, delin$countyfp, MoreArgs = list(collapse = ""))
  pl <- merge(pl, delin, by = "countyfips")
  pl <- pl[, c("fips","statefp.x","placefp","countyfp", "statename", "countyname.x","placename","totalpop1980", "smsa")]
  colnames(pl)[2] <- "statefp"
  colnames(pl)[6] <- "countyname"
  
  #Consolidation share of places
  pl <- merge(pl, smsa,
              by="smsa",
              all = FALSE)
  
  pl$HHI.smsa <- (pl$totalpop1980 / pl$SMSApop)^2
  
  # Consolidation index HHI by SMSA
  smsa.hhipl <- aggregate(pl$HHI.smsa, by=list(pl$smsa), FUN=sum, na.rm=FALSE)
  names(smsa.hhipl)  <- c("smsa","SMSAhhipl")
  
  smsa <- merge(smsa, smsa.hhipl,
                by = "smsa")
  
  #smsa[smsa$SMSAhhipl == 1, ]
  #plot(log(smsa$SMSApop), smsa$SMSAhhipl, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMSA_1980", value=smsa, overwrite=TRUE)
  dbDisconnect(db)
}