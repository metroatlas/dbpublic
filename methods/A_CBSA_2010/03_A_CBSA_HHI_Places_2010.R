# Computes the Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for Incorporated Places only for the 2010 Census

A_CBSA_HHI_Places_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  places    <- dbReadTable(db, "C_SF1_Places_2010")
  counties  <- dbReadTable(db, "C_SF1_Counties_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  dbDisconnect(db)
  
  if("CBSAhhipl" %in% names(cbsa)) {
    cbsa$CBSAhhipl  <- NULL
  }
  
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$state != "15" & places$state != "72", ]
  counties <- counties[counties$state != "15" & counties$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
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
  
  pl <- merge(pl, cbsa, by = "CBSACode")
  
  # Place concentration share
  pl$CBSAhhipl <- (pl$totalpop2010 / pl$CBSApop)^2
  
  # Place consolidation index HHI by CBSA
  cbsa.hhipl <- aggregate(pl$CBSAhhipl, by=list(pl$CBSACode), FUN=sum, na.rm=FALSE)
  colnames(cbsa.hhipl) <- c("CBSACode", "CBSAhhipl")
  
  cbsa <- merge(cbsa, cbsa.hhipl, by = "CBSACode", all.x = TRUE)
  cbsa <- cbsa[complete.cases(cbsa),]
  
  # Controls
  # cbsa[cbsa$CBSAhhipl > 1, ]
  # plot(log(cbsa$CBSApop), cbsa$CBSAhhipl, pch = 20)
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}