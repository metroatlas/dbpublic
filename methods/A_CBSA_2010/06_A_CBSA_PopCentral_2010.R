# Proportion of metropolitan population in central cities

A_CBSA_PopCentral_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  places    <- dbReadTable(db, "C_SF1_Places_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  pcities   <- dbReadTable(db, "C_CBSA_PrincipalCities_20132")
  dbDisconnect(db)
  
  pcities$CBSAType <- pcities$Type
  pcities$Type <- NULL
  
  # Delete columns if exist
  if("CBSApcpop" %in% names(cbsa)) {
    cbsa$CBSApcpop  <- NULL
  }
  
  if("CBSApcprop" %in% names(cbsa)) {
    cbsa$CBSApcprop  <- NULL
  }
  
  # Get rid of Hawai and Puerto Rico
  places <- places[places$state != "15" & places$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  pcities$fips <- paste(pcities$FIPSStateCode, pcities$FIPSPlaceCode, sep="")
  
  # This should be done in a more robust way, i.e. not by row index
  # Tweak for Dothan, AL, that should count for Houston County and not Dale or Henry
  placelist <- placelist[-c(178,283),]
  
  # Tweak for Albany, OR, that should count for Linn County and not Benton
  placelist <- placelist[-c(29250),]
  
  # Get population for principal cities
  pcities <- merge(pcities, places[,c("fips", "totalpop2010", "placetype")], by="fips", all.x = T)
  
  # Total population of cbsa principal cities
  pcitiespop <- pcities[,c("CBSACode", "totalpop2010")]
  pcitiespop$CBSACode <- as.integer(pcitiespop$CBSACode)
  
  cbsa.pcpop <- aggregate(pcitiespop$totalpop2010, by = list(pcitiespop$CBSACode), FUN = sum)
  colnames(cbsa.pcpop) <- c("CBSACode", "CBSApcpop")
  
  # Merge with cbsa table
  cbsa <- merge(cbsa, cbsa.pcpop, by = "CBSACode", all.x = T)
  cbsa$CBSApcprop <- cbsa$CBSApcpop / cbsa$CBSApop
  
  # Controls
  # cbsa[cbsa$CBSApcprop > 1,]
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}
