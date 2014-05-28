# Computes the Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for Counties only for the 2010 Census

A_CBSA_HHI_Counties_2010 <- function() {
  # Connect to database
  db <- conma()
  
  # Import data
  counties  <- dbReadTable(db, "C_SF1_Counties_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  cbsa      <- dbReadTable(db, "A_CBSA_2010")
  
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  counties <- counties[counties$state != "15" & counties$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
  # Delete the column if it alread exists
  if("CBSAhhico" %in% names(cbsa)) {
    cbsa$CBSAhhico  <- NULL
  }
  
  # HHI Counties
  
  co <- merge(counties, delin,
              by.x = c("fipsstate", "fipscounty"),
              by.y = c("FIPSStateCode", "FIPSCountyCode"),
              all.x = TRUE)
  
  co <- co[,c("fipsstate", "fipscounty", "fips", "statename", "countyname", "totalpop2010", "CBSACode")]
  
  co <- merge(co, cbsa, by = "CBSACode")
  
  # County concentration share
  co$CBSAhhico <- (co$totalpop2010 / co$CBSApop)^2
  
  # County consolidation index HHI by CBSA
  cbsa.hhico <- aggregate(co$CBSAhhico, by=list(co$CBSACode), FUN=sum, na.rm=FALSE)
  colnames(cbsa.hhico) <- c("CBSACode", "CBSAhhico")
  
  cbsa <- merge(cbsa, cbsa.hhico, by = "CBSACode", all.x = TRUE)
  
  #plot(log(cbsa$CBSApop), cbsa$CBSAhhico, pch = 20)
  
  ####### WRITE TABLE ############
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}
