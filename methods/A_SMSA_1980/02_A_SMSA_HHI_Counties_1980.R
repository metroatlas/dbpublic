# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# for the 1980 census, for Counties only.

A_SMSA_HHI_Counties_2010 <- function() {
  
  # Import data
  db <- conma()
  counties  <- dbReadTable(db, "C_SF1_Counties_1980")
  delin     <- dbReadTable(db, "C_MetroDelineations_1981")
  smsa      <- dbReadTable(db, "A_SMSA_1980")
  dbDisconnect(db)
  
  # Delete the column if it alread exists
  if("SMSAhhico" %in% names(smsa)) {
    smsa$SMSAhhico  <- NULL
  }
  
  # Get rid of Hawaii and Puerto Rico
  counties <- counties[counties$fipsstate != "15" & counties$fipsstate != "72", ]
  
  # Counties with SMSA and population
  delin$countyfips <- mapply(paste0, delin$statefp, delin$countyfp, MoreArgs = list(collapse = ""))
  co <- counties[,c("fipsstate", "fipscounty", "fips", "STATE", "COUNTY", "totalpop1980")]
  colnames(co) <- c("fipsstate", "fipscounty", "fips", "statename", "countyname", "totalpop1980")
  co <- merge(co, delin[,c(1,7)], by.x = "fips", by.y = "countyfips")
  
  
  # Consolidation share of counties
  co <- merge(co, smsa,
              by="smsa",
              all = FALSE)
  
  co$HHI.smsa <- (co$totalpop1980 / co$SMSApop)^2
  
  # Consolidation index HHI by SMSA
  smsa.hhico <- aggregate(co$HHI.smsa, by=list(co$smsa), FUN=sum, na.rm=FALSE)
  names(smsa.hhico)  <- c("smsa","SMSAhhico")

  smsa <- merge(smsa, smsa.hhico,
                by = "smsa")
  
  #smsa[smsa$SMSAhhico == 1, ]
  #plot(log(smsa$SMSApop), smsa$SMSAhhico, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_SMSA_1980", value=smsa, overwrite=TRUE)
  dbDisconnect(db)
}