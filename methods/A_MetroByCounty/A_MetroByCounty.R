A_MetroByCounty <- function() {
  
  # Import data
  db <- conma()
  co1950 <- dbReadTable(db, "A_SMA_ByCounty_1950")
  co1980 <- dbReadTable(db, "A_SMSA_ByCounty_1980")
  co2010 <- dbReadTable(db, "A_CBSA_ByCounty_2010")
  dbDisconnect(db)
  
  # Prepare columns for 1950
  
  # Remove duplicated counties (Yellowstone National Park)
  co1950 <- co1950[!duplicated(co1950$fips),]
  
  to.keep <- c("fipsstate",
               "fipscounty",
               "totalpop1950",
               "sma",
               "smaname",
               "smapop",
               "smahhi",
               "SMAhhico",
               "SMAhhipl",
               "SMAplcount",
               "SMAplby10000")
  
  co1950 <- co1950[,to.keep]
  colnames(co1950) <- c("fipsstate",
                        "fipscounty",
                        "countypop_1950",
                        "CBSAcode_1950",
                        "CBSAname_1950",
                        "CBSApop_1950",
                        "CBSAhhi_1950",
                        "CBSAhhico_1950",
                        "CBSAhhipl_1950",
                        "CBSAplcount_1950",
                        "CBSAplby10000_1950")
  
  # Change fips code for Dade county, who changed FIPS from 12025 to 12086 in 1997
  co1950[co1950$fipsstate == "12" & co1950$fipscounty == "025",]$fipscounty  <- "086"
  
  
  # Prepare columns for 1980
  to.keep <- c("fipsstate",
               "fipscounty",
               "totalpop1980",
               "smsa",
               "SMSAname",
               "SMSApop",
               "SMSAhhi",
               "SMSAhhico",
               "SMSAhhipl",
               "SMSAmpdi",
               "SMSAplcount",
               "SMSAplby10000",
               "COplcount",
               "COplby10000")
  
  co1980 <- co1980[,to.keep]
  colnames(co1980) <- c("fipsstate",
                        "fipscounty",
                        "countypop_1980",
                        "CBSAcode_1980",
                        "CBSAname_1980",
                        "CBSApop_1980",
                        "CBSAhhi_1980",
                        "CBSAhhico_1980",
                        "CBSAhhipl_1980",
                        "CBSAmpdi_1980",
                        "CBSAplcount_1980",
                        "CBSAplby10000_1980",
                        "COplcount_1980",
                        "COplby10000_1980")

  # Change fips code for Dade county, who changed FIPS from 12025 to 12086 in 1997
  co1980[co1980$fipsstate == "12" & co1980$fipscounty == "025",]$fipscounty  <- "086"
  
  # Prepare columns for 2010
  to.keep <- c("fipsstate",
               "fipscounty",
               "countyname",
               "statename",
               "totalpop2010",
               "CBSACode",
               "CBSATitle",
               "Type",
               "CBSApop",
               "CBSAhhi",
               "CBSAhhico",
               "CBSAhhipl",
               "CBSAmpdi",
               "CBSAplcount",
               "CBSAplby10000",
               "CBSApcpop",
               "CBSApcprop",
               "CBSAzb",
               "COplcount",
               "COplby10000")
  
  co2010 <- co2010[,to.keep]
  colnames(co2010) <- c("fipsstate",
                        "fipscounty",
                        "countyname",
                        "statename",
                        "countypop_2010",
                        "CBSAcode_2010",
                        "CBSAname_2010",
                        "CBSAtype_2010",
                        "CBSApop_2010",
                        "CBSAhhi_2010",
                        "CBSAhhico_2010",
                        "CBSAhhipl_2010",
                        "CBSAmpdi_2010",
                        "CBSAplcount_2010",
                        "CBSAplby10000_2010",
                        "CBSApcpop_2010",
                        "CBSApcprop_2010",
                        "CBSAzb_2010",
                        "COplcount_2010",
                        "COplby10000_2010")
  
  
  # Eliminate Micropolitan areas (10'000 to 49'999 inhabitants), 2010
  # Only Metropolitan areas (50'000+ inhabitants) are comparable with other decennials
  
  to.na <- c("CBSAcode_2010",
             "CBSAname_2010",
             "CBSApop_2010",
             "CBSAhhi_2010",
             "CBSAhhico_2010",
             "CBSAhhipl_2010",
             "CBSAmpdi_2010",
             "CBSAplcount_2010",
             "CBSAplby10000_2010",
             "CBSApcpop_2010",
             "CBSApcprop_2010",
             "CBSAzb_2010")
  
  for(v in to.na) {
    co2010[!is.na(co2010$CBSAtype_2010) & co2010$CBSAtype_2010 == 0, v] <- NA
  }
  
  co2010$CBSAtype_2010 <- NULL
  
  # Merge years
  co <- merge(co1950, co1980,
              by = c("fipsstate","fipscounty"),
              all = TRUE)
  co <- merge(co2010, co,
              by = c("fipsstate","fipscounty"),
              all = TRUE)
  
  co$fips <- paste(co$fipsstate, co$fipscounty, sep = "")
  
  # Binary variable if part of a metro area
  
  co$CBSApart_1950 <- as.integer(!is.na(co$CBSAcode_1950))
  co$CBSApart_1950[co$CBSApart_1950 == 0] <- NA
  
  co$CBSApart_1980 <- as.integer(!is.na(co$CBSAcode_1980))
  co$CBSApart_1980[co$CBSApart_1980 == 0] <- NA
  
  co$CBSApart_2010 <- as.integer(!is.na(co$CBSAcode_2010))
  co$CBSApart_2010[co$CBSApart_2010 == 0] <- NA
  
  # Compute delta variables
  co$Deltahhi30y_2010 <- co$CBSAhhi_2010 - co$CBSAhhi_1980
  co$Deltahhi30y_1980 <- co$CBSAhhi_1980 - co$CBSAhhi_1950
  
  co$Deltampdi30y_2010 <- co$CBSAmpdi_2010 - co$CBSAmpdi_1980
  
  co$DeltaCOplcount30y_2010   <- co$COplcount_2010 - co$COplcount_1980
  co$DeltaCOplby1000030y_2010 <- co$COplby10000_2010 - co$COplby10000_1980
  
  co$DeltaCBSAplcount30y_1980   <- co$CBSAplcount_1980 - co$CBSAplcount_1950
  co$DeltaCBSAplby1000030y_1980 <- co$CBSAplby10000_1980 - co$CBSAplby10000_1950
  co$DeltaCBSAplcount30y_2010   <- co$CBSAplcount_2010 - co$CBSAplcount_1980
  co$DeltaCBSAplby1000030y_2010 <- co$CBSAplby10000_2010 - co$CBSAplby10000_1980
  
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_MetroByCounty", value=co, overwrite=TRUE)
  dbDisconnect(db)
}