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
               "smahhi")
  
  co1950 <- co1950[,to.keep]
  colnames(co1950) <- c("fipsstate",
                        "fipscounty",
                        "countypop_1950",
                        "CBSAcode_1950",
                        "CBSAname_1950",
                        "CBSApop_1950",
                        "CBSAhhi_1950")
  
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
               "SMSAplcount",
               "SMSAplby10000")
  
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
                        "CBSAplcount_1980",
                        "CBSAplby10000_1980")

  # Change fips code for Dade county, who changed FIPS from 12025 to 12086 in 1997
  co1980[co1980$fipsstate == "12" & co1980$fipscounty == "025",]$fipscounty  <- "086"
  
  # Prepare columns for 2010
  to.keep <- c("fipsstate",
               "fipscounty",
               "countyname",
               "totalpop2010",
               "CBSACode",
               "CBSATitle",
               "CBSApop",
               "CBSAhhi",
               "CBSAhhico",
               "CBSAhhipl",
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
                        "countypop_2010",
                        "CBSAcode_2010",
                        "CBSAname_2010",
                        "CBSApop_2010",
                        "CBSAhhi_2010",
                        "CBSAhhico_2010",
                        "CBSAhhipl_2010",
                        "CBSAplcount_2010",
                        "CBSAplby10000_2010",
                        "CBSApcpop_2010",
                        "CBSApcprop_2010",
                        "CBSAzb_2010",
                        "COplcount_2010",
                        "COplby10000_2010")
  
  # Merge years
  co <- merge(co1950, co1980,
              by = c("fipsstate","fipscounty"),
              all = TRUE)
  co <- merge(co2010, co,
              by = c("fipsstate","fipscounty"),
              all = TRUE)
  
  co$fips <- paste(co$fipsstate, co$fipscounty, sep = "")
  
  # Compute delta variables
  co$Deltahhi30y_2010 <- co$CBSAhhi_2010 - co$CBSAhhi_1980
  co$Deltahhi30y_1980 <- co$CBSAhhi_1980 - co$CBSAhhi_1950
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_MetroByCounty", value=co, overwrite=TRUE)
  dbDisconnect(db)
  
}