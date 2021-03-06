# Computes the generalized Herfindahl-Hirschmann index of
# concentration of political power in US metropolitan areas
# and the Metropolitan Power Diffusion Index
# for the 2010 census.

A_CBSA_HHI_2010 <- function() {

  # Import data
  db <- conma()
  places    <- dbReadTable(db, "C_SF1_Places_2010")
  counties  <- dbReadTable(db, "C_SF1_Counties_2010")
  delin     <- dbReadTable(db, "C_MetroDelineations_201302")
  placelist <- dbReadTable(db, "C_PlaceList_2010")
  dbDisconnect(db)
  
  # Get rid of Hawaii and Puerto Rico
  places <- places[places$state != "15" & places$state != "72", ]
  counties <- counties[counties$state != "15" & counties$state != "72", ]
  placelist <- placelist[placelist$statefp != "15" & placelist$statefp != "72", ]
  delin <- delin[delin$FIPSStateCode != "15" & delin$FIPSStateCode != "72", ]
  
  placelist$fips <- paste(placelist$statefp, placelist$placefp, sep="")
  
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
  
  # Counties with CBSA and population
  co <- merge(counties, delin,
              by.x = c("fipsstate", "fipscounty"),
              by.y = c("FIPSStateCode", "FIPSCountyCode"),
              all.x = TRUE)
  
  co <- co[,c("fipsstate", "fipscounty", "fips", "statename", "countyname", "totalpop2010", "CBSACode")]
  
  # Aggregated population by CBSA
  plco <- rbind(pl[,c("CBSACode", "totalpop2010")], co[,c("CBSACode", "totalpop2010")])
  cbsa.pop.aggr <- aggregate(plco$totalpop2010, by=list(plco$CBSACode), FUN=sum, na.rm=FALSE)
  names(cbsa.pop.aggr)  <- c("CBSACode","CBSApop.aggr")
  
  # Share of counties
  co <- merge(co, cbsa.pop.aggr,
              by="CBSACode",
              all = FALSE)
  
  co$HHI.cbsa <- (co$totalpop2010 / co$CBSApop.aggr)^2
  co$MPDI.cbsa <- sqrt((co$totalpop2010 / co$CBSApop.aggr))
  
  #Share of places
  pl <- merge(pl, cbsa.pop.aggr,
              by="CBSACode",
              all = FALSE)
  
  pl$HHI.cbsa <- (pl$totalpop2010 / pl$CBSApop.aggr)^2
  pl$MPDI.cbsa <- sqrt((pl$totalpop2010 / pl$CBSApop.aggr))
  
  # Sums of squared shares
  plco.hhi  <- rbind(pl[,c("CBSACode", "HHI.cbsa")], co[,c("CBSACode", "HHI.cbsa")])
  plco.mpdi <- rbind(pl[,c("CBSACode", "MPDI.cbsa")], co[,c("CBSACode", "MPDI.cbsa")])
  
  # Consolidation index HHI by CBSA
  cbsa.hhi <- aggregate(plco.hhi$HHI.cbsa, by=list(plco.hhi$CBSACode), FUN=sum, na.rm=FALSE)
  names(cbsa.hhi)  <- c("CBSACode","CBSAhhi")
  
  # Consolidation index MPDI by CBSA
  cbsa.mpdi <- aggregate(plco.mpdi$MPDI.cbsa, by=list(plco.mpdi$CBSACode), FUN=sum, na.rm=FALSE)
  names(cbsa.mpdi)  <- c("CBSACode","CBSAmpdi")
  
  # Total population of CBSA
  cbsa.pop <- aggregate(co$totalpop2010, by=list(co$CBSACode), FUN=sum, na.rm=FALSE)
  names(cbsa.pop)  <- c("CBSACode","CBSApop")
  
  cbsa <- merge(cbsa.pop, cbsa.hhi, by = "CBSACode")
  cbsa <- merge(cbsa, cbsa.mpdi, by = "CBSACode")
  cbsa <- merge(cbsa, delin[,c("CBSACode", "CBSATitle", "Type")], by = "CBSACode")
  cbsa <- cbsa[!duplicated(cbsa$CBSACode), ]
  cbsa$Type <- as.integer(cbsa$Type)
  
  #cbsa[cbsa$CBSAhhi == 1, ]
  #plot(log(cbsa$CBSApop), cbsa$CBSAhhi, pch = 20)
  
  # Write table
  db <- conma()
  dbWriteTable(db, name="A_CBSA_2010", value=cbsa, overwrite=TRUE)
  dbDisconnect(db)
}