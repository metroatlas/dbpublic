# Compute the CBSA table from census data
# Core Based Statistical Area
# Create table of CBSA values by county (data used to create maps)
# Export relevant csv files

A_CBSA_2010 <- function() {
  A_CBSA_HHI_2010()
  A_CBSA_HHI_Counties_2010()
  A_CBSA_HHI_Places_2010()
  A_CBSA_ByCounties_2010()
  A_CBSA_2010_Export()
  A_CBSA_ByCounty_2010_Export()
}