# Compute the CBSA table from census data
# Core Based Statistical Area
# Export relevant csv files
A_CBSA_2010 <- function() {
  A_CBSA_HHI_2010()
  A_CBSA_ByCounties_2010()
}