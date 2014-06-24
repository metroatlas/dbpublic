# This R script builds the database for the Metropolitan Atlas using:
# - census.gov America FactFinder for 1990-2010 census
# - NHGIS data tables for 1950-1980 census
# - EPFL digitalized files for population by place 1950-1960

# WORKING DIRECTORY
# Make sure the working directory is set to the root of the repository,
# where this file is, to execute it.

# Source all the R files in subdirectories
source("conma.R")
source("sourceAll.R")
sourceAll()

# Set TRUE if you want to redownload the data
# This can be set for each function
d  <-  FALSE

# Vintage Census Codes for places
C_PlacesVintageCodes(d)

# FIPS codes and abbreviations for states
C_StateCodes(d)

# Import US Census 1950
C_SF1_Counties_1950(d)
C_SF1_Places_1950(d)

# Import US Census 1980
C_SF1_Counties_1980(d)
C_SF1_Places_1980(d)
C_PlaceList_1980(d)
C_MetroNames_1981(d)
C_MetroDelineations_1981(d)

# Import US Census 2000
C_SF1_Counties_2000(d)
C_SF1_Places_2000(d)
C_MetroDelineations_2003(d)

# Import US Census 2010
C_SF1_Counties_2010(d)
C_SF1_Places_2010(d)
C_PlaceList_2010(d)
C_MetroDelineations_2013(d)
C_CBSA_PrincipalCities_2013(d)

# MDPI
R_CBSA_MDPI_2007(d)

# Compute data on CBSA (Core-Based Statistical Areas) 2010
A_SMSA_1980()
A_CBSA_2010()
