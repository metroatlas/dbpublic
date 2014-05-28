# This R script builds the database for the Metropolitan Atlas using:
# - census.gov America FactFinder for 1990-2010 census
# - NHGIS data tables for 1950-1980 census
# - EPFL digitalized files for population by place 1950-1960

# WORKING DIRECTORY
# Make sure the working directory is set to the root of the repository,
# where this file is, to execute it.

# Source all the R files in subdirectories
source("sourceAll.R")
sourceAll()

source("conma.R")

# Import US Census 2010
C_SF1_Counties_2010()
C_SF1_Places_2010()
C_PlaceList_2010()
C_MetroDelineations_2013()
