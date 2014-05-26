# This R script builds the database for the Metropolitan Atlas using:
# - census.gov API for 1990-2010 census
# - NHGIS data tables for 1950-1980 census
# - EPFL digitalized files for population by place 1950-1960

# Source all the R files in subdirectories
source("sourceAll.R")
sourceAll()

source("conma.R")
db <- conma()

# Import US Census 2010
C_PlaceList_2010()