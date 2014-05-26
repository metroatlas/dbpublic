dbpublic
========

Public database and data import methods for the metropolitan atlas.

# How it works
Open and execute makedb.R in R to compute the metropolitan atlas database.
The latest computed version if the database is metroatlas.db, in sqlite format.

# Behind the scene
Methods to gather and process data are contained in R scripts in the directory methods.
Data downloaded by these methods or downloaded manually (when they are not available to a script), are stored in the data folder.

DataSources.md contains all sources for datasets that are not available for a script to download.