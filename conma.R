# Get the connector for the metroplitan atlas database
conma <- function() {
  library(RSQLite)
  db <- dbConnect(SQLite(), dbname="metroatlas.db")
  return(db)
}