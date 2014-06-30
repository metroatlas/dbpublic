C_StateCodes <- function(d) {
  Sys.setlocale("LC_ALL", 'en_US.UTF-8')
  
  # Download data
  if(d){
    fileUrl <- "http://www.census.gov/geo/reference/docs/state.txt"
    download.file(fileUrl, destfile="data/C_StateCodes.txt", method="curl")
    dateDownloaded <- date()
    write(dateDownloaded,file="data/C_StateCodes.txt.date.txt")
  }
  
  # Load data
  states <- read.csv("data/C_StateCodes.txt",
                        sep="|", header = TRUE,
                        colClasses="character",
                        fileEncoding="UTF-8")
  
  colnames(states) <-  tolower(names(states))
  colnames(states)[3] <-  "statename"
  colnames(states)[1] <-  "statefp"
  
  # Write table
  db <- conma()  
  dbWriteTable(db, name="C_StateCodes", value=states, overwrite=TRUE)
  dbDisconnect(db)
}