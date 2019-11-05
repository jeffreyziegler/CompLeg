# load libraries
library("XML"); library(jsonlite)

filenames <- gsub("\\.txt$","", list.files(pattern="\\.txt$"))
# iterate over those file names and read each .csv
for(i in filenames){
  # Tip: you almost always want to read stringsAsFactors=F in .csv
  # because you want to often text as text, not factors
  assign(i, fromJSON(paste(i, ".txt", sep=""), stringsAsFactors = F, encoding = "UTF-8"))
}

outputData <- fromJSON("output.txt")

# look at the call
# outputData$head

# look at the results of the json curl
results <- outputData$results

# see debates
results$bill$debates[[1]] # Dáil
results$bill$debates[[2]] # Seanad
