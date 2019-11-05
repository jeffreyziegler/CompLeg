# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("jsonlite", "curl"), pkgTest)

# set working directory
# should be common across users if you create 
# root like this on your GitHub
setwd('~/Documents/GitHub/CompLegFall2019/data/ireland_lower')


req <- curl_fetch_memory("https://api.oireachtas.ie/v1/debates?chamber_type=house&chamber_id=&date_start=1919-01-01&date_end=2020-01-01&limit=10000")

filenames <- gsub("\\.txt$","", list.files(pattern="\\.txt$"))
# iterate over those file names and read each .csv
for(i in filenames[-1]){
  # Tip: you almost always want to read stringsAsFactors=F in .csv
  # because you want to often text as text, not factors
  assign(i, fromJSON(paste(i, ".txt", sep="")))
}



# look at the results of the json curl

## remain to be fixed:
# debates_results <- output_debates$results
# oralquestion_results <- output_oralquestions$results
# writtenquestion_results <- output_oralquestions$results

bills_results <- output_bills$results
dailconstituencies_results <- output_dailconstitu$results
seanadconstituencies_results <- output_seanadconstitu$results
party_results <- output_party$results
votes69_results <- output_votes$results
votes09_results <- output_votes2$results
votes19_results <- output_votes3$results
member_results <- output_members$results

oralquestion_results1 <- output_oralquestions1$results

# stages of the bill through both chambers
bills_results$bill$events

# see debates associated w/ bills
bills_results$bill$debates[[1]] # Dáil
bills_results$bill$debates[[2]] # Seanad

# see all parts of debate
debates_results$debateRecord$debateSections

# can retrieve all of these texts either in .pdf 
# .xml also seems to return content if you check in browser
debates_results$debateRecord$formats$pdf
debates_results$debateRecord$formats$xml

dailconstituencies_results$house$constituenciesOrPanels$constituencyOrPanel$showAs
party_results$house$parties$party$showAs
votes09_results$division$debate$formats$pdf

