############################
# clean environment
# load packages
############################
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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "XML", "tidyr", "lubridate", "readxl"), pkgTest)

############################
# set wd, read in pre-info
############################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/output')
affair <- read.csv("switzerland_affair_d.csv")

##########################
# download html
##########################
x <- nrow(affair)
affair$affair_id <- as.character(affair$affair_id)
affairId <- affair$affair_id
error <- affair$affair_id

for (i in 1:x){
  if (nchar(affair$affair_id[i])==4){
    affairId[i] <- paste(20, substr(affair$affair_id[i], 1, 2), 000, substr(affair$affair_id[i], 4, 4), sep="")
  } else if (nchar(affair$affair_id[i])==5){
    affairId[i] <- paste(20, substr(affair$affair_id[i], 1, 2), 00, substr(affair$affair_id[i], 4, 5), sep="")
  } else if (nchar(affair$affair_id[i])==6){
    affairId[i] <- paste(20,substr(affair$affair_id[i], 1, 2), 0, substr(affair$affair_id[i], 4, 6), sep="")
  } else if (nchar(affair$affair_id[i])==7){
    affairId[i] <- paste(20,substr(affair$affair_id[i], 1, 2), substr(affair$affair_id[i], 4, 7), sep="")
  } else {
    error[i] <- NA
  }
}

## note that observation 4416 is "Bundesbeschluss über die Genehmigung und die Umsetzung des Übereinkommens des Europarats und der OECD über die gegenseitige Amtshilfe in Steuersachen",
## we remove it from the dataset
affairId_list <- affairId[-c(which(is.na(error)))] %>%
  unique()

setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/affair_text')
for(i in 2014:x){
  url <- paste("https://www.parlament.ch/de/ratsbetrieb/suche-curia-vista/geschaeft?AffairId=", affairId_list[i], sep="")
  file <- paste(affairId_list[i], "_d.html", sep="")
  download.file(url, file, quiet = TRUE)
  Sys.sleep(runif(1, 0, 0.25))
}

