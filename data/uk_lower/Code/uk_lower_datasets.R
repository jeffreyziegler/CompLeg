# these datasets come from http://www.data.parliament.uk/dataset
# they include:
# (1) Bills
# (2) Constituencies
# (3) Divisions
# (4) Written Answers
# (5) Oral Questions
# (6) Written Questions
# (7) Written Statements

#####################
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "lubridate"), pkgTest)

# working directoy
setwd("~/Documents/GitHub/CompLegFall2019/data/uk_lower/")

########################
# download all XML files
########################

# create function that takes inputs of:
# (1) type: what data base do you want to access (bills, divisions, etc.)?
# (2) maxPages: this is the number of web pages the documents are stored across
# the API only lets you download 500 at a time, so you have to iterate over pages
# (3) fileName: where is the destination of the file? 
# Our labelling is slightly different than the website (ex: "answeredquestions" & "Written_Responses")
download_xml <- function(type, maxPages, fileName){
  for(i in 0:maxPages) {
    # make URL
    url <- str_c("http://lda.data.parliament.uk/", type,".csv?_pageSize=500&_page=", i, collapse = "")
    # make file name
    file <- str_c(getwd(), "/", fileName, "/", type, "_page_", i, ".xml", collapse = "")
    # download file
    tryCatch(download.file(url, file, quiet = TRUE), 
             error = function(e) print(paste(file, 'questions missing')))
    
    # random delay
    Sys.sleep(runif(1, 0, 0.15))
  }  
}

# Here's all the info you need to downlaod that dataset(s):

# "bills", 7, "Bills"
# "answeredquestions", 509, "Written_Responses"
# "constituencies", 7, "Constituencies"
# "commonsdivisions", 8, "Divisions"
# "commonswrittenquestions", 510, "Written_Questions"

# You just pick which dataset(s) you want and insert that info below in the object datasets
# Ex: I just want to get the bills
# datasets <- data.frame(type=c("bills"), maxPages=c(7), fileName=c("Bills"))
# Now, let's say I want to get bills and constituencies
# datasets <- data.frame(type=c("bills", "constituencies"), maxPages=c(7, 7), fileName=c("Bills", "Constituencies"))
# to get all:
# datasets <- data.frame(type=c("bills", "constituencies", "commonsdivisions", "answeredquestions", "commonswrittenquestions"), 
#                       maxPages=c(7, 7, 8, 509, 510), 
#                       fileName=c("Bills", "Constituencies", "Divisions", "Written_Responses", "Written_Questions"))

#datasets <- data.frame(type=c(), maxPages=c(), fileName=c())

# Last, execute function for each dataset you download
for(set in 1:length(unique(datasets$type))){
  download_xml(type = datasets[set, "type"], maxPages = datasets[set, "maxPages"],
               fileName = datasets[set, "fileName"])
}
