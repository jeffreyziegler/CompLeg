# these datasets come from http://data.parliament.uk/membersdataplatform/memberquery.aspx
# they include:
# (1) Member name and general info
# (2) Parties they've been apart of
# (3) Government posts they've held
# (4) Opposition posts they've held
# (5) Parliamentary posts they've held
# (6) Committees they've participated on
# (7) Constituencies they've served

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

####################
# Download XML files
####################

# need to iterate over different criteria to download
# first, select that characteristics we're interested in
MPcharacteristics <- c("Committees", "OppositionPosts", "GovernmentPosts", "Constituencies",
                       "Parties", "ParliamentaryPosts"
                       #, "Experiences", "Interests", "Staff"
                       )

# indicate the base URL to include all members from Commons
baseURL <- "http://data.parliament.uk/membersdataplatform/services/mnis/members/query/House=Commons|Membership=all/"

# iterate over 
for(i in 1:length(MPcharacteristics)){
  # set url
  url <- str_c(baseURL, MPcharacteristics[i], collapse = "")
  # make file name
  file <- str_c(getwd(), "/raw_characteristics/", MPcharacteristics[i], "_raw_characteristics.xml", collapse = "")
  # download xml using url
  tryCatch(download.file(url, file, quiet = TRUE), error = function(e) print(paste(file, 'content missing')))
  # random delay between downloading iterations
  Sys.sleep(runif(1, 0, 0.15))
}