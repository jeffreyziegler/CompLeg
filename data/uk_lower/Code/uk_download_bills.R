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

# set working directory
setwd("~/Documents/GitHub/CompLegFall2019/data/uk_lower/Bills")

# initialize results with 1st page
result <- read.csv("bills_page_0.csv")
# set counter to empty
result$identifier <- NULL
# loop over pages to load into 
for(i in 1:4){
  file <- str_c("bills_page_", i,".csv", collpase = "")
  interm <- read.csv(file)
  tryCatch({interm$identifier <- NULL}, warnings =function(e) print(paste(file, 'no identifier')))
  result <- rbind(result,interm)
}

# Rename the column 
result <- result %>% select(date, bill.type, title, sponsors...sponsor.printed, session...display.name, home.page...uri)
# Rename the column 
result <- dplyr::rename(result, date_introduced = date, bill_type = bill.type, bill_title = title, member_name = sponsors...sponsor.printed, session = session...display.name, url = home.page...uri )

# add variable for session, House, and parliament
result$session <- as.character(result$session)
result$parliament <- NA
result[result$session == "2005-2006", ]$parliament <- 54
result[result$session == "2006-2007", ]$parliament <- 54
result[result$session == "2007-2008", ]$parliament <- 54
result[result$session == "2008-2009", ]$parliament <- 54
result[result$session == "2009-2010", ]$parliament <- 55
result[result$session == "2010-2012", ]$parliament <- 55
result[result$session == "2012-2013", ]$parliament <- 55
result[result$session == "2013-2014", ]$parliament <- 55
result[result$session == "2014-2015", ]$parliament <- 56
result[result$session == "2015-2016", ]$parliament <- 56
result[result$session == "2016-2017", ]$parliament <- 56
result[result$session == "2017-2019", ]$parliament <- 57
result[result$session == "", ]$parliament <- NA

result$session <-as.factor(result$session)

levels(result$session)

result$chamber_number <- NA

result$chamber_number[str_detect(result$bill_title, "HL")] <- 2
result$chamber_number[is.na(result$chamber_number)] <- 1

# create urls to download html from each page
result$url <- as.character(result$url)
result$session <- as.character(result$session)
result$bill_title <- as.character(result$bill_title)
result$shortURL <- gsub(".*/","", result$url)
result$shortSession <- paste("20", gsub("20","", result$session), sep="")
result$newURL <- paste("https://services.parliament.uk/Bills/", result$shortSession, "/", result$shortURL, sep="")

# remove all rows that don't have bill title
result <- result[which(result$bill_title!=""), ]

# find only unique cases and download html
uniqueResults <- result %>% distinct(bill_title, .keep_all = T)


# loop over pages and download
for(i in 1:nrow(uniqueResults)) {
  tryCatch({
    download_html(uniqueResults[i,"newURL"], file = paste("htmls/", gsub(" ","_", gsub("[^[:alnum:] ]", "", uniqueResults[i,"bill_title"])), ".html", sep=""))
    print(i)
  }, warnings =function(e) print(paste(file, 'no URL')))
  
}