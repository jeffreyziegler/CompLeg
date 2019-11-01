#######################
# load libraries
#######################
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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "readxl"), pkgTest)

###################################
# read in useful data in iteration
###################################
# set working directory and read in data
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
raw_canada_committees <- read_excel("raw_canada_committees.xlsx")
canada_non_standing_committees <- read.csv("canada_non_standing_committees.csv")

# number of non-joint committees
acronym <- canada_non_standing_committees$committee_acronym[canada_non_standing_committees$committee_type != "Joint Committees"]
x <- length(acronym)
# number of joint committees
acronym_j <- canada_non_standing_committees$committee_acronym[canada_non_standing_committees$committee_type == "Joint Committees"]
z <- length(acronym_j)

# number of sessions
parliament_number <- raw_canada_committees$parliament_number
session_number <- raw_canada_committees$session_number
number <- paste(parliament_number, session_number, sep="_") %>%
  unique()
pn <- substr(number,1,2)
sn <- substr(number,4,4)
n <- paste("/Members?parl=",parliament_number,"&session=",session_number,sep="") %>%
  unique()
y <- length(number)

# download through iteration
# for each committee, iteration through sessions
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/NonStanding Committes/DateChange')
for(i in 1:x){
  for (j in 1:y){
    url <- paste("https://www.ourcommons.ca/Committees/en/",acronym[i], n[j], sep="")
    file <- paste(acronym[i], "_", pn[j], "_", sn[j], ".html", sep="")
    # download.file(url, file, quiet = TRUE)
  }
}
for(i in 1:z){
  for (j in 1:y){
    url <- paste("https://www.parl.ca/Committees/en/",acronym_j[i], n[j], sep="")
    file <- paste(acronym_j[i], "_", pn[j], "_", sn[j], ".html", sep="")
    # download.file(url, file, quiet = TRUE)
  }
}



#######################
# parse HTML function
#######################
parse_HTML <- function(file) {
  # get some information from the name of the file
  info <- str_split(file, "\\/|_|\\.")
  info <- unlist(info)
  
  # read html
  html <- read_html(file)
  
  # date change
  dc <- html %>%
    html_nodes("option") %>% 
    html_text()
  date_change <- str_extract(dc, "[A-Z][a-z]+ [0-9]{1,2}, [0-9]{4}") %>%
    na.omit()
  
  # check 
  if(length(date_change) == 0) {
    return(NULL)
  }
  
  # return the dataframe
  out <- data.frame(committee_acronym = info[3],
                    parliament_number = info[4],
                    session_number = info[5],
                    date_change = date_change)
  return(out)
}

#####################
# read in data
#####################

# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)

####################
# raw cleaning
####################
rownames(df) <- c()
canada_non_standing_committee_date_change <- df

###############
# write csv
###############
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
write.csv(canada_non_standing_committee_date_change, "canada_non_standing_committee_date_change.csv")
