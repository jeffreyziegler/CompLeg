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

#########################################
# read in data useful
# download html files through iteration
#########################################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("canada_committees_date_change.csv")

###################################
# read in useful data in iteration
# download html files
###################################
# change the date to the one in url 
df$date_change <- gsub(",","",df$date_change)
df$dc <- as.Date(df$date_change, format="%B %d %Y")
x <- nrow(df)

# download through iteration
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Associate Membership')
dc <- df$dc
acronym <- df$committee_acronym
pn <- df$parliament_number
sn <- df$session_number
number <- paste("/Members?membershipon=",dc, "&parl=", pn,"&session=",sn,"&includeAssociates=True#AssociateMembers", sep="")

for(i in 1:x){
  url <- paste("https://www.ourcommons.ca/Committees/en/", acronym[i], number[i], sep="")
  file <- paste(acronym[i], "_", pn[i], "_", sn[i], "_", dc[i], ".html", sep="")
  # download.file(url, file, quiet = TRUE)
}
url

#######################
# parse HTML function
#######################
parse_HTML <- function(file) {
  # get some information from the name of the file
  info <- str_split(file, "\\/|_|\\.")
  info <- unlist(info)
  
  # read html
  html <- read_html(file)
  
  # get names of the associate members
  name <- html %>%
    html_nodes(".name") %>% 
    html_text()
  
  # check 
  if (is_empty(name)) {
    return(NULL)
  }
  
  # return the dataframe
  out <- data.frame(committee_acronym = info[3],
                    parliament_number = info[4],
                    session_number = info[5],
                    date_change = info[6],
                    position = "Associate Member",
                    full_name = name)
  return(out)
}

#########################################
# read in data
# create raw associate membership dataset
##########################################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Associate Membership')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)
raw_canada_committee_associate_membership <- df

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(raw_canada_committee_associate_membership, "raw_canada_committee_associate_membership.csv")

