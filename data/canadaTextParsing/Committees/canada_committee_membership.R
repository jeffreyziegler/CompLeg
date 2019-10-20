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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo"), pkgTest)

########################
# read in data useful
########################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("canada_committees_date_change.csv")

###################################
# read in useful data in iteration
###################################
# change the date to the one in url 
df$date_change <- gsub(",","",df$date_change)
df$dc <- as.Date(df$date_change, format="%B %d %Y")
x <- nrow(df)

# download through iteration
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Membership')
dc <- df$dc
acronym <- df$committee_acronym
pn <- df$parliament_number
sn <- df$session_number
number <- paste("/Members?parl=",pn,"&session=",sn,"&membershipOn=", sep="")

for(i in 1:x){
  url <- paste("https://www.ourcommons.ca/Committees/en/", acronym[i], number[i], "#committeeMembersPanel", sep="")
  file <- paste(acronym[i], "_", pn[i], "_", sn[i], "_", dc[i], ".html", sep="")
  download.file(url, file, quiet = TRUE)
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
  
  # full_name, last_name, first_name, and title
  raw <- html %>%
    html_nodes(".title,.first-name") %>% 
    html_text()
  
  x <- which(raw=="Chair")
  y <- which(raw=="Vice-Chairs")
  z <- which(raw=="Members")
  k <- length(raw)
  
  title <- "Initiate"
  title[c(x:(y-1))] <- "Chair"
  title[c(y:(z-1))] <- "Co-chair"
  title[c(z:k)] <- "Member"
  
  # title <- "Initiate"
  # if(x!=(y-1)) {
  #   title[c(x:(y-1))] <- "Chair"
  # }
  # if(y!=(z-1)){
  #   title[c(y:(z-1))] <- "Co-chair"
  # }
  # if(z!=k){
  #   title[c(z:k)] <- "Member"
  # }

  first_name <- raw[-c(x,y,z)][c(TRUE,FALSE)]
  title <- title[-c(x,y,z)][c(TRUE,FALSE)]
  
  last_name <- html %>%
    html_nodes(".last-name") %>% 
    html_text()
  last_name <- last_name[c(TRUE,FALSE)]
  
  full_name <- paste(first_name, last_name, sep=" ") %>%
    unique()
  
  # check 
  if(length(full_name) == 0) {
    return(NULL)
  }
  
  # return the dataframe
  out <- data.frame(committee_acronym = info[3],
                    parliament_number = info[4],
                    session_number = info[5],
                    date_change = info[6],
                    title = title,
                    first_name = first_name,
                    last_name = last_name,
                    full_name = full_name)
  return(out)

}

#####################
# read in data
#####################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Membership')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)

###########
# clean 
###########s
  


