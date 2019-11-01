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


########################
# set working directory 
# read in useful data
########################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("canada_non_standing_committee_date_change.csv")
name <- read.csv("canada_non_standing_committees.csv")


###################################
# read in useful data in iteration
# download files
###################################
# change the date to the one in url 
df$date_change <- gsub(",","",df$date_change)
df$dc <- as.Date(df$date_change, format="%B %d %Y")
colnames(df)
df_a <- merge(x=df, y=name, by="committee_acronym") %>%
  select(committee_acronym,
         parliament_number,
         session_number,
         dc,
         committee_type)
df_aa <- df_a %>%
  filter(committee_type != "Joint Committees")
df_ab <- df_a %>%
  filter(committee_type == "Joint Committees")

# download non joint committee membership through iteration
xa <- nrow(df_aa)
dc <- df_aa$dc
acronym <- df_aa$committee_acronym
pn <- df_aa$parliament_number
sn <- df_aa$session_number
number <- paste("/Members?includeAssociates=True&parl=",pn,"&session=",sn,"&membershipOn=",sep="")

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/NonStanding Committes/Membership_a')
for(i in 1:xa){
  url <- paste("https://www.ourcommons.ca/Committees/en/", acronym[i], number[i], dc[i], "#committeeMembersPanel", sep="")
  file <- paste(acronym[i], "_", pn[i], "_", sn[i], "_", dc[i], ".html", sep="")
  # download.file(url, file, quiet = TRUE)
}

# download joint committee membership through iteration
xb <- nrow(df_ab)
dc_j <- df_ab$dc
acronym_j <- df_ab$committee_acronym
pn_j <- df_ab$parliament_number
sn_j <- df_ab$session_number
number_j <- paste("/Members?includeassociates=True&parl=",pn_j,"&session=",sn_j,"&membershipOn=",dc_j,"#committeeMembersPanel", sep="")

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/NonStanding Committes/Membership_ja')
for(i in 1:xb){
  url <- paste("https://www.parl.ca/Committees/en/", acronym_j[i], number_j[i], sep="")
  file <- paste(acronym_j[i], "_", pn_j[i], "_", sn_j[i], "_", dc_j[i], ".html", sep="")
  # download.file(url, file, quiet = TRUE)
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

#######################
# parse HTML_j function
#######################
parse_HTML_j <- function(file) {
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

########################################
# read in html files and make a raw csv 
########################################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/NonStanding Committes/Membership_a')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/NonStanding Committes/Membership_ja')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out_j <- alply(.data = files, .margins = 1, .fun = parse_HTML_j, .progress = "text", .inform = TRUE)
# stack data frames
df_j <- do.call("rbind", out_j)

# bind two dataset
raw_canada_non_standing_committee_associate_membership <- rbind(df,df_j)

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(raw_canada_non_standing_committee_associate_membership, "raw_canada_non_standing_committee_associate_membership.csv")
