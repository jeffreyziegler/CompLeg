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

# Set Working Directory
setwd("~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Questions/toc")

# parameters
parliament <- 42
session <- 1
sittings <- 438

#####################
# download html files
#####################

source("canada_toc_download.R")

##################
# parse HTML files
##################

source("canada_questions_download.R")

#####################
# read in member data
#####################

member <- read.csv("../../Members (CSV)/canada_members_updated.csv",encoding = "UTF-8", stringsAsFactors = F) %>%
  select("full_name", "last_name", "member_number")
colnames(member)[2] <- "member_last"

# merge member full name and member ID
x <- merge(questions, member, by = "member_last", all = T)
merged <- x[!is.na(x$key_ID),] %>%
  arrange(key_ID)
merged$member_last <- NULL

###########
# write csv
###########

write.csv(merged, "canada_questions_updated.csv")