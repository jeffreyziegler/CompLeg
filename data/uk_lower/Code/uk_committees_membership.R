############################
# detach and load packages
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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "XML", "tidyr"), pkgTest)

##########################
# set working directory 
# import dataset
# create raw dataset
##########################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/uk_lower/raw_characteristics/')

# read in dataset
files <- list.files(pattern="*.xml", full.names=TRUE, recursive=FALSE)

# to dataframe
committees <- xmlToList(xmlParse(files[1]))
# constituencies <- xmlToDataFrame(files[2])
# experiences <- xmlToDataFrame(files[3])
# governmentPosts <- xmlToDataFrame(files[4])
# interests <- xmlToDataFrame(files[5])
# oppositionPosts <- xmlToDataFrame(files[6])
# parliamentaryPosts <- xmlToDataFrame(files[7])
# parties <- xmlToDataFrame(files[8]) 
# staff <- xmlToDataFrame(files[9]) 

##################################
# make the dataframe that we want 
##################################
 nullToNA <- function(x) {
   if(is.null(x)){
     x <- NA
   } else {
     x <- x
   }
 }

 for (i in 1:length(committees)) {
  ListAs <- committees[[i]]$ListAs
  LayingMinisterName <- committees[[i]]$LayingMinisterName
  House <- committees[[i]]$House
  member_id <- committees[[i]][[".attrs"]]["Member_Id"]
  ListAs <- nullToNA(ListAs)
  LayingMinisterName <- nullToNA(LayingMinisterName)
  House <- nullToNA(House)
  member_id <- nullToNA(member_id)
  for (j in 1:length(committees[[i]]$Committees)) {
    committee_name <- committees[[i]]$Committees[[j]]$Name
    start_date <- committees[[i]]$Committees[[j]]$StartDate
    end_date <- committees[[i]]$Committees[[j]]$EndDate
    chair_date <- committees[[i]]$Committees[[j]]$ChairDates
    committee_id <- committees[[i]]$Committees[[j]][[".attrs"]]["Id"]
    committee_name <- nullToNA(committee_name)
    start_date <- nullToNA(start_date)
    end_date <- nullToNA(end_date)
    committee_id <- nullToNA(committee_id)
    if (!is.null(chair_date)){
      for (k in 1:length(chair_date <- committees[[i]]$Committees[[j]]$ChairDates)){
        chair_startDate <- committees[[i]]$Committees[[j]]$ChairDates[[k]]$StartDate
        chair_endDate <- committees[[i]]$Committees[[j]]$ChairDates[[k]]$EndDate
        observation <- data.frame(full_name = ListAs,
                                  House = House,
                                  member_id = member_id,
                                  committee_name = committee_name,
                                  start_date = start_date,
                                  end_date = end_date,
                                  chair_startDate = chair_startDate,
                                  chair_endDate = chair_endDate,
                                  committee_id = committee_id)
        out <- rbind(out, observation)
      }
    } else {
      chair_startDate <- NA
      chair_endDate <- NA
      observation <- data.frame(full_name = ListAs,
                                House = House,
                                member_id = member_id,
                                committee_name = committee_name,
                                start_date = start_date,
                                end_date = end_date,
                                chair_startDate = chair_startDate,
                                chair_endDate = chair_endDate,
                                committee_id = committee_id)
      if (i==1 && j==1) {
        out <- observation
      } else {
        out <- rbind(out, observation)
      }
    } 
  }
 }

#######################
# clean the dataframe
# create committee
#######################
df <- out
rownames(df) <- c()
df_a <- df %>%
  select(committee_name, committee_id, House) %>%
  group_by(committee_name, House) %>%
  count()

df_b <- df %>%
  select(committee_name, committee_id) %>%
  mutate(committee_id = as.numeric(committee_id)) %>%
  unique %>%
  na.omit()
rownames(df_b) <- c()



