#######################
# set working directory
# load data
# and load libraries
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

#####################
# read in data
#####################

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/')
canada_chamber_membership <- read.csv("canada_chamber_membership.csv", encoding="UTF-8")
canada_constituencies <- read.csv("canada_constituencies.csv", encoding="UTF-8")
canada_members <- read.csv("canada_members.csv", encoding="UTF-8")

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Members (CSV)') 
file_names <- list.files(pattern="*.csv", full.names=TRUE, recursive=FALSE)
df <- do.call(rbind, lapply(file_names, function(x) cbind(read.csv(x,encoding="UTF-8",na.strings=c("", "NA")), parliament_number=strsplit(x,'_|\\.')[[1]][5])))

##############################
# update constituencies
##############################

# import data
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Members (CSV)')
canada_commons_members_parl42 <- read.csv("canada_commons_members_parl42.csv", encoding="UTF-8")

# update the dataframe, sort by constituency_name
colnames(canada_commons_members_parl42)[which(names(canada_commons_members_parl42) == "Province...Territory")] <- "province_name"
colnames(canada_commons_members_parl42)[which(names(canada_commons_members_parl42) == "Constituency")] <- "constituency_name"
updated <- merge(canada_constituencies,canada_commons_members_parl42,by=c("constituency_name","province_name"),all=TRUE) %>%
  distinct()
#canada_constituencies_updated <- updated[order(updated$constituency_name)]
sorted_name <- sort(updated$constituency_name)
name <- data.frame(sorted_name) 
canada_constituencies_updated <- merge(x=updated,y=name,by.x="constituency_name",by.y="sorted_name",all.y=TRUE) %>%
  select(constituency_name,
         observation_path,
         chamber_path,
         constituency_path,
         observation_number,
         chamber_number,
         constituency_number,
         chamber_name,
         province_name) %>%
  distinct()

# clean the dataframe
colnames(canada_constituencies_updated)
canada_constituencies_updated$observation_number <- as.numeric(rownames(canada_constituencies_updated))
canada_constituencies_updated$chamber_number <- 1
canada_constituencies_updated$constituency_number <- as.numeric(rownames(canada_constituencies_updated))
canada_constituencies_updated$chamber_name <- "House of Commons"
canada_constituencies_updated$chamber_path <- paste("/chamber-",canada_constituencies_updated$chamber_number,sep="") 
canada_constituencies_updated$constituency_path <- paste(canada_constituencies_updated$chamber_path,"/constituency-",canada_constituencies_updated$constituency_number,sep="") 
canada_constituencies_updated$observation_path <- paste(canada_constituencies_updated$chamber_path,"/observation-",canada_constituencies_updated$observation_number,sep="") 

#######################
# member
#######################

# change column names
colnames(df) <- c("honorific_title","first_name","last_name","constituency_name","province_name","political_affiliation","start_date","end_date","parliament_number")

# drop all the empty rows and set full name
df<-df[!is.na(df$first_name),]
df$full_name <- paste(df$first_name, df$last_name, sep=" ")

# set start and end date to date format
df_a <- df %>% 
  separate(start_date, c("month", "day","year",NA), sep = "/| ") %>%
  separate(parliament_number, c(NA,"parliament_number"), sep="l") 
df_a$start_date <- as.Date(paste(df_a$year,df_a$month,df_a$day,sep="-"))
df_b <- df_a %>%
  separate(end_date, c("month", "day","year",NA), sep = "/| ")
df_b$end_date <- as.Date(paste(df_b$year,df_b$month,df_b$day,sep="-"))
df_b$end_date[is.na(df_b$end_date)] <- as.Date("2019-09-11")

# merge with constituency
df_c <- merge(x=df_b,y=canada_constituencies_updated,by="constituency_name",all.x=TRUE) %>%
  select(observation_path,
         chamber_path,
         observation_number,
         chamber_number,
         chamber_name,
         full_name,
         first_name,
         last_name,
         constituency_name,
         constituency_path,
         start_date,
         end_date) %>%
  distinct()

# select the correct start and end date
df_d <- df_c %>%
  group_by(full_name) %>%
  slice(which.min(start_date)) %>%
  distinct()
df_e <- df_c %>%
  group_by(full_name) %>%
  slice(which.max(end_date)) %>%
  distinct()
df_e$start_date <- df_d$start_date

# sort member by last name, first name, and constituency name
df_g <- df_e[order(df_e$last_name,df_e$first_name,df_e$constituency_name),]
df_g$member_number <- rownames(df_g)
df_g$member_path <- paste(df_g$chamber_path,"/member-",df_g$member_number,sep="") 

# update observation path, observation number, party name, and constituency ID
df_g$observation_path <- paste(df_g$chamber_path,"/member-",df_g$member_number,sep="") 
colnames(df_g)[which(names(df_g) == "constituency_path")] <- "constituency_ID"
df_g$observation_number <- rownames(df_g)
df_g$party_name <- NA

########### missing party name!! #################
################ Is constituency_ID the same as constituency_path?
##################################################

canada_members_updated <- df_g[c("observation_path",
                                 "chamber_path",
                                 "member_path",
                                 "observation_number",
                                 "chamber_number",
                                 "member_number",
                                 "chamber_name",
                                 "full_name",
                                 "first_name",
                                 "last_name",
                                 "constituency_name",
                                 "constituency_ID",
                                 "party_name",
                                 "start_date",
                                 "end_date")]

#######################
# membership
#######################

# TODO: raw data
df_aa <- merge(x=df_b,y=canada_members_updated ,by="full_name",all.x=TRUE) %>%
  select(observation_path,
         parliament_number,
         chamber_path,
         observation_number,
         parliament_number,
         chamber_number,
         chamber_name,
         full_name,
         first_name.x,
         last_name.x,
         constituency_name,
         constituency_path,
         start_date,
         end_date) %>%
  distinct()
  