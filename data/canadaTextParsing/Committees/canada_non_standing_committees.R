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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo","readxl"), pkgTest)

#####################
# read in data
#####################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read_excel("raw_canada_committees.xlsx")

# select from the raw data
df_a <- df %>%
  filter(type!="Standing Committees") %>%
  select(committee_name,committee_acronym,type) %>%
  distinct()

# re-arrange
df_b <- df_a[order(df_a$committee_name),]

# clean
df_b$chamber_number <- 1
df_b$committee_number <- rownames(df_b)
df_b$observation_number <- rownames(df_b)
df_b$committee_type <- df_b$type

df_b$chamber_path <- paste("/chamber-", df_b$chamber_number, sep="")
df_b$committee_path <- paste(df_b$chamber_path,"/committee-",df_b$committee_number,sep="") 
df_b$observation_path <- paste(df_b$chamber_path,"/committee-",df_b$observation_number,sep="")

canada_non_standing_committees <- df_b %>%
  select(observation_path,
         chamber_path,
         committee_path,
         observation_number,
         chamber_number,
         committee_number,
         committee_name,
         committee_acronym,
         committee_type)

# output
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
write.csv(canada_non_standing_committees, "canada_non_standing_committees.csv")
