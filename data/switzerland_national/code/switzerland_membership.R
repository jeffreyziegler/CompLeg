############################
# clean environment
# load packages
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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "XML", "tidyr", "lubridate", "readxl"), pkgTest)

##########################
# import data
# create raw dataframe
##########################
df <- read_excel("~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/membership/german/Ratsmitglieder_1848_DE.xlsx")

##########################
# clean the data
##########################
df_a <- df
colnames(df_a) <- c("active",
                    "first_name",
                    "last_name",
                    "gender",
                    "canton_name",
                    "canton_abbreviation",
                    "council_name",
                    "parliamentary_group_name",
                    "parliamentary_group_abbreviation",
                    "party_name",
                    "party_abbreviation",
                    "marital_status",
                    "birth_place_city",
                    "birth_place_canton",
                    "mandates",
                    "date_joining",
                    "date_leaving",
                    "citizenship",
                    "date_of_birth",
                    "date_of_death")

df_a$date_joining <- as.Date(df$DateJoining,format="%d.%m.%Y")
df_a$date_leaving <- as.Date(df$DateLeaving,format="%d.%m.%Y")
df_a$date_of_birth <- as.Date(df$DateOfBirth,format="%d.%m.%Y")
df_a$date_of_death <- as.Date(df$DateOfDeath,format="%d.%m.%Y")
  
  


