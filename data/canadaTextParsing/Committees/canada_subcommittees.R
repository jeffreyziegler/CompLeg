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
df <- read_excel("raw_canada_subcommittees.xlsx")
df_aa <- read_excel("raw_canada_committees.xlsx")

# select from the raw data
df_a <- df %>%
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

df_c <- df_b
x <- strsplit(df_c$committee_name, "[ ]")
df_c$super_committee_acronym <- sapply( x, "[", 3 )

df_d <- merge(x=df_c, y=df_aa, by.x="super_committee_acronym", by.y="committee_acronym")
df_d$super_committee_name <- df_d$committee_name.y
df_d$super_committee_type <- df_d$type.y
df_d$committee_name <- df_d$committee_name.x


canada_subcommittees <- df_d %>%
  select(observation_path,
         chamber_path,
         committee_path,
         observation_number,
         chamber_number,
         committee_number,
         committee_name,
         committee_acronym,
         committee_type,
         super_committee_acronym,
         super_committee_name,
         super_committee_type) %>%
  unique() %>%
  arrange(as.numeric(observation_number))

# output
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
write.csv(canada_subcommittees, "canada_subcommittees.csv")
