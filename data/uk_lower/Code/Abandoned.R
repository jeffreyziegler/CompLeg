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
df <- committees

df$committees <- as.character(df$Committees)
df$committees[df$committees==""] <- "Temp"

df_a <- df %>%
  mutate(full_name = DisplayAs) %>%
  mutate(last_name = str_replace(ListAs,"(.*)(,\\s)(.*)","\\1")) %>%
  mutate(first_name = str_replace(ListAs,"(.*)(,\\s)(.*)","\\3")) %>%
  mutate(constituency_name = MemberFrom)

df_b <- df_a %>%
  mutate(committees = gsub("False", "", committees)) %>%
  mutate(committees = gsub("True", "", committees)) %>%
  mutate(committees = strsplit(as.character(committees), "(?<=\\d{4}-\\d{2}-\\d{2}T00:00:00[A-Z])", perl = TRUE)) %>% 
  unnest(committees)

df_b$temp <- substring(df_b$committees, nchar(df_b$committees), nchar(df_b$committees))
as.character(df_b$temp)
sum(is.null(df_b$temp))

for(i in 1:nrow(df_b)-1){
  if (df_b$temp[i] != "0"){
    df_b$committees[i+1] <- paste(df_b$temp[i], df_b$committees[i+1], sep="")
  }
}

df_b$committees[df_b$committees==""] <- NA
df_c <- subset(df_b, !is.na(committees))
df_c$committees[df_c$committees=="Temp"]<- NA



########################
# create committee.csv
########################
# committee_name <- df_c$committees %>%
#   unique()
# 
# df_aa <- as.data.frame(committee_name)
# df_aa <- df_aa %>%
#   mutate(type = str_split_fixed(committee_name, "\\(",2)[,2]) %>%
#   mutate(committee_name = str_split_fixed(committee_name, "\\(",2)[,1]) %>%
#   mutate(type = gsub("\\)", "", type))

##################################
# merge with the committees.csv
##################################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/uk_lower/Output/')
name <- read.csv("uk_committees.csv")
df_aa <- merge(x=df_c, y=name, by.x="committees", by.y="committee_name", all.x=T)










