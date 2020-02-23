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
setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/affair/german')
file_names <- list.files(pattern="*.xlsx", full.names=TRUE, recursive=FALSE)
df <- do.call(rbind, lapply(file_names, function(x) cbind(read_excel(x,range = cell_cols("A:K")), parliament_number=strsplit(x,'-|\\.|\\/')[[1]][3])))

############################
# clean and export
############################
df_a <- df
df_a$parliament_number = as.numeric(substr(df$parliament_number, 1, 2))
df_a$session_number = as.numeric(substr(df$parliament_number, 3, 4))
colnames(df_a) <- c("vote_date",
                    "council",
                    "committee",
                    "department",
                    "affair_id",
                    "affair_title",
                    "vote_registration_number",
                    "vote_meaning_yes",
                    "vote_meaning_no",
                    "division_text",
                    "vote_submission_text",
                    "parliament_number",
                    "session_number")

rownames(df_a) <- c()
df_a$observation_number <- rownames(df_a)
df_a$affair_path <- paste("/affair-", df_a$affair_id, sep="")
df_a$vote_registration_path <- paste(df_a$affair_path, "/vote_registration-", df_a$vote_registration_number, sep="")
df_a$observation_path <- df_a$vote_registration_path 

switzerland_affair_d <- df_a %>%
  select(observation_path,
         affair_path,
         vote_registration_path,
         observation_number,
         affair_id,
         affair_title,
         vote_registration_number,
         parliament_number,
         session_number,
         vote_date,
         council,
         committee,
         department,
         vote_meaning_yes,
         vote_meaning_no,
         division_text,
         vote_submission_text)

# write csv
setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/output')
# write.csv(switzerland_affair_d, "switzerland_affair_d.csv")


