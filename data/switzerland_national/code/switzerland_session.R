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

############################
# import html
# create raw data frame
############################
html <- read_html("~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/session/Frühere Sessionen.html")

raw <- html %>%
  html_nodes(".pd-rteTableEvenCol-highlighted, .pd-rteTableOddCol-highlighted, .td-style") %>% 
  html_text() %>%
  trimws()

description <- raw[c(T,F,F,F,F)]
start_date <- as.Date(raw[c(F,T,F,F,F)], format="%d.%m.%Y")
end_date <- as.Date(raw[c(F,F,T,F,F)], format="%d.%m.%Y")
legislature <- raw[c(F,F,F,T,F)]
session_number <- raw[c(F,F,F,F,T)]

df <- data.frame(parliament_number = legislature,
                 session_number = session_number,
                 session_description = description,
                 start_date = start_date,
                 end_date = end_date)

############################
# clean and export
############################
df_a <- df
rownames(df_a) <- c()
df_a$observation_number <- rownames(df_a)
df_a$parliament_path <- paste("/parliament-", df_a$parliament_number, sep="")
df_a$session_path <- paste(df_a$parliament_path, "/session-", df_a$session_number, sep="")
df_a$observation_path <- df_a$session_path

switzerland_session_d <- df_a %>%
  select(observation_path,
         parliament_path,
         session_path,
         observation_number,
         parliament_number,
         session_number,
         session_description,
         start_date,
         end_date)

# write csv
setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/output')
write.csv(switzerland_session_d, "switzerland_session_d.csv")

