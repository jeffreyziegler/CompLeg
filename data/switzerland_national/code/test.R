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

# df <- read_excel("~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/affair/german/5101-2019-wintersession-d.xlsx", range = cell_cols("A:K"))
# 
# df_b[1,]
# df_b[1463,]
# sum(!is.na(df_b[1463,]))==0&&sum(!is.na(df_b[1464,]))==2
# test <- c()
# for(i in 1462:1465){
#   if((sum(!is.na(df_b[i,]))==1&&sum(!is.na(df_b[i+1,]))==0)||(sum(!is.na(df_b[i-1,]))==0&&sum(!is.na(df_b[i,]))==1)){
#     a = df_b$V1[i]
#   } else if((sum(!is.na(df_b[i,]))==2&&sum(!is.na(df_b[i+1,]))==0)||(sum(!is.na(df_b[i-1,]))==0&&sum(!is.na(df_b[i,]))==2)){
#     a = df_b$V1[i]
#     b = df_b$V2[i]
#   }
#   df_b$V6[i] = a
#   df_b$V7[i] = b
# }

url
test <- read_html("~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/affair_text/20070057_d.html")
t <- test %>%
  html_node(".ng-binding") %>%
  html_text()

which(affairId_list==20143854)
affairId_list[2013]
