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
# import excel
# create raw data frame
############################
df <- read.csv("~/Documents/GitHub/CompLegFall2019/data/switzerland_national/raw_data/parliamentary_group/ra-fraktionen-statistik-1912-d.csv", 
                stringsAsFactors = F, header=F, na.strings=c("", "NA"))

############################
# clean and export
############################
df_a<-df
rownames(df_a) <- c()
df_a$V7 <- NA

# if two concecutive na rows, make it into one na row for further operation
x = nrow(df_a)
remove <- c()
for(i in 1:x){
  if(sum(!is.na(df_a[i,]))==0&sum(!is.na(df_a[i+1,]))==0){
    remove <- c(remove, i)
  }
}
df_b <- df_a[-c(remove),]
rownames(df_b) <- c()

# assign date and parliamentary number to each observation
a = df_b$V1[1]
b = df_b$V2[1]
x = nrow(df_b)
df_c <- df_b
for(i in 2:x){
  if((sum(!is.na(df_b[i,]))==1&sum(!is.na(df_b[i+1,]))==0)|(sum(!is.na(df_b[i-1,]))==0&sum(!is.na(df_b[i,]))==1)){
    a = df_b$V1[i]
  } else if((sum(!is.na(df_b[i,]))==2&sum(!is.na(df_b[i+1,]))==0)|(sum(!is.na(df_b[i-1,]))==0&sum(!is.na(df_b[i,]))==2)){
    a = df_b$V1[i]
    b = df_b$V2[i]
  }
  df_c$V6[i] = a
  df_c$V7[i] = b
}

# remove NAs and the necessary rows
remove <- c(1,2)
for(i in 1:x){
  if((sum(!is.na(df_b[i,]))==1&sum(!is.na(df_b[i+1,]))==0)|(sum(!is.na(df_b[i,]))==2&sum(!is.na(df_b[i+1,]))==0)){
    remove <- c(remove, i, i+1)
  }
  if((sum(!is.na(df_b[i-1,]))==0&sum(!is.na(df_b[i,]))==1)|(sum(!is.na(df_b[i-1,]))==0&sum(!is.na(df_b[i,]))==2)){
    remove <- c(remove, i-1, i)
  }
}
final_remove <- remove %>% unique()
df_d <- df_c[-c(final_remove),]

# column names
colnames(df_d) <- c("parliamentary_group_name",
                    "V2",
                    "NR",
                    "SR",
                    "BV",
                    "date",
                    "parliament_number",
                    "V7")

df_e <- df_d
df_e$NR[is.na(df_e$NR)] <- 0
df_e$SR[is.na(df_e$SR)] <- 0
df_e$BV[is.na(df_e$BV)] <- 0
df_e$NR <- as.numeric(df_e$NR)
df_e$SR <- as.numeric(df_e$SR)
df_e$BV <- as.numeric(df_e$BV)

df_f <- df_e
x = nrow(df_e)
for(i in 1:x){
  if(sum(df_e$NR[i]+df_e$SR[i]+df_e$BV[i])>100 & is.na(df_d$parliamentary_group_name[i])){
    df_f$parliamentary_group_name[i] <- "total"
  }
}
df_f$parliament_number <- substr(df_d$parliament_number, 1, 2)

# export
switzerland_parliamentary_group_sitting_d <- df_f %>%
  select(parliamentary_group_name,
         NR,
         SR,
         BV,
         date,
         parliament_number)
setwd('~/Documents/GitHub/CompLegFall2019/data/switzerland_national/output')
# write.csv(switzerland_parliamentary_group_sitting_d, "switzerland_parliamentary_group_sitting_d.csv")




