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

#######################
# parse HTML function
#######################
parse_HTML <- function(file) {
  # get some information from the name of the file

  info <- str_split(file, "\\/| - |\\.")
  info <- unlist(info)

  # read html
  html <- read_html(file)
  
  # select name
  raw <- html %>%
    html_nodes("h3") %>% 
    html_text()
  
  # omit confounding names
  clean <- raw[raw!="What do Select Committees do?" 
               & raw!= "Commons committee calendar" 
               & raw!= "Guide for witnesses" 
               & raw!= "Watch Parliament TV"
               & raw!= "House of Lords Committee Bulletin"
               & raw!= "Scrutiny uncovered"]
  
  # return the dataframe
  out <- data.frame(committee_type = info[3],
                    committee_name = clean)
  return(out)
}

################################
# read in html
# parse and create raw dataset
################################
setwd('~/Documents/GitHub/CompLegFall2019/data/uk_lower/Committee')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse HTML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)

#####################
# clean the dataset
#####################
# clean up committee type
df$committee_type[df$committee_type=="Former Commons Select Committees"] <- "House of Commons select committees"
df$committee_type[df$committee_type=="Former Lords Select Committees"] <- "House of Lords Select Committees"
df$committee_type[df$committee_type=="Former Joint Select Committees"] <- "Joint Select Committees"
df$committee_type[df$committee_type=="Former Other Committees"] <- "Other Committees"

# assign chamber number
df$chamber_number[df$committee_type=="House of Commons select committees"] <- 1
df$chamber_number[df$committee_type=="House of Lords Select Committees"] <- 2
df$chamber_number[df$committee_type=="Joint Select Committees"] <- 3
df$chamber_number[df$committee_type=="Other Committees"] <- 4

# committee number
df <- df[order(as.character(df$committee_name)),]
rownames(df) <- c()
df$committee_number <- rownames(df)

# observation number
df_a <- df[order(as.numeric(df$chamber_number), as.numeric(df$committee_number)),]
rownames(df_a) <- c()
df_a$observation_number <- rownames(df_a)

# path
df_a$chamber_path <- paste("/chamber-", df_a$chamber_number, sep="")
df_a$committee_path <- paste(df_a$chamber_path,"/committee-",df_a$committee_number,sep="") 
df_a$observation_path <- paste(df_a$committee_path,"/observation-",df_a$observation_number,sep="")

uk_committees <- df_a %>%
  select(observation_path,
         chamber_path,
         committee_path,
         observation_number,
         chamber_number,
         committee_number,
         committee_name,
         committee_type)

#####################
# write csv
#####################
setwd('~/Documents/GitHub/CompLegFall2019/data/uk_lower/Output')
#write.csv(uk_committees, "uk_committees.csv")



