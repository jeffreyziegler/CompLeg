#######################
# load libraries
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

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "readxl"), pkgTest)

########################
# read in data useful
########################
# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("canada_committees_date_change.csv")

###################################
# read in useful data in iteration
###################################
# change the date to the one in url 
df$date_change <- gsub(",","",df$date_change)
df$dc <- as.Date(df$date_change, format="%B %d %Y")
x <- nrow(df)

# download through iteration
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Associate Membership')
dc <- df$dc
acronym <- df$committee_acronym
pn <- df$parliament_number
sn <- df$session_number
number <- paste("/Members?membershipon=",dc, "&parl=", pn,"&session=",sn,"&includeAssociates=True#AssociateMembers", sep="")

for(i in 1:x){
  url <- paste("https://www.ourcommons.ca/Committees/en/", acronym[i], number[i], sep="")
  file <- paste(acronym[i], "_", pn[i], "_", sn[i], "_", dc[i], ".html", sep="")
  # download.file(url, file, quiet = TRUE)
}

#######################
# parse HTML function
#######################
parse_HTML <- function(file) {
  # get some information from the name of the file
  info <- str_split(file, "\\/|_|\\.")
  info <- unlist(info)
  
  # read html
  html <- read_html(file)
  
  # get names of the associate members
  name <- html %>%
    html_nodes(".name") %>% 
    html_text()
  
  # check 
  if (is_empty(name)) {
    return(NULL)
  }
  
  # return the dataframe
  out <- data.frame(committee_acronym = info[3],
                    parliament_number = info[4],
                    session_number = info[5],
                    date_change = info[6],
                    position = "Associate Member",
                    full_name = name)
  return(out)
}

#####################
# read in data
#####################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Associate Membership')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)
raw_canada_committee_associate_membership <- df

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(raw_canada_committee_associate_membership, "raw_canada_committee_associate_membership.csv")

#######################
# clean dataset
#######################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("raw_canada_committee_associate_membership.csv")
df_a <- read_excel("raw_canada_committees.xlsx")

# find the full name of the committee
df_b <- merge(x=df, y=df_a, by=c("committee_acronym","parliament_number","session_number"), all.x=TRUE)

# start date
df_c <- df_b %>%
  group_by(parliament_number, session_number, full_name, position) %>%
  slice(which.min(date_change)) %>%
  distinct()

# end date
df_d <- df_b %>%
  group_by(parliament_number, session_number, full_name, position) %>%
  slice(which.max(date_change)) %>%
  distinct()

# form the raw dataset that would be useful later
final <- data.frame(committee_name = df_c$committee_name,
                    committee_acronym = df_c$committee_acronym,
                    parliament_number = df_c$parliament_number,
                    session_number = df_c$session_number,
                    full_name = df_c$full_name,
                    start_date = df_c$date_change,
                    end_date = df_d$date_change,
                    position = df_c$position)

# merge with member
# get information about member and constituency
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing')
member <- read.csv("canada_members_updated.csv")
df_e <- merge(x=final, y=member, by="full_name", all=T)

# merge with committee
# get information about committee
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
committee <- read.csv("canada_committees.csv")
df_f <- merge(x=df_e, y=committee, by=c("committee_name","committee_acronym"))

# select and re-arrange the raw dataset
df_g <- df_f %>%
  select(parliament_number,
         session_number,
         chamber_number.x,
         committee_number,
         member_number,
         committee_name,
         committee_acronym,
         committee_path,
         full_name,
         member_path,
         constituency_name,
         constituency_ID,
         position,
         start_date.x,
         end_date.x)
colnames(df_g) <- c("parliament_number",
                    "session_number",
                    "chamber_number",
                    "committee_number",
                    "member_number",
                    "committee_name",
                    "committee_acronym",
                    "committee_ID",
                    "member_name",
                    "member_ID",
                    "constituency_name",
                    "constituency_ID",
                    "position",
                    "start_date",
                    "end_date")

# re-order the dataset to set observation number
df_h <- df_g[order(df_g$parliament_number, df_g$session_number, df_g$chamber_number, df_g$member_number),]
rownames(df_h) <- c()
df_h$observation_mumber <- rownames(df_h)

# final clean up
df_h$parliament_path <- paste("/parliament-", df_h$parliament_number, sep="")
df_h$chamber_path <- paste(df_h$parliament_path, "/chamber-", df_h$chamber_number, sep="")
df_h$committee_path <- paste(df_h$chamber_path, "/committee-", df_h$committee_number, sep="")
df_h$member_path <- paste(df_h$committee_path, "/member-", df_h$member_number, sep="")
df_h$observation_path <- paste(df_h$committee_path, "/member-", df_h$member_number, sep="")

# finish!
canada_committee_associate_membership <- df_h
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(canada_committee_associate_membership, "canada_committee_associate_membership.csv")
