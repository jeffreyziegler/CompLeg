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
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Membership')
dc <- df$dc
acronym <- df$committee_acronym
pn <- df$parliament_number
sn <- df$session_number
number <- paste("/Members?parl=",pn,"&session=",sn,"&membershipOn=", sep="")

for(i in 1:x){
  url <- paste("https://www.ourcommons.ca/Committees/en/", acronym[i], number[i], dc[i], "#committeeMembersPanel", sep="")
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
  
  # first_name, and title. get two information together in order to decide the title/position by 
  # the order of the information.
  raw <- html %>%
    html_nodes(".title,.first-name") %>% 
    html_text()
  
  # check 
  if(length(raw) == 0) {
    return(NULL)
  }
  
  # with order, get position
  x <- as.numeric(which(raw=="Chair"))
  y <- as.numeric(which(raw=="Vice-Chairs" | raw=="Vice-Chair"))
  z <- as.numeric(which(raw=="Members"))
  k <- as.numeric(length(raw))
  title <- "Initiate"
  
  if (is_empty(k)) {
    title <- NA
  } else if (!is_empty(x) & !is_empty(y) & is_empty(z)){
    title[c(x:y-1)] <- "Chair"
    title[c(y:k)] <- "Co-chair"
  } else if (is_empty(x) & !is_empty(y) & is_empty(z)){
    title[c(y:k)] <- "Co-chair"
  } else if (!is_empty(x) & is_empty(y) & is_empty(z)){
    title[c(x:k)] <- "Chair"
  } else if (is_empty(x) & is_empty(y)) { 
    title[c(z:k)] <- "Member"
  } else if(is_empty(x) & !is_empty(y)){
    title[c(y:z-1)] <- "Co-chair"
    title[c(z:k)] <- "Member"
  } else if (!is_empty(x) & is_empty(y)) { 
    title[c(x:z-1)] <- "Chair"
    title[c(z:k)] <- "Member"
  } else {
    title[c(x:y-1)] <- "Chair"
    title[c(y:z-1)] <- "Co-chair"
    title[c(z:k)] <- "Member"
  }

  # since each contains both desktop version and mobile version, (mobile version doesn't have
  # middle name), make sure that only the full name remains
  first_name <- raw[-c(x,y,z)][c(TRUE,FALSE)]
  title <- title[-c(x,y,z)][c(TRUE,FALSE)]
  
  # same, but for last name only
  last_name <- html %>%
    html_nodes(".last-name") %>% 
    html_text()
  last_name <- last_name[c(TRUE,FALSE)]
  
  # full name
  full_name <- paste(first_name, last_name, sep=" ")
  
  # return the dataframe
  out <- data.frame(committee_acronym = info[3],
                    parliament_number = info[4],
                    session_number = info[5],
                    date_change = info[6],
                    position = title,
                    first_name = first_name,
                    last_name = last_name,
                    full_name = full_name)
  return(out)

}

#####################
# read in data
#####################
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees/Membership')
# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
df <- do.call("rbind", out)
raw_canada_committee_membership <- df

setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(raw_canada_committee_membership, "raw_canada_committee_membership.csv")


###########
# clean 
###########
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
df <- read.csv("raw_canada_committee_membership.csv")
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
canada_committee_membership <- df_h
setwd('~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Committees')
# write.csv(canada_committee_membership, "canada_committee_membership.csv")
  