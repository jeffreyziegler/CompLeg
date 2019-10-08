# Josh Fjelstul

# libraries
library(stringr)
library(plyr)
library(tidyverse)
library(dplyr)

##################################################
# make a dataset of MPs
##################################################

# set working directory
setwd("~/Dropbox/Professional/Projects/Parliaments Project/Canada/Members (CSV)/")

# read in data
dat_38 <- read.csv("canada_commons_members_parl38.csv", stringsAsFactors = FALSE)
dat_39 <- read.csv("canada_commons_members_parl39.csv", stringsAsFactors = FALSE)
dat_40 <- read.csv("canada_commons_members_parl40.csv", stringsAsFactors = FALSE)
dat_41 <- read.csv("canada_commons_members_parl41.csv", stringsAsFactors = FALSE)

# session
dat_38$parliament_number <- 38
dat_39$parliament_number <- 39
dat_40$parliament_number <- 40
dat_41$parliament_number <- 41

# stack
dat <- rbind(dat_38, dat_39, dat_40, dat_41)

# rename variables
names(dat) <- c("title", "first_name", "last_name", "constituency_name", "province_name", "party_name", "start_date", "end_date", "parliament_number")

# chamber number
dat$chamber_number <- 1

# trim white space
dat$first_name <- str_replace_all(dat$first_name, "[[:space:]]+", " ")
dat$last_name <- str_replace_all(dat$last_name, "[[:space:]]+", " ")
dat$constituency_name <- str_replace_all(dat$constituency_name, "[[:space:]]+", " ")
dat$province_name <- str_replace_all(dat$province_name, "[[:space:]]+", " ")
dat$party_name <- str_replace_all(dat$party_name, "[[:space:]]+", " ")
dat$first_name <- str_trim(dat$first_name)
dat$last_name <- str_trim(dat$last_name)
dat$constituency_name <- str_trim(dat$constituency_name)
dat$province_name <- str_trim(dat$province_name)
dat$party_name <- str_trim(dat$party_name)

# full name
dat$full_name <- str_c(dat$first_name, dat$last_name, sep = " ")

# drop missing
dat <- filter(dat, first_name != "")

# fix constituency name
dat$constituency_name[dat$constituency_name == "Western Arctic"] <- "Northwest Territories"

# clean dates
dat$start_year <- str_extract(dat$start_date, "[0-9]{4}")
dat$start_month <- str_extract(dat$start_date, "^[0-9]{1,2}")
dat$start_day <- str_extract(dat$start_date, "/[0-9]{1,2}/")
dat$start_day <- str_extract(dat$start_day, "[0-9]{1,2}")
dat$end_year <- str_extract(dat$end_date, "[0-9]{4}")
dat$end_month <- str_extract(dat$end_date, "^[0-9]{1,2}")
dat$end_day <- str_extract(dat$end_date, "/[0-9]{1,2}/")
dat$end_day <- str_extract(dat$end_date, "[0-9]{1,2}")

# pad
dat$start_month <- str_pad(dat$start_month, width = 2, side = "left", pad = "0")
dat$start_day <- str_pad(dat$start_day, width = 2, side = "left", pad = "0")
dat$end_month <- str_pad(dat$end_month, width = 2, side = "left", pad = "0")
dat$end_day <- str_pad(dat$end_day, width = 2, side = "left", pad = "0")

# dates
dat$start_date <- str_c(dat$start_year, dat$start_month, dat$start_day, sep = "-")
dat$end_date <- str_c(dat$end_year, dat$end_month, dat$end_day, sep = "-")
dat$start_date <- as.Date(dat$start_date)
dat$end_date <- as.Date(dat$end_date)

# drop variables
dat$start_day <- NULL
dat$start_month <- NULL
dat$start_year <- NULL
dat$end_day <- NULL
dat$end_month <- NULL
dat$end_year <- NULL

# clean party names
dat$party_name[dat$party_name == "Conservative"] <- "Conservative Party of Canada"
dat$party_name[dat$party_name == "Liberal"] <- "Liberal Party of Canada"
dat$party_name[dat$party_name == "NDP"] <- "New Democratic Party"
dat$party_name[dat$party_name == "Green Party"] <- "Green Party of Canada"

# drop title
dat$title <- NULL

# chamber name
dat$chamber_name <- "House of Commons"

###########################################################################
###########################################################################
# constituencies
###########################################################################
###########################################################################

# duplicate
dat$duplicate <- duplicated(dat$constituency_name)

# filter
constituencies <- filter(dat, !duplicate)

# select variables
constituencies <- select(constituencies, constituency_name, province_name)

# chamber number
constituencies$chamber_number <- 1

# arrange
constituencies <- arrange(constituencies, constituency_name)

# constituency number
constituencies$constituency_number <- 1:nrow(constituencies)

# observation number
constituencies$observation_number <- 1:nrow(constituencies)

# chamber path
constituencies$chamber_path <- "/chamber-1"

# constituency path
constituencies$constituency_path <- str_c("/chamber-1/constituency-", constituencies$constituency_number)

# observation path
constituencies$observation_path <- constituencies$constituency_path

# chamber name
constituencies$chamber_name <- "House of Commons"

# select variables
constituencies <- select(constituencies, observation_path, chamber_path, constituency_path,
                         observation_number, chamber_number, constituency_number,
                         chamber_name, constituency_name, province_name)

# set working directory
setwd("~/Dropbox/Professional/Projects/Parliaments Project/Canada/Final Data (CSV)/")

# read file
write.csv(constituencies, "canada_constituencies.csv", row.names = FALSE)

###########################################################################
###########################################################################
# members
###########################################################################
###########################################################################

# collapse
members <- dat %>% 
  group_by(first_name, last_name, constituency_name) %>% 
  summarize(full_name = full_name[1],
            party_name = party_name[1],
            start_date = min(start_date), end_date = max(end_date)) %>%
  ungroup()

# sort
members <- arrange(members, last_name, first_name, constituency_name)

# member number
members$member_number <- 1:nrow(members)

# chamber number
members$chamber_number <- 1

# observation number 
members$observation_number <- 1:nrow(members)

# chamber path
members$chamber_path <- "/chamber-1"

# member path
members$member_path <- str_c("/chamber-1/member-", members$member_number)

# observation number
members$observation_path <- members$member_path

# chamber name
members$chamber_name <- "House of Commons"

# merge in constituency path
members <- left_join(members, select(constituencies, constituency_name, constituency_path), by = c("constituency_name"))

# rename variable
members <- rename(members, constituency_ID = constituency_path)

# select variables
members <- select(members, observation_path, chamber_path, member_path,
                  observation_number, chamber_number, member_number,
                  chamber_name, full_name, first_name, last_name, 
                  constituency_name, constituency_ID, party_name,
                  start_date, end_date)

# set working directory
setwd("~/Dropbox/Professional/Projects/Parliaments Project/Canada/Final Data (CSV)/")

# read file
write.csv(members, "canada_members.csv", row.names = FALSE)

###########################################################################
###########################################################################
# parliament membership
###########################################################################
###########################################################################

# member number
dat <- dat %>% 
  group_by(parliament_number) %>% 
  arrange(last_name, first_name, constituency_name) %>%
  mutate(member_number = 1:n()) %>% 
  ungroup()

# parliament path
dat$parliament_path <- str_c("/parliament-", dat$parliament_number)

# chamber path
dat$chamber_path <- str_c("/parliament-", dat$parliament_number,
                          "/chamber-1")

# member path
dat$member_path <- str_c("/parliament-", dat$parliament_number,
                         "/chamber-1",
                         "/member-", dat$member_number)

# observation path
dat$observation_path <- dat$member_path

# chamber name
dat$chamber_name <- "House of Commons"

# arrange
dat <- arrange(dat, parliament_number, chamber_number, member_number)

# observation number
dat$observation_number <- 1:nrow(dat)

# merge in constituency path
dat <- left_join(dat, select(constituencies, constituency_name, constituency_path), by = "constituency_name")

# rename variable
dat <- rename(dat, constituency_ID = constituency_path)

# merge in member path
x <- select(members, full_name, constituency_name, member_path)
x <- rename(x, member_ID = member_path)
dat <- left_join(dat, x, by = c("full_name", "constituency_name"))

# select variables
dat <- select(dat, observation_path, parliament_path, chamber_path, member_path, 
              observation_number, parliament_number, chamber_number, member_number,
              chamber_name, full_name,  first_name, last_name, member_ID, constituency_name, constituency_ID, 
              party_name, start_date, end_date)

# set working directory
setwd("~/Dropbox/Professional/Projects/Parliaments Project/Canada/Final Data (CSV)/")

# read file
write.csv(dat, "canada_chamber_membership.csv", row.names = FALSE)

##################################################
# end R script
##################################################