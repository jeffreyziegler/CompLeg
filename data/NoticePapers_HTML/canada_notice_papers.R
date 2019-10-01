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

# set working directory
setwd('~/Documents/GitHub/CompLegFall2019/data/NoticePapers_HTML/')

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo"), pkgTest)


#################################
# function to parse HTML
#################################

parse_HTML <- function(file) {
  
  # parliament
  info <- str_split(file, "_|\\.")
  info <- unlist(info)
  
  # read in HTML
  html <- read_html(file)
  
  # heading
  h <- html %>% html_nodes("b") %>% html_text()
  sitting_date <- str_extract(h[6], "[A-Z][a-z]+, [A-Z][a-z]+ [0-9]{1,2}, [0-9]{4}")

  # text
  td <- html %>% html_nodes("td")
  td_text <- td %>% html_text()

  # footnote
  sup <- td %>% html_node("b") %>% as.character()
  sup <- as.numeric(str_detect(sup, "sup>2<"))
  
  # class
  class <- td %>% html_attr("class")
  
  # headings
  h3 <- td %>% html_node("h3") %>% html_text()
  h4 <- td %>% html_node("h4") %>% html_text()
  h4[h3 == "Private Members' Business"] <- "Private Members' Business"
  
  # paragraph
  p <- td %>% html_node("p") %>% html_text()
  
  # text
  td <- td %>% html_text()

  # check number of questions
  if(length(td) == 0) {
    return(NULL)
  }
  
  # make a data frame
  out <- data.frame(key_ID = 1:length(td), 
                    parliament_number = info[3],
                    session_number = info[4],
                    chamber_number = 1, 
                    sitting_number = info[5],
                    sitting_date = sitting_date,
                    heading = h4,
                    class = class,
                    footnote = sup,
                    text = td,
                    paragraph = p,
                    stringsAsFactors = FALSE)
  
  # return data frame
  return(out)
}

##################################################
# read in data
##################################################

# file names
files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)
# parse XML
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)
# stack data frames
out <- do.call("rbind", out)
# data frame
dat <- out

