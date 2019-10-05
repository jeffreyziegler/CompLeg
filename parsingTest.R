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


#####################################

test <- read_html("notice_papers_41_2_2.html")

files <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)

number <- test %>% html_nodes("b") %>% html_text() 

info <- str_split(files, "_|\\.")
info <- unlist(info)

# everything on the websie, as well as the actual content that we are interested in
everything <- test %>% html_nodes("td")
content <- test %>% html_nodes(".ItemPara") 
contentText <- content %>% html_text()

## About the foot note, here we only can detect if the footnote exists or not
## If would be better if we can remove it from text
## And put it as a numeric varibale in Sup. 
## We need to note what this sup means at the same time
sup <- content %>% html_nodes(".Footnote , sup") %>% html_text()
supIndex <- sup
supText <- test %>% html_nodes(".Footnote") %>%  html_text()
supText <- trimws(supText, c("both"))
# Here sup mataches questionNumber, and the supText provides actual content of each sup
# We can split the supText and merge it again with sup, and thus we can know the acutal
# sup of each questionNumber

# keyID of each motion
# TODO: need to aviod sup
keyIDCheck <- str_extract(number[8:length(number)], "[A-Z][-][0-9]+")

# sitting date
sittingDate <- str_extract(number[6], "[A-Z][a-z]+, [A-Z][a-z]+ [0-9]{1,2}, [0-9]{4}")

# All the headings
# TODO: need to be matched with number
heading <- everything %>% html_nodes(".RubricTitle") %>% html_text()

# check number of questions
if(length(everything) == 0) {
  return(NULL)
}

# output
output <- data.frame(contentText,
                     supIndex,
                     sup,
                     parliamentNumber = info[3],
                     sessionNumber = info[4],
                     chamberNumber = 1, 
                     sittingNumber = info[5],
                     sittingDate = sittingDate)
output <- output %>% separate(contentText, into=c("keyID","introDate","people","content"), sep="[ ][—][ ]") 
output$keyID[output$keyID!=keyIDCheck] <- NA
output$sup <- as.character(output$sup)
for (i in 1:length(supText)){
  for (j in 1:length(output$sup)){
    if (output$sup[j] == substr(supText[i],1,1)){
      output$sup[j] <- substr(supText[i],2,nchar(supText[i]))
    } 
  }
}

View(output)



