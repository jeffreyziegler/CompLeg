#####################
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c("stringr", "dplyr", "plyr", "tidytext", "tidyverse", "quanteda", "zoo", "lubridate"), pkgTest)

# working directoy
setwd("~/Documents/GitHub/CompLegFall2019/data/")

#################
# Text analysis 
#################

# load speeches data from 38th parliament
speechesDF <- read.csv("canadaTextParsing/Examples/canada_floor_speeches.csv", stringsAsFactors = F, encoding = "UTF-8")

###################################
# create function to clean speeches
# to create DTM
###################################

clean_text <- function(inputVec){
  # lowercase
  tempVec <- tolower(inputVec)
  # remove everything that is not a number or letter
  tempVec <- str_replace_all(tempVec,"[^a-zA-Z\\s]", " ")
  # make sure all spaces are just one white space
  tempVec <- str_replace_all(tempVec,"[\\s]+", " ")
  # remove blank words
  tempVec <- tempVec[which(tempVec!="")]
  #browser()
  # tokenize (split on each word)
  tempVec <- str_split(tempVec, " ")[[1]]
  # create function for removing stop words
  remove_words <- function(str, stopwords) {
    x <- unlist(strsplit(str, " "))
    x <- x[!x %in% stopwords]
    # remove single letter words
    return(x[nchar(x) > 1])
  }
  
  # remove stop words
  tempVec <- remove_words(tempVec, stopwords("english"))
  
  # get count of each word in "document"
  count_df <- data.frame(document=row, 
                         count=rle(sort(tempVec))[[1]],
                         word=rle(sort(tempVec))[[2]])
  return(count_df)
}

# create new vector that we will continuously append via rbind
# probably not the most computationally efficient way to do this...
all_words <- NULL
# loop over all rows in original DF of speeches
for(row in 1:dim(speechesDF)[1]){
  all_words <- rbind(all_words, clean_text(speechesDF[row, "paragraph_text"]))
}
# find unique words in word matrix
unique(all_words$word)
# there are 676 unique words in corpus

###################################
# create open DTM filled w/ zeroes
###################################

DTM <- matrix(0, nrow=dim(speechesDF)[1], ncol=length(unique(all_words$word)))
# assign column names of DTM to be the unique words (in alpha order)
colnames(DTM) <- unique(all_words$word)
# loop over each "document"/paragraph
for(document in 1:dim(speechesDF)[1]){
  # find all the words that are used in that paragraph
  document_subset <- all_words[which(all_words$document==document),]
  # loop over each word
  for(row in 1:dim(document_subset)[1]){
    # and check which column it's in
    DTM[document, which(colnames(DTM)==document_subset[row, "word"] )] <- all_words[row, "count"]
  }
}

###################
# distance measures
###################

# (1) squared euclidean distance function
euclidean_distance <- function(points1, points2) {
  # create empty distance matrix based on size of two vectors
  distanceMatrix <- matrix(NA, nrow = dim(points1)[1], ncol = dim(points2)[1])
  # for each row
  for(i in 1:nrow(points2)) {
    # get the distance 
    distanceMatrix[, i] <- sqrt(rowSums(t(t(points1) - points2[i, ])^2))
  }
  distanceMatrix
}

###################
# k-means
###################

# (2) define k-means algorithm
K_means <- function(x, k, n_iter) {
  # create empty vectorized lists based on the size of iterations
  clusterHistory <- vector(n_iter, mode="list")
  centerHistory <- vector(n_iter, mode="list")
  # generate starting centers
  # sample some centers, 5 for example
  centers <- x[sample(nrow(x), k),]
  
  # iterate for set number of times
  for(i in 1:n_iter) {
    # calculate distance of each word from center
    distances_to_centers <- euclidean_distance(x, centers)
    # assign to closest cluster (min)
    clusters <- apply(distances_to_centers, 1, which.min)
    # reupdate where the center of the cluster is
    centers <- apply(x, 2, tapply, clusters, mean)
    # save each clust and center for next step
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  # return list of clusters and centers
  list(clusters=clusterHistory, centers=centerHistory)
}

# execute function
our_output <- K_means(DTM, k=2, n_iter=10)
# compare to the "real-thing"
kmeans_output <- kmeans(DTM, centers = 2, iter.max = 10, nstart = 1)
print(kmeans_output)