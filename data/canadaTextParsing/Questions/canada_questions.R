# libraries
library(stringr)

# parameters
parliament <- 42
session <- 1
sittings <- 438

#####################
# download html files
#####################

# will probably need to figure out a way to handle errors
# because some sittings might not have a order/notice paper page

# loop through sittings
for(i in 1:sittings) {
  #browser()
  # set working directory
  setwd("~/Documents/GitHub/compLegFall2019/data/canadaTextParsing/Questions/")

  # make URL
  url <- str_c("https://www.ourcommons.ca/DocumentViewer/en/", parliament, "-", session, "/house/sitting-", i, "/order-notice/page-ToC", collapse = "")
  
  # make file name
  file <- str_c("table_of_contents_", parliament, "_", session, "_", i, ".html", collapse = "")
  
    # download file
  download.file(url, file, quiet = TRUE)
  
  # random delay
  Sys.sleep(runif(1, 0, 0.25))
  
  # update progress bar
  setTxtProgressBar(pb, i)
}