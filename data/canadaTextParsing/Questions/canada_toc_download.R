# loop through sittings
for(i in 1:sittings) {
  
  # make URL
  url <- str_c("https://www.ourcommons.ca/DocumentViewer/en/", parliament, "-", session, "/house/sitting-", i, "/order-notice/page-ToC", collapse = "")
  
  # make file name
  file <- str_c("table_of_contents_", parliament, "_", session, "_", i, ".html", collapse = "")
  
  # download file
  tryCatch(download.file(url, file, quiet = TRUE), 
           error = function(e) print(paste(file, 'questions missing')))
  
  # random delay
  Sys.sleep(runif(1, 0, 0.25))
}

# read in table of contents files
tables <- list.files(pattern="*.html", full.names=TRUE, recursive=FALSE)

# download questions
for (i in 1:length(tables)) {
  # read table of contents
  list <- read_html(tables[i]) %>% html_nodes(".Link") %>% html_text()
  attr <- read_html(tables[i]) %>% html_nodes(".Link") %>% html_attrs()
  loc <- match("Questions",list)
  
  # make url
  name <- unlist(str_split(tables[i], "_|\\."))
  url <- paste0("https://www.ourcommons.ca",attr[[loc]][2])
  #browser()
  # make file name
  filename <- str_c("../Questionsurl/questions_", name[5], "_", name[6], "_", name[7], ".html", collapse = "")
  
  # download file
  if (!is.na(url)&!is.na(loc)) {
    download.file(url, filename, quiet = TRUE)
  }
  
  # random delay
  Sys.sleep(runif(1, 0, 0.25))
}