# define parser function
parse_HTML <- function(file) {

  # sittings
  info <- unlist(str_split(file, "_|\\."))
  parliament <- info[5]
  session <- info[6]
  sitting <- info[7]
  #browser()
  
  # read in HTML
  html <- read_html(file)
  
  # heading
  heading <- html %>% html_nodes(".Item:nth-child(2) div") %>% html_text()
  sitting_date <- str_extract(heading, "[A-Z][a-z]+\\s[0-9]{1,2}, [0-9]{4}") %>%
    mdy() %>%
    str_replace_all("-","/")
  
  # text
  text <- html %>% html_nodes(".ItemPara") %>% html_text() %>%
    str_replace_all("(Q-[0-9]+)(2)","\\1")
  
  # number
  number <- html %>% html_nodes(".ItemPara b") %>% html_text() %>%
    str_replace_all("(Q-[0-9]+)(2)","\\1")
  
  # date
  notice_date <- text %>%
    str_extract("[A-Z][a-z]+\\s[0-9]{1,2}, [0-9]{4}") %>%
    mdy() %>%
    str_replace_all("-","/")
  
  # member
  member_text <- html %>% html_nodes("b+ .parldata-widget-popup") %>% html_text()
  constituency <- member_text %>%
    str_replace("(.+\\()(.+)(\\))","\\2")
  member_last <- member_text %>%
    str_replace("(M.\\.\\s)(.+)(\\s\\(.+\\))","\\2")
  
  # member_attr <- html %>% html_nodes(".parldata-widget-popup") %>% html_attrs()
  # member <- 1:length(member_attr)
  # constituency <- 1:length(member_attr)
  # for (i in 1:length(member_attr)) {
  #   link <- paste0("https:",member_attr[[i]][2])
  #   member[i] <- link %>% read_html() %>% html_nodes(".mpname") %>% html_text()
  #   constituency[i] <- link %>% read_html() %>% html_node(".mparea") %>% html_text()
  # }

  # check number of questions
  if(length(text) == 0) {
    return(NULL)
  }
  
  # make a data frame
  out <- data.frame(parliament_ID = info[3],
                    session_ID = paste0(info[3],"-", info[4]),
                    sitting_ID = paste0(info[3],"-", info[4],"-",info[5]),
                    item_ID = paste0(info[3],"-", info[4],"-",number),
                    sitting_date = sitting_date,
                    item_number = number,
                    member_text = member_text,
                    text = text,
                    notice_date = notice_date,
                    constituency = constituency,
                    member_last = member_last,
                    stringsAsFactors = FALSE)
  # return data frame
  return(out)
}

# read in html files
files <- list.files(path="../Questionsurl", pattern="*.html", full.names=TRUE, recursive=FALSE)

# parse html
out <- alply(.data = files, .margins = 1, .fun = parse_HTML, .progress = "text", .inform = TRUE)

# stack data frames
out <- do.call("rbind", out)

# add key ID
questions <- data.frame(key_ID = 1:nrow(out), arrange(out,sitting_date))