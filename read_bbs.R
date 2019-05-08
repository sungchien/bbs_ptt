library(tidyverse)
library(httr)
library(rvest)
library(magrittr)
library(purrr)
library(lubridate)
library(stringr)
library(xml2)
library(xlsx)

getTitle <- function (x) {
  tl <- character(2)
  tl[1] <- x %>%
    html_node("a") %>%
    html_text()
  tl[2] <- x %>%
    html_node("a") %>%
    html_attr("href")
  names(tl) <- c("title", "link")
  return(tl)
}

getDate <- function (x) {
  date_data <- x %>%
    html_nodes(css=".date") %>%
    html_text() %>%
    str_trim()
  
  return(date_data)
}
getAuthor <- function (x) {
  author_data <- x %>%
    html_nodes(css=".author") %>%
    html_text() %>%
    str_trim()
  
  return(author_data)
}
checkEmpty <- function (x) {
  ifelse(is_empty(x), "", x)
}

getPostContent <- function(link, session) {
  print(link)
  post_html <- jump_to(session, link) %>%
    read_html()
  contents <- html_node(post_html, css="#main-content") %>%
    xml_contents()
  content_names <- html_name(contents)
  content_class <- html_attr(contents, "class")
  content_text <- html_text(contents)
  
  pd <- character(7)
  metalines <- content_text[grepl("^article-metaline$", html_attr(contents, "class"))]
  if (length(metalines)==3)
    pd[1:3] <- sapply(metalines, function(x) substr(x, 3, nchar(x)))
  
  pd[4] <- str_trim(paste0(
    content_text[content_names=="text"|(content_names=="span"&content_class=="hl")],
    collapse=""))
  
  pd[5] = as.character(length(which(content_class=="push")))
  
  push_label <- html_nodes(contents, ".push-tag") %>%
    html_text()
  pd[6] <- as.character(length(which(grepl("推", push_label))))

  pd[7] <- html_nodes(contents, ".push-userid") %>%
    html_text(trim=TRUE) %>%
    unique() %>%
    length() %>%
    as.character()

  names(pd) <- c("pauthor", "ptitle", "ptime", "ptext",
                 "comment_no", "push_no", "commenter_no")
  return(pd)
}

getCommenter <- function(title, link, post_date, author, id, session) {
  print(link)
  post_html <- jump_to(session, link) %>%
    read_html()
  
  comment_label <- html_nodes(post_html, ".push-tag") %>%
    html_text()
  
  pd = data.frame()
  if (length(comment_label)>0) {
    commenter_id <- html_nodes(post_html, ".push-userid") %>%
      html_text(trim=TRUE)
    
    pd = data.frame(id=id,
                    author_id=author,
                    commenter_id=commenter_id,
                    comment_label=comment_label,
                    stringsAsFactors = FALSE)
  }
  return(pd)
}

in_msg <- menu(c("一天", "一週", "一個月", "一年", "其他"), title="選擇一個時間範圍?")

x_day <- Sys.Date()
current_year <- year(x_day)
x1_day <- switch(in_msg,
              x_day-1,
              x_day-7,
              x_day-30,
              x_day-365,
              as.Date(readline("請輸入開始日期: (例如: 2019/03/25)")))

# 取得八卦版首頁資料
# set_cookie
session <- html_session("https://www.ptt.cc/bbs/Gossiping/index.html",
                        set_cookies('over18'='1'))
# 取得台劇版首頁資料
session <- html_session("https://www.ptt.cc/bbs/TaiwanDrama/index.html")

dir_html <- session %>%
  read_html()

tot_title <- data.frame()
end_loop <- FALSE

while (!end_loop) {
  post_nodes <- dir_html %>%
    html_nodes("#main-container>div") %>%
    `[[`(2) %>%
    html_nodes(xpath="./div")
  
  at <- html_attr(post_nodes, "class")
  bp <- 2 # 第一個是search-bar
  sep_pos <- which(grepl("r-list-sep", at)) #index頁有置底文
  ep <- ifelse(is_empty(sep_pos), length(post_nodes), sep_pos-1)
  
  title_data <- sapply(post_nodes, getTitle) %>%
    t() %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    slice(bp:ep)
  
  date_data <- sapply(post_nodes, getDate) %>%
    sapply(checkEmpty) %>%
    `[`(bp:ep)
  
  poss_date <- as.Date(paste0(current_year,"/", date_data))
  if (length(date_data)>1)  prev_date <- c(poss_date[2:length(date_data)], x_day)
  else prev_date <- x_day
  if (any((poss_date-prev_date)>0)) {
    l_point <- which((poss_date-prev_date)>0)
    b_date <- as.Date(paste0(current_year,"/", date_data[(l_point+1):length(date_data)]))
    current_year <- current_year-1
    t_date <- as.Date(paste0(current_year,"/", date_data[1:l_point]))
    title_data$post_date <- c(t_date, b_date)
  } else {
    title_data$post_date <- poss_date
  }
  
  title_data$author <- sapply(post_nodes, getAuthor) %>%
    sapply(checkEmpty) %>%
    `[`(bp:ep)

  if (any(title_data$post_date < x1_day)) {
    title_data %<>%
      filter(post_date >= x1_day)
    end_loop <- TRUE
  }
  
  tot_title <- rbind(title_data, tot_title)
  
  paging_nodes <- dir_html %>%
    html_nodes("#action-bar-container>div>div") %>%
    `[[`(2) %>%
    html_nodes(xpath="./a")
  
  paging_text <- paging_nodes %>%
    html_text()
  paging_links <- paging_nodes %>%
    html_attr("href")
  paging_links[grepl("上頁", paging_text)]
  
  dir_html <- jump_to(session, paging_links[grepl("上頁", paging_text)]) %>%
    read_html()
  print(paging_links[grepl("上頁", paging_text)])
}

tot_title %<>%
  filter(!is.na(title)) %>%
  mutate(id=row_number())

tot_cont <- sapply(tot_title$link, getPostContent, session) %>%
  t() %>%
  as.data.frame(stringsAsFactors=FALSE)

tot_pos <- cbind(tot_title, tot_cont)

tot_pos$ptime <- parse_date_time(substr(tot_pos$ptime, 4, nchar(tot_pos$ptime[1])), "b d H M S Y", tz="")

write.xlsx(tot_pos, "TaiwanDrama.xlsx", row.names=FALSE)

tot_commenter <- pmap_dfr(tot_title,  getCommenter, session)

write.xlsx(tot_commenter, "TaiwanDrama_commenters.xlsx", row.names=FALSE)
