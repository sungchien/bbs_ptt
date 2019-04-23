library(tidyverse)
library(httr)
library(rvest)

in_msg <- menu(c("一天", "一週", "一個月", "一年"), title="選擇一個時間範圍?")

x <- Sys.Date()

x1 <- switch(in_msg,
              x-1,
              x-7,
              x-30,
              x-365)

# 取得八卦版首頁資料
# set_cookie
session <- html_session("https://www.ptt.cc/bbs/Gossiping/index.html",
                        set_cookies('over18'='1'))

session <- html_session("https://www.ptt.cc/bbs/TaiwanDrama/index.html")

post_nodes <- session %>%
  read_html() %>%
  html_nodes("#main-container>div") %>%
  `[[`(2) %>%
  html_nodes(xpath="./div")

at <- html_attr(post_nodes, "class")
sep_pos <- which(grepl("r-list-sep", at))

getTitle <- function (x) {
  tl <- character(2)
  tl[1] <- x %>%
    html_node("a") %>%
    html_text()
  tl[2] <- x %>%
    html_node("a") %>%
    html_attr("href")
  return(tl)
}
bp <- 2 # 第一個是search-bar
ep <- sep_pos-1
title_data <- sapply(post_nodes, getTitle) %>%
  t()
getDate <- function (x) {
  date_data <- x %>%
    html_nodes(css=".date") %>%
    html_text()
  return(date_data)
}
checkDate <- function (x) {
  ifelse(purrr::is_empty(x), "", x)
}
date_data <- sapply(post_nodes, getDate) %>%
  sapply(checkDate)
