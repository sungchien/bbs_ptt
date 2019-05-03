library(readxl)
library(magrittr)
library(tidyverse)
library(lubridate)
library(jiebaR)
#文字處理套件
library(tm)
#疏鬆矩陣運算套件
library(slam)
library(stm)

post_df <- read_excel("TaiwanDrama.xlsx") %>%
  mutate(keywd_in_ti=grepl("與惡", title),
         keywd_in_tx=grepl("與惡", ptext)) %>%
  mutate(rel=keywd_in_ti|keywd_in_tx) %>%
  mutate(date_info=as.Date(post_date)-as.Date("2019/03/10")) %>%
  mutate(id=row_number())             # 每則發文加上編號(id)

# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="TaiwanDrama.dict", bylines=TRUE)

# 製作過濾去不包含中文字或只有一個中文字之候選詞語的函數
filterChineseTerms <- function (str_text) {
  str_text <- unlist(str_text)
  str_text <- str_text[grepl("\\p{Han}+", str_text, perl=TRUE)]
  str_text <- str_text[nchar(str_text)>1]
  paste(str_text, collapse=" ")
}

# 將新聞內容斷詞
post_df %<>%
  rowwise() %>%                           # 逐行運算
  mutate(words=segment(paste0(title, ptext, sep="。"), mp.seg)) %>% # 斷詞
  mutate(words=filterChineseTerms(words)) %>% # 過濾去不包含中文字的候選詞語
  ungroup() %>%
  select(id, rel, date_info, title, words)          # 選擇發文編號和斷詞結果

#建立語料庫
vc = VCorpus(VectorSource(post_df$words))

#將形式為character string的句子依據詞語之間的空白轉為vector，vector上的單位為詞語
strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))

#建立文件-詞語矩陣，每個元素為每一種詞語出現在一筆文件上的次數
dtm = DocumentTermMatrix(vc,
                         control=list(tokenize=strsplit_space_tokenizer, wordLengths=c(1, Inf)))

term.rel_count = col_sums(dtm[post_df$rel, ])

term.rel_df = col_sums(dtm[post_df$rel, ]>0)/nrow(post_df[post_df$rel, ])
term.ir_df = col_sums(dtm[!post_df$rel, ]>0)/nrow(post_df[!post_df$rel, ])
term.pr_diff = (term.rel_df-term.ir_df)/term.rel_df

cr1 <- term.rel_count>5
length(which(cr1))
cr2 <- term.pr_diff>0.8
length(which(cr2))
length(which(cr1 & cr2))

dtm1 <- dtm[, cr1 & cr2]

dtm1 <- dtm1[post_df$rel, ]
doc.termno = row_sums(dtm1)
length(which(doc.termno<=10))
dtm1 <- dtm1[doc.termno>10, ]

out <- list()
out$documents <- lapply(1:dtm1$nrow, function (x) matrix(as.integer(c(dtm1$j[dtm1$i==x], dtm1$v[dtm1$i==x])), nrow=2, byrow=TRUE))
out$vocab <- dtm1$dimnames$Terms
out$meta <- post_df[dtm1$dimnames$Docs, ]

stora <- searchK(documents=out$documents, vocab=out$vocab,
                 K=seq(5, 50, 5), prevalence=~s(date_info),
                 data=out$meta)
plot.searchK(stora)

stora <- searchK(documents=out$documents, vocab=out$vocab,
                 K=0, prevalence=~s(date_info),
                 data=out$meta)
postFit <- stm(documents=out$documents, vocab=out$vocab,
               K=5, prevalence=~s(date_info),
               max.em.its=75, data=out$meta,
               init.type="Spectral")
x <- labelTopics(postFit, n=10)
write.xlsx(x$frex, "commjour_20.xlsx")
write.xlsx(x$score, "commjour_20_score.xlsx")

y <- findThoughts(postFit, texts=out$meta$title, n=5)

