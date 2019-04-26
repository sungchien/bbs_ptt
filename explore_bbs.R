library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(ggplot2)

post_df <- read_excel("TaiwanDrama.xlsx")

post_df %<>%
  mutate(comment_no=as.integer(comment_no),
         push_no=as.integer(push_no),
         commenter_no=as.integer(commenter_no)) %>%
  mutate(post_date=as.Date(post_date)) %>%
  mutate(keywd_in_ti=grepl("與惡", title),
         keywd_in_tx=grepl("與惡", ptext))

# 相關文章數
print(paste("資料庫內總文章數", nrow(post_df)))
print(paste("標題出現「與惡」的文章數", length(which(post_df$keywd_in_ti))))
print(paste("內文出現「與惡」的文章數", length(which(post_df$keywd_in_tx))))
print(paste("標題和內文都出現「與惡」的文章數", length(which(post_df$keywd_in_ti&post_df$keywd_in_tx))))

# 統計每天的發文數
post_df %>%
  group_by(post_date) %>%
  summarise(post_c=n(), rel_post_c=sum(keywd_in_ti|keywd_in_tx)) %>%
  mutate(ir_post_c=post_c-rel_post_c) %>%
  select(-post_c) %>%
  gather(key="post_type", value="value", -post_date) %>%
  mutate(post_type=factor(post_type, levels=c("rel_post_c", "ir_post_c"), labels=c("與惡", "其他"))) %>%
  ggplot(aes(x=post_date, y=value, group=post_type, fill=post_type)) +
  geom_area() +
  scale_x_date(date_breaks="1 week") +
  scale_fill_viridis_d()

# 作者發文數分布
post_df %>%
  filter(keywd_in_ti|keywd_in_tx) %>%
  group_by(author) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
