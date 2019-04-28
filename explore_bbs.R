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
  scale_fill_viridis_d() +
  labs(x="日期", y="發文篇數", fill="發文類型", title="台劇板發文數量統計") +
  theme(panel.background = element_rect(fill="white"),
        axis.text.x=element_text(angle=60, hjust=1))

# 作者發文數分布
author_rank <- post_df %>%
  filter(keywd_in_ti|keywd_in_tx) %>%
  group_by(author) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(author=factor(author, levels=author)) %>%
  mutate(rank=row_number(desc(count))) %>%
  mutate(cum_count=cumsum(count)) %>%
  mutate(prob=count/sum(count), cum_prob=cum_count/sum(count))

author_rank %>%
  filter(prob>=prob[10]) %>%
  ggplot(aes(x=reorder(author, prob), y=prob)) +
  geom_col(fill="navy") +
  coord_flip() +
  labs(x="發文者", y="發文佔總體比例", title="高發文者發文數佔總體比例") +
  theme(panel.background = element_rect(fill="white"))

author_rank %>%
  ggplot(aes(x=rank, y=cum_prob)) +
  geom_line()

# 依照發布期間長短排列作者
post_df %>%
  filter(keywd_in_ti|keywd_in_tx) %>%
  group_by(author) %>%
  summarise(first_post=min(post_date),
            last_post=max(post_date),
            total_count=n()) %>%
  mutate(duration=last_post-first_post) %>%
  count(duration, sort=TRUE)

post_df %>%
  filter(keywd_in_ti|keywd_in_tx) %>%
  group_by(author) %>%
  summarise(first_post=min(post_date),
            last_post=max(post_date),
            total_count=n()) %>%
  filter(total_count>5) %>%
  mutate(duration=last_post-first_post) %>%
  arrange(duration) %>%
  mutate(author=factor(author, levels=author)) %>%
  mutate(label_pos=first_post+duration/2) %>%
  arrange(desc(duration)) %>%
  ggplot() +
  geom_rect(aes(xmin=first_post, xmax=last_post,
                ymin=as.numeric(author)-0.3, ymax=as.numeric(author)+0.3,
                fill=author, alpha=total_count)) +
  geom_text(aes(x=label_pos, y=as.numeric(author),
                label=paste0(as.character(author), "發文", total_count))) +
  scale_x_date(date_breaks="1 week") +
  scale_fill_brewer(palette="Set2") +
  scale_alpha_continuous(range=c(0.6, 1.0)) +
  theme(legend.position="none")
  
post_df %>%
  gather(key="stat_type", value="value", comment_no, push_no, commenter_no) %>%
  mutate(post_type=ifelse(keywd_in_ti|keywd_in_tx, "與惡", "其他")) %>%
  ggplot(aes(x=post_date, y=value, color=post_type)) +
  geom_point() +
  scale_x_date(date_breaks="1 week") +
  scale_color_manual(values=c("red", "blue")) +
  facet_wrap(~stat_type, nrow=3, scales="free_y")
