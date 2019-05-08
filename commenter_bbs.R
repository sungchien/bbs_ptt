library(tidyverse)
library(magrittr)
library(purrr)
library(lubridate)
library(readxl)
library(igraph)
library(ggplot2)

post_df <- read_excel("TaiwanDrama.xlsx") %>%
  mutate(comment_no=as.integer(comment_no),
         push_no=as.integer(push_no),
         commenter_no=as.integer(commenter_no)) %>%
  mutate(post_date=as.Date(post_date)) %>%
  mutate(rel=grepl("與惡", title)|grepl("與惡", ptext))

commenter_df <- read_excel("TaiwanDrama_commenters.xlsx")

rel_commenter_df <- commenter_df %>%
  filter(id %in% post_df$id[post_df$rel]) %>%
  group_by(id, author_id, commenter_id, comment_label) %>%
  summarise(count=n())

rel_commenter_df1 <- rel_commenter_df %>%
  group_by(id, author_id, commenter_id) %>%
  summarise(count=sum(count)) %>%
  arrange(desc(count))

rel_commenter_df2 <- rel_commenter_df1 %>%
  group_by(author_id, commenter_id) %>%
  summarise(count=n_distinct(count)) %>%
  select(commenter_id, author_id, count) %>%
  arrange(desc(count))

print(paste("發文數：", length(unique(rel_commenter_df1$id))))
print(paste("發文者人數：", length(unique(rel_commenter_df2$author_id))))
print(paste("推文者人數：", length(unique(rel_commenter_df2$commenter_id))))

# 將data frame轉換成graph，以推文者回應發文者的發文數為兩個連結間的權重
post_graph <- graph_from_data_frame(rel_commenter_df2)

# 參與的使用者帳號：Number of Vertices, vcount
print(paste("參與的使用者帳號數：", vcount(post_graph)))

# 推文者回應的發文者帳號數
out_deg <- degree(post_graph, v = V(post_graph), mode = "out",
                  loops = TRUE, normalized = FALSE)
table(out_deg)
# 推文者回應的發文者帳號數：0，只參與發文的使用者帳號
print(paste("只參與發文的使用者帳號數：", length(which(out_deg==0))))

# 發文者獲得的推文者帳號數
in_deg <- degree(post_graph, v = V(post_graph), mode = "in",
                 loops = TRUE, normalized = FALSE)
table(in_deg)
# 發文者獲得的推文者帳號數：0，只參與推文的使用者帳號
print(paste("只參與發文的使用者帳號數：", length(which(in_deg==0))))

# 同時有發文與推文的帳號數
print(paste("同時有發文與推文的帳號數：",
            vcount(post_graph)-length(which(out_deg==0))-length(which(in_deg==0))))

user_df <- data.frame(user_id= attr(V(post_graph), "names"),
                      in_deg, out_deg,
                      stringsAsFactors = FALSE)

user_df1 <- user_df %>%
  filter(in_deg>0 & out_deg>0)

med_in <- median(user_df1$in_deg)*2
med_out <- median(user_df1$out_deg)*2

user_df1 %<>%
  mutate(category=case_when(
    in_deg<=med_in & out_deg<=med_out ~ "不活躍使用者",
    in_deg<=med_in & out_deg>med_out ~ "偏向回應使用者",
    in_deg>med_in & out_deg<=med_out ~ "偏向發言使用者",
    in_deg>med_in & out_deg>med_out ~ "靈魂人物"
  ))

user_df2 <- user_df1 %>%
  filter(in_deg>250 | out_deg>50) 


user_df1 %>%
  ggplot(aes(x=out_deg, y=in_deg)) +
  geom_point(aes(color=category)) +
  geom_text(aes(x=out_deg+5, y=in_deg, label=user_id),
            data=user_df2) +
  labs(x="回應帳號數", y="受回應帳號數")

post_df %>%
  filter(rel==TRUE) %>%
  filter(author=="vk64sg13") %>%
  select(title, ptext)

post_df %>%
  filter(rel==TRUE) %>%
  mutate(author1=ifelse(author%in%c("sodabubble", "ttnakafzcm"), author, "others")) %>%
  select(author1, post_date, comment_no, commenter_no, push_no) %>%
  gather(int_type, value, -author1, -post_date) %>%
  ggplot() +
  geom_point(aes(x=post_date, y=value, color=author1), alpha=0.3) +
  scale_x_date(date_breaks="1 week") +
  facet_wrap(~int_type, nrow=3, scales="free")

is_connected(post_graph, "weak")
comp <- components(post_graph, "weak")
print(paste("分為多少個部分：", comp$no))
print(paste("最大的部分佔所有節點數比例：", max(comp$csize)/vcount(post_graph)))
