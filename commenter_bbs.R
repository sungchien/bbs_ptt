library(tidyverse)
library(magrittr)
library(purrr)
library(lubridate)
library(readxl)
library(igraph)

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

is_connected(post_graph, "weak")
comp <- components(post_graph, "weak")
print(paste("分為多少個部分：", comp$no))
print(paste("最大的部分佔所有節點數比例：", max(comp$csize)/vcount(post_graph)))
