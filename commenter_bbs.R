library(tidyverse)
library(magrittr)
library(purrr)
library(lubridate)
library(readxl)
library(igraph)
library(ggplot2)
library(colorspace)

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

selected_vid <- user_df %>%
  mutate(vid=row_number()) %>%
  filter(in_deg>0 & out_deg>0) %>%
  pull(vid)

active_user_g <- induced_subgraph(post_graph, selected_vid)

plot(active_user_g,
     vertex.size=10,
     vertex.label=NA,
     edge.arrow.size=0.3,
     layout = layout_with_kk)

print(paste("共有多少節點：", gorder(active_user_g)))
print(paste("共有多少連結：", gsize(active_user_g)))
print(paste("密度：", edge_density(active_user_g)))

print(paste0("此網路彼此", ifelse(is_connected(active_user_g, "weak"), "相連", "不相連")))

comp <- components(active_user_g, "weak")
print(paste("弱相連：分為多少個部分：", comp$no))
print(paste("最大的部分佔所有節點數比例：", max(comp$csize)/vcount(active_user_g)))

comp <- components(active_user_g, "strong")
print(paste("強相連：分為多少個部分：", comp$no))
print(paste("最大的部分佔所有節點數比例：", max(comp$csize)/vcount(active_user_g)))

print(paste("最長距離：", diameter(active_user_g)))
print(paste("平均距離：", mean_distance(active_user_g)))

dyad_census(active_user_g)
triad_census(active_user_g)

ei <- eigen_centrality(active_user_g)
au <- authority_score(active_user_g)
hu <- hub_score(active_user_g)
user_df <- data.frame(user_id=V(active_user_g)$name,
                      in_deg=degree(active_user_g, mode="in"),
                      out_deg=degree(active_user_g, mode="out"),
                      btw=betweenness(active_user_g),
                      eig=ei$vector,
                      aut=au$vector,
                      hub=hu$vector,
                      core=coreness(active_user_g),
                      stringsAsFactors=FALSE)

# degree centrality
user_df %>%
  ggplot() +
  geom_density(aes(x=in_deg)) +
  scale_y_continuous(limits=c(0, 0.2), breaks=seq(0, 0.2, 0.02)) +
  labs(x="指入程度", y="密度", title="指入程度分布") +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  arrange(desc(in_deg)) %>%
  mutate(rank=row_number(), cum_prob=cumsum(in_deg)/sum(in_deg)) %>%
  ggplot() +
  geom_line(aes(x=rank, y=cum_prob)) +
  scale_x_continuous(breaks=seq(0, 200, 25)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1)) +
  labs(x="指入程度序別", y="指入程度累積") +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  ggplot() +
  geom_density(aes(x=out_deg)) +
  scale_y_continuous(limits=c(0, 0.2), breaks=seq(0, 0.2, 0.02)) +
  labs(x="指出程度", y="密度", title="指出程度分布") +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))
  
user_df %>%
  arrange(desc(out_deg)) %>%
  mutate(rank=row_number(), cum_prob=cumsum(out_deg)/sum(out_deg)) %>%
  ggplot() +
  geom_line(aes(x=rank, y=cum_prob)) +
  scale_x_continuous(breaks=seq(0, 200, 25)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1)) +
  labs(x="指出程度序別", y="指出程度累積") +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  top_n(10, in_deg) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, in_deg), y=in_deg)) +
  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="指入程度", title="指入程度前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  top_n(10, out_deg) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, out_deg), y=out_deg)) +
  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="指出程度", title="指出程度前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  ggplot() +
  geom_text(aes(x=out_deg, y=in_deg, label=user_id)) +
  labs(x="指出程度", y="指入程度", title="各使用者帳號的指入與指出程度") +
  theme(panel.background=element_blank(),
        panel.grid.major=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

# betweenness centrality
user_df %>%
  top_n(10, btw) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, btw), y=btw)) +
  #  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="中介性", title="中介性前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

# eigenvector centrailty
user_df %>%
  top_n(10, eig) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, eig), y=eig)) +
  #  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="特徵中心性", title="特徵中心性前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

# authority-hub
user_df %>%
  top_n(10, aut) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, aut), y=aut)) +
  #  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="authority", title="authority score前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

user_df %>%
  top_n(10, hub) %>%
  ggplot() +
  geom_col(aes(x=reorder(user_id, hub), y=hub)) +
  #  scale_y_continuous(breaks=seq(0, 70, 10)) +
  coord_flip() +
  labs(x="使用者帳號", y="hub", title="hub score前10的使用者帳號") +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_line(color="grey80"),
        panel.grid.minor=element_blank(),
        axis.line=element_line(color="grey80"))

# coreness
spal <- choose_palette()
vcolor <- spal(12)[user_df$core]
plot(active_user_g,
     vertex.size=10,
     vertex.label=NA,
     vertex.color=vcolor,
     edge.arrow.size=0.3,
     layout = layout_with_kk)

# 