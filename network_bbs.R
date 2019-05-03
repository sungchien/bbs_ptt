library(tidyverse)
library(jiebaR)
library(wordcloud)
library(readxl)
library(igraph)

post_df <- read_excel("TaiwanDrama.xlsx") %>%
  mutate(keywd_in_ti=grepl("與惡", title),
         keywd_in_tx=grepl("與惡", ptext)) %>%
  mutate(rel=keywd_in_ti|keywd_in_tx)

# 設定jieba斷詞器
mp.seg <- worker(type="mp", user="TaiwanDrama.dict", bylines=TRUE)

# 將發文內容斷詞
post_df %<>%
  mutate(id=row_number()) %>%             # 每則發文加上編號(id)
  mutate(word=segment(ptext, mp.seg)) %>% # 斷詞
  select(id, rel, word) %>%               # 選擇發文編號和斷詞結果
  unnest(word) %>%                        # 將斷詞結果展開
  filter(grepl("\\p{Han}+", word, perl=TRUE)) %>% # 保留至少一個中文字的詞語
  filter(nchar(word)>1)

word.info <- post_df %>%
  mutate(total.doc = n_distinct(id)) %>%
  group_by(word, total.doc) %>%           # 統計詞語出現文件數
  summarise(count=n(), doc=n_distinct(id)) %>%
  ungroup()

word.rel <- post_df %>%
  filter(rel) %>%
  mutate(rel_total.doc = n_distinct(id)) %>%
  group_by(word, rel_total.doc) %>%           # 統計詞語出現文件數
  summarise(rel_count=n(), rel_doc=n_distinct(id)) %>%
  ungroup()

word.info %>%
  left_join(word.rel) %>%
  filter(count>10) %>%
  mutate(ir_total.doc=total.doc-rel_total.doc,
         ir_doc=(doc-rel_doc)/ir_total.doc,
         rel_doc=rel_doc/rel_total.doc) %>%
  ggplot(aes(x=rel_doc, y=ir_doc)) +
  geom_point()

keyword <- word.info %>%
  left_join(word.rel) %>%
  filter(count>10) %>%
  mutate(ir_total.doc=total.doc-rel_total.doc,
         ir_doc=(doc-rel_doc)/ir_total.doc,
         rel_doc=rel_doc/rel_total.doc) %>%
  mutate(pr_diff=(rel_doc-ir_doc)/rel_doc) %>%
  filter(!is.na(rel_count)) %>%
  filter(pr_diff>0.8) %>%
  top_n(100, rel_count) %>%
  arrange(desc(rel_count))

# 找出每則新聞中出現的重要詞語
keyword.post <- post_df %>%
  filter(rel) %>%             # 去除發文中tfidf過低的詞語
  group_by(id, word) %>%
  summarise(count=n()) %>%
  semi_join(keyword, by="word") %>%  # 以重要詞語比對每個發文內容
  arrange(id)

# 統計重要詞語出現的新聞數量
kw_docs <- keyword.post %>%
  group_by(word) %>%              # 統計重要詞語出現的文件數
  summarise(c=n_distinct(id)) %>%
  ungroup() %>%
  arrange(desc(c))

# 計算重要詞語共同出現的新聞數量
kw_codocs <- keyword.post %>%
  inner_join(keyword.post, by=c("id")) %>% # 找出每一則新聞共同出現的重要詞語
  group_by(word.x, word.y) %>%             # 統計每一對重要詞語共同出現的新聞數量
  summarise(dxy=n()) %>%
  arrange(desc(dxy)) %>%                   # 按照共同出現的新聞數排序
  ungroup() %>%
  filter(word.x != word.y)                 # 刪除相同的詞語

# 加上重要詞語出現的新聞數量
kw_codocs <- kw_codocs %>%
  left_join(kw_docs, by=c("word.x"="word")) %>%
  rename(dx=c) %>%
  left_join(kw_docs, by=c("word.y"="word")) %>%
  rename(dy=c)

#########
# 以Jaccard similarity和Correlation Coefficient兩種方式計算詞語共同出現的相關性

# Jaccard similarity
jaccardSimilarity <- function(dx, dy, dxy) {
  dx <- as.numeric(dx)
  dy <- as.numeric(dy)
  dxy <- as.numeric(dxy)
  return(dxy/(dx+dy-dxy))
}

# 計算每對重要詞語的Jaccard Similarity
word_net.js <- kw_codocs %>%
  rowwise() %>%
  mutate(js=jaccardSimilarity(dx, dy, dxy)) %>%
  ungroup() %>%
  arrange(desc(js))

# 刪減較不重要共現資訊
word_net.js <- word_net.js %>%
  mutate(pr=percent_rank(js)) %>%           # 根據jc進行百分比排序(由小到大)
  filter(pr>0.75) %>%                       # 保留後1/4
  select(from=word.x, to=word.y, weight=js)

########
# 將重要詞語與其共現資訊表示成網路圖

# 將資料轉成網路
wg.js <- graph_from_data_frame(word_net.js, directed=FALSE)

# 將節點之間的線合併
wg.js <- simplify(wg.js, edge.attr.comb = list("mean"))

# 根據節點分群的結果為各節點設定顏色
cl.js <- cluster_louvain(wg.js)
cl.js.mem <-  membership(cl.js)
mem.no <- length(unique(cl.js.mem))

# 顏色
vertex.color <- brewer.pal(mem.no, "Dark2")[cl.js.mem]

# 計算各節點在圖形上的座標
coords.js <- layout_(wg.js, with_graphopt())
coords.js <- layout_(wg.js, with_fr())

plot(wg.js, vertex.shape="none",
     vertex.label.cex=0.8, vertex.label.color=vertex.color,
     layout=coords.js)

# 畫圖
png(file="graph_js.png", width=800, height=600)
plot(x=cl.js, y=wg.js, vertex.shape="none",
     vertex.label.cex=0.8, edge.lty="blank", layout=coords.js)
dev.off()

# 查看各分群(主題)內的詞語
cl.js.mem <- membership(cl.js)
for (i in seq(max(cl.js.mem))) {
  print(paste("Cluster", i))
  print(V(wg.js)$name[cl.js.mem==i])
}

# correlation coefficient
phiCoefficient <- function(d, dx, dy, dxy) {
  d <- as.numeric(d)
  dx <- as.numeric(dx)
  dy <- as.numeric(dy)
  dxy <- as.numeric(dxy)
  d.not.x <- d - dx
  d.not.y <- d - dy
  dx.not.y <- dx - dxy
  dy.not.x <- dy - dxy
  d.not.x.not.y <- d.not.y - dx.not.y
  return((dxy*d.not.x.not.y-dx.not.y*dy.not.x)/sqrt(dx*dy*d.not.x*d.not.y))
}

# 所有的關鍵詞語共出現在多少則新聞(d)
d <- keyword.post %>%
  distinct(id) %>%
  nrow()

# 計算每個詞語的Correlation Coefficient
word_net.cc <- kw_codocs %>%
  rowwise() %>%
  mutate(cc=phiCoefficient(d, dx, dy, dxy)) %>%
  ungroup()

# 刪減較不重要共現資訊的網路圖
word_net.cc <- word_net.cc %>%
  mutate(pr=percent_rank(cc)) %>%
  filter(pr>0.75) %>%
  select(from=word.x, to=word.y, weight=cc)

# 將資料轉成網路
wg.cc <- graph_from_data_frame(word_net.cc, directed=FALSE)

# 將節點之間的線合併
wg.cc <- simplify(wg.cc, edge.attr.comb = list("mean"))

# 對節點分群
cl.cc <- cluster_louvain(wg.cc)

# 計算各節點在圖形上的座標
coords.cc <- layout_(wg.cc, with_graphopt())

# 畫圖
png(file="graph_cc.png", width=800, height=600)
plot(x=cl.cc, y=wg.cc, vertex.shape="none",
     vertex.label.cex=0.8, edge.lty="blank", layout=coords.cc)
dev.off()

cl.cc.mem <- membership(cl.cc)
for (i in seq(max(cl.cc.mem))) {
  print(paste("Cluster", i))
  print(V(wg.cc)$name[cl.cc.mem==i])
}
