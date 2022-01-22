library(tidyr)
library(dplyr)
library(igraph)

load("data/crypto/multilag/valid_relations3_2018.rda")
load("data/crypto/multilag/daily_cosinus_multilag3_2018.rda")


leader_follower <- valid_relations %>% dplyr::select(Ticker2 , Ticker)

g1 <- graph_from_data_frame(d=leader_follower,  directed=T) 
plot(g1,edge.color= "black", vertex.size	= 8, edge.arrow.size	= 0.5)
V(g1)$color <- "red"




day_multi <- daily_res_multilag[["2018-12-24"]]
daily_multi_filter <- day_multi %>% 
  filter(Ticker != Ticker2) %>% 
  filter(abs(cos_vecs) > 0.9999995) %>% 
  select(Ticker, Ticker2)
  
  
g2 <- graph_from_data_frame(d= daily_multi_filter, directed= T)
V(g2)$color <- "blue"

v1 <- V(g1)$name
v2 <- V(g2)$name
V(g2)[v2 %in% v1]$color <- "red"
# V(g2)$size <- degree(g2) * 2

par(mfrow=c(1, 2))
par(oma=c(2,0,2,0))
plot(g2, vertex.size= 2, edge.arrow.size= 0.5, vertex.label.font = 0.001)
plot(g1, vertex.size= 2, edge.arrow.size= 0.5, vertex.label.font = 0.001)





