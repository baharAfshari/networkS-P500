library(igraph)

load("data/multilag/valid_relations3_2018.rda")


leader_follower <- valid_relations %>% dplyr::select(Ticker2 , Ticker)

net <- graph_from_data_frame(d=leader_follower,  directed=T) 
plot(net,edge.color= "black", vertex.size	= 8, edge.arrow.size	= 0.5)
