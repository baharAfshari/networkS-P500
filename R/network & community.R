library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)
library(ggplot2)
library(intergraph)
library(GGally)
library(ggnetwork)
library(ggiraph)
library(networkD3)
library(network)
load("data/day_network_sample.rda")

data <- day_network_sample[,-1]
graph <- graph.data.frame(data, directed=F)
graph <- simplify(graph)
fc <- fastgreedy.community(graph)
V(graph)$community <- fc$membership
nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = V(graph)$community)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(graph, what="edges")


visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = F)

# another way -------------------------------------------------------------
data <- data[,-3]
net_mat <- graph.edgelist(as.matrix(data), directed = F)

s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
s_day_network_sample[is.na(s_day_network_sample)] <- 0
s_day_network_sample <- as.matrix(s_day_network_sample)



net3 <- as.network(x = s_day_network_sample, # the network object
                   directed = F, # specify whether the network is directed
                   loops = FALSE, # do we allow self ties (should not allow them)
                   matrix.type = "adjacency" # the type of input
)

edge_values <- day_network_sample$cos_vecs
set.edge.value(net3,"Weight",edge_values)
summary.network(net3,print.adj = FALSE)
plot.network(net3, 
             vertex.col = "black", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net3,"Weight")) # edge width
)

# qgraph package ----------------------------------------------------------
library(tidyverse)
library(igraph)
library(graphlayouts)
library(ggraph)
library(extrafont)
loadfonts()


edges <- ("data/normal_day.csv")
edges_tbl <- map(edges,read_csv)

got_graphs <- map(1,function(x) {
  g <- graph_from_data_frame(edges_tbl[x],directed = F)
  g$title <- paste0("Season ",x)
  g
})

mutate_graph <- function(x){
  V(x)$name <- str_replace_all(V(x)$name,"\\_"," ") %>% 
    str_to_title()
  clu <- cluster_louvain(x)
  V(x)$clu <- as.character(clu$membership)
  V(x)$size <- graph.strength(x)
  x
}

got_graphs <- map(got_graphs,mutate_graph)
qgraph(got_graphs[[1]])



got_palette <- c("#1A5878","#C44237","#AD8941","#E99093",
                 "#50594B","#DE9763","#5D5F5D","#E2E9E7")

plot_graph <- function(x){
  ggraph(x,layout = "stress")+
    geom_edge_link0(aes(edge_alpha = (data$cos_vecs+1)*2),edge_colour= )+
    geom_node_point(aes(fill=clu,size=size),shape=20,col="grey25")+
    geom_node_text(aes(size=size),family = "Enchanted Land",repel=F)+
    scale_edge_width_continuous(range=c(0.1,1.5))+
    scale_size_continuous(range=c(1,8))+
    scale_fill_manual(values=got_palette)+
    theme_graph(title_family = "Enchanted Land",
                title_size = 20)+
    theme(legend.position = "none")
}

got_plot <- map(got_graphs,plot_graph)
got_plot[[1]]
ga <- graph_from_data_frame(d = edges,directed = FALSE,vertices = nodes)
ga
qgraph(ga)
ggraph(ga,"stress",bbox = 15)+
  geom_edge_link0(edge_colour = "grey66",edge_width = 0.5)+
  geom_node_point(aes(),shape = 21,size = 3)+
  geom_node_text(aes(label = name,size = degree(ga)),
                 family = "serif",repel = TRUE)+
  scale_fill_manual(values=c("F" = "#EEB422","M" = "#424242","grey66"))+
  scale_size(range=c(2,5),guide = FALSE)+
  theme_graph()+
  theme(legend.position = "bottom")

# alaki -------------------------------------------------------------------

weight <- data$cos_vecs+1
clu <- cluster_louvain(ga, weights = weight)

# define a custom color palette
got_palette <- c("#1A5878", "#C44237", "#AD8941", "#E99093", "#50594B")

ggraph(ga,layout = "stress")+
  geom_edge_link0(aes(edge_alpha= weight),edge_colour = scale_color_gradient(low="red", high="blue"))+
  geom_node_point(aes(),shape=21)+
  scale_fill_manual(values = got_palette)+
  scale_edge_width(range = c(0.2,3))+
  scale_size(range = c(1,6))+
  theme_graph()+
  theme(legend.position = "none")

# linkcomm ----------------------------------------------------------------

library(linkcomm)

lc <- getLinkCommunities(data, hcmethod = "single")

plot(lc, type = "graph", layout = layout.fruchterman.reingold)
plot(lc, type = "graph", layout = "spencer.circle")
