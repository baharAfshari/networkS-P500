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
