library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)

load("data/day_network_sample.rda")

data <- day_network_sample[,-1]
graph <- graph.data.frame(data, directed=F)
graph <- simplify(graph)
fc <- fastgreedy.community(graph)
V(graph)$community <- fc$membership
nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = V(graph)$community)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(graph, what="edges")[1:2]

visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
