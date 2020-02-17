library(igraph)
library(ggnetwork)
library(tidyr)
library(tibble)
library(gplots)


# heatmap + dendogram for a normal day ------------------------------------
load('data/day_network_sample.rda')

s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
s_day_network_sample[is.na(s_day_network_sample)] <- 0

# custom palette
my_palette <- colorRampPalette(c("turquoise", "yellow", "red"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green

dist_s_sample <- dist(s_day_network_sample, method="euclidean")
dist_s_sample<- as.matrix(dist_s_sample)
heatmap.2(dist_s_sample,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='both',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 

# heatmap + dendogram for a crash day -------------------------------------


