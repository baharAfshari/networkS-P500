library(tidyr)
library(dplyr)
library(ggplot2)
library(lsa)
library(purrr)
library(magrittr)
library(tibble)


load("data/daily_cosinus_2018.rda")
load("data/daily_cosinus_lag1_2018.rda")


for (day in 1:length(daily_res_lag1)) {
  lagday_network_sample <- daily_res_lag1[[day]]
  lag_day_network_sample <- spread(lagday_network_sample[,-1], Ticker2, cos_vecs)
  lag_day_network_sample <- as.matrix(column_to_rownames(lag_day_network_sample, var = "Ticker"))
  sym_lag_1day <- pmax(lag_day_network_sample, t(lag_day_network_sample))
  
  
  
}


# plot --------------------------------------------------------------------

# custom palette
my_palette <- colorRampPalette(c("red", "purple", "blue"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green

heatmap.2(sym_lag_1day,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='none',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 

