# library -----------------------------------------------------------------
library(xts)
library(TTR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggbio)
library(raster)
library(lsa)


load("data/total_tidy.rda")

sample_total_tidy <- total_tidy %>% 
  filter(Date == "2018-12-24") %>% 
  dplyr::select(Date, Ticker, vol_x_normal, vol_y_normal)



cos_vectors <- function(x1,x2,y1,y2){
  dot.prod_x <- x1%*%x2
  dot.prod_y <- y1%*%y2
  cosin <- dot.prod_x+ dot.prod_y
}

caculate_cosine <- function(x){
  res <- cross2(x$Ticker, x$Ticker) %>% 
    do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>% 
    as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>% 
    left_join(x, by = "Ticker") %>% 
    mutate(vol_y_normal_2 = x$vol_y_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(vol_x_normal_2 = x$vol_x_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(cos_vec = cos_vectors(vol_x_normal, vol_x_normal_2, vol_y_normal, vol_y_normal_2))
  return(res)
}
