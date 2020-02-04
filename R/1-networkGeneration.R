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
library(purrr)
library(magrittr)


load("data/total_tidy.rda")

years <- total_tidy %>% pull(Date) %>% format("%Y") %>% unique()
years <- years[-length(years)]

dates <- as.Date(c(as.Date("2018-01-01"):as.Date("2018-01-30")))

sample_total_tidy <- total_tidy %>% 
  filter(Date %in% dates) %>% 
  dplyr::select(Date, Ticker, vol_x_normal, vol_y_normal) %>% 
  mutate(Ticker = as.character(Ticker))

x <- sample_total_tidy


cos_vectors <- function(x1,x2,y1,y2){
  dot.prod_x <- x1 * x2
  dot.prod_y <- y1 * y2
  cosin <- dot.prod_x+ dot.prod_y
  return(cosin)
}

calculate_cosine <- function(x){
  res <- cross2(x$Ticker, x$Ticker) %>% 
    do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>% 
    as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>% 
    left_join(x, by = "Ticker") %>% 
    filter(Ticker != Ticker2) %>% 
    mutate(vol_y_normal_2 = x$vol_y_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(vol_x_normal_2 = x$vol_x_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(cos_vecs = cos_vectors(vol_x_normal, vol_x_normal_2, vol_y_normal, vol_y_normal_2))
  return(res)
}


res <- calculate_cosine(sample_total_tidy)

ggplot(res, aes(res$cos_vecs))+geom_histogram()
