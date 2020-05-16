# library -----------------------------------------------------------------
library(tidyr)
library(dplyr)
library(lsa)
library(purrr)
library(magrittr)
library(tibble)
library(gplots)

# loading data ------------------------------------------------------------
load("data/total_tidy.rda")


pmax.abs <- function(x, y) {
  z <- y
  x.bigger <- (abs(x) > abs(y))
  z[x.bigger] <- x [x.bigger]
  return(z)
}

cos_vectors <- function(x1,x2,y1,y2){
  dot.prod_x <- x1 * x2
  dot.prod_y <- y1 * y2
  cosin <- dot.prod_x+ dot.prod_y
  return(cosin)
}

calculate_cosine <- function(x, y){
  res <- cross2(x$Ticker, x$Ticker) %>% 
    do.call(what = rbind) %>% set_colnames(c("Ticker", "Ticker2")) %>% 
    as_tibble() %>% mutate(Ticker = as.character(Ticker), Ticker2 = as.character(Ticker2)) %>% 
    left_join(x, by = "Ticker") %>% 
    mutate(vol_y_normal_2 = y$vol_y_normal[match(Ticker2, y$Ticker)]) %>% 
    mutate(vol_x_normal_2 = y$vol_x_normal[match(Ticker2, y$Ticker)]) %>% 
    mutate(cos_vecs = cos_vectors(vol_x_normal, vol_x_normal_2, vol_y_normal, vol_y_normal_2))
  return(res)
}

for (year in years) {
  
  load_dir <- paste0("data/multilag/daily_cosinus_multilag3_", year, ".rda")
  load(load_dir)
  
  multilags_dt <- bind_rows(daily_res_multilag)
  
  relation_counts <- multilags_dt %>% select(Ticker, Ticker2, first, second) %>% 
    mutate(relation = paste0(first, second)) %>% 
    select(Ticker, Ticker2, relation) %>% 
    group_by(Ticker, Ticker2, relation) %>% 
    summarise(coun_relation = n())
  
  valid_relations <- relation_counts %>% 
    group_by(Ticker, Ticker2) %>% 
    filter(Ticker != Ticker2) %>% 
    mutate(open_days = sum(coun_relation)) %>% 
    filter(coun_relation > open_days / 2)
  
  save_dir <- paste0("data/multilag/valid_relations3_", year, ".rda")
  save(valid_relations, file = save_dir)
}