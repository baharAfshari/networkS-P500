# library -----------------------------------------------------------------
library(xts)
library(TTR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(raster)
library(lsa)
library(purrr)
library(magrittr)
library(igraph)
library(ggnetwork)
library(tidyr)
library(tibble)
library(gplots)
library(gridExtra)

# loading data ------------------------------------------------------------
load("data/crypto/daily_cosinus_2018.rda")
load("data/crypto/total_tidy.rda")


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
  
  dates <- seq(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), "days")
  
  sample_total_tidy <- total_tidy %>% 
    filter(Date %in% dates) %>% 
    dplyr::select(Date, Ticker, vol_x_normal, vol_y_normal) %>% 
    mutate(Ticker = as.character(Ticker))
  
  unique_dates <- sample_total_tidy %>% pull(Date) %>% unique()
  n <- length(unique_dates)
  daily_res_lag1 <- list()
  for (i in 2:n){
    
    day1 <- unique_dates[i-1]
    day2 <- unique_dates[i]
    
    sample_day1_tidy <- sample_total_tidy %>% filter(Date == day1)
    sample_day2_tidy <- sample_total_tidy %>% filter(Date == day2)
    
    daily_res_lag1[[as.character(as.Date(day2))]] <- calculate_cosine(sample_day1_tidy, sample_day2_tidy) %>% 
      dplyr::select(Date, Ticker, Ticker2, cos_vecs)
    
  }
  
  save_dir <- paste0("data/crypto/daily_cosinus_lag1_", year, ".rda")
  save(daily_res_lag1, file = save_dir)
}


# heatmap + dendogram for a normal day ------------------------------------
day_network_sample <- daily_res[[1]]
s_day_network_sample <- spread(day_network_sample[,-1], Ticker2, cos_vecs)
s_day_network_sample <- column_to_rownames(s_day_network_sample, var = "Ticker")
s_day_network_sample[is.na(s_day_network_sample)] <- 0
s_day_network_sample <- s_day_network_sample+ t(s_day_network_sample)
s_day_network_sample <- as.matrix(s_day_network_sample)

# custom palette
my_palette <- colorRampPalette(c("red", "purple", "blue"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green

heatmap.2(s_day_network_sample,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='none',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 

# lag_day heatmap ---------------------------------------------------------

lagday_network_sample <- daily_res_lag1[["2018-12-20"]]
lag_day_network_sample <- spread(lagday_network_sample[,-1], Ticker2, cos_vecs)
lag_day_network_sample <- column_to_rownames(lag_day_network_sample, var = "Ticker")
lag_day_network_sample <- as.matrix(lag_day_network_sample)

heatmap.2(lag_day_network_sample,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='none',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 

# lag_day 2 heatmap ---------------------------------------------------------

lagday_network_sample <- daily_res_lag1[["2018-12-24"]]
lag_day_network_sample <- spread(lagday_network_sample[,-1], Ticker2, cos_vecs)
lag_day_network_sample <- column_to_rownames(lag_day_network_sample, var = "Ticker")
lag_day_network_sample <- as.matrix(lag_day_network_sample)

heatmap.2(lag_day_network_sample,
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='none',     
          Rowv=T,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 





