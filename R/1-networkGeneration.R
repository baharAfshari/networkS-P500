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


load("data/crypto/total_tidy.rda")

years <- total_tidy %>% pull(Date) %>% format("%Y") %>% unique()

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
    mutate(vel_y_normal_2 = x$vel_y_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(vel_x_normal_2 = x$vel_x_normal[match(Ticker2, x$Ticker)]) %>% 
    mutate(cos_vecs = cos_vectors(vel_x_normal, vel_x_normal_2, vel_y_normal, vel_y_normal_2))
  return(res)
}

for (year in years) {
  
  for (j in 6:12) {
    if(j == 12) {
      dates <- seq(from = as.Date(paste0(year, "-12-01")), to = as.Date(paste0(year, "-12-31")), "days")
      save_dir <- paste0("data/crypto/daily_cosinus_", year, "_", j,".rda")
    } else {
      k <- j + 1
      dates <- seq(from = as.Date(paste0(year, "-", j,"-01")), to = as.Date(paste0(year, "-", k, "-01")), "days")
      dates <- dates[-length(dates)]
      save_dir <- paste0("data/crypto/daily_cosinus_", year, "_", j,".rda")
    }
    print(dates)
    print(save_dir)
    sample_total_tidy <- total_tidy %>% 
      filter(Date %in% dates) %>% 
      dplyr::select(Date, Ticker, vel_x_normal, vel_y_normal) %>% 
      mutate(Ticker = as.character(Ticker))
    
    unique_dates <- sample_total_tidy %>% pull(Date) %>% unique()
    
    daily_res <- list()
    for (day in unique_dates){
      
      sample_day_tidy <- sample_total_tidy %>% filter(Date == day)
      
      daily_res[[as.character(as.Date(day))]] <- calculate_cosine(sample_day_tidy) %>% 
        dplyr::select(Date, Ticker, Ticker2, cos_vecs)
      
    }
    
    save(daily_res, file = save_dir)
  }
}



# daily network normal day -----------------------------------------------------------

day_network_sample <- daily_res[[1]]






