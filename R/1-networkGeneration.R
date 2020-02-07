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


load("data/total_tidy.rda")

years <- total_tidy %>% pull(Date) %>% format("%Y") %>% unique()
years <- years[-length(years)]

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

for (year in years) {
  
  dates <- seq(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), "days")
  
  sample_total_tidy <- total_tidy %>% 
    filter(Date %in% dates) %>% 
    dplyr::select(Date, Ticker, vol_x_normal, vol_y_normal) %>% 
    mutate(Ticker = as.character(Ticker))
  
  unique_dates <- sample_total_tidy %>% pull(Date) %>% unique()
  
  daily_res <- list()
  for (day in unique_dates){
    
    sample_day_tidy <- sample_total_tidy %>% filter(Date == day)
    
    daily_res[[as.character(as.Date(day))]] <- calculate_cosine(sample_day_tidy) %>% 
      dplyr::select(Date, Ticker, Ticker2, cos_vecs)
    
  }
  
  save_dir <- paste0("data/daily_cosinus_", year, ".rda")
  save(daily_res, file = save_dir)
}



# daily network -----------------------------------------------------------

day_network_sample <- daily_res[[1]]






