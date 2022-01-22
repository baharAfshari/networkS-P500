# library -----------------------------------------------------------------
library(tidyr)
library(dplyr)
library(lsa)
library(purrr)
library(magrittr)
library(tibble)
library(gplots)

# loading data ------------------------------------------------------------
load("data/crypto/total_tidy.rda")


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
  
  dates <- seq(from = as.Date(paste0(year, "-01-01")), to = as.Date(paste0(year, "-12-31")), "days")
  
  sample_total_tidy <- total_tidy %>% 
    filter(Date %in% dates) %>% 
    dplyr::select(Date, Ticker, vol_x_normal, vol_y_normal) %>% 
    mutate(Ticker = as.character(Ticker))
  
  unique_dates <- sample_total_tidy %>% pull(Date) %>% unique()
  n <- length(unique_dates)
  daily_res_multilag <- list()
  for (i in 3:n){
    
    day1 <- unique_dates[i-2]
    day2 <- unique_dates[i-1]
    day3 <- unique_dates[i]

    
    sample_day1_tidy <- sample_total_tidy %>% filter(Date == day1)
    sample_day2_tidy <- sample_total_tidy %>% filter(Date == day2)
    sample_day3_tidy <- sample_total_tidy %>% filter(Date == day3)

    
    day3_day1 <- calculate_cosine(sample_day3_tidy, sample_day1_tidy) %>% 
      dplyr::select(Date, Ticker, Ticker2, cos_vecs) %>% mutate(first = 3, second = 1)
    day3_day2 <- calculate_cosine(sample_day3_tidy, sample_day2_tidy) %>% 
      dplyr::select(Date, Ticker, Ticker2, cos_vecs) %>% mutate(first = 3, second = 2)
    day3_day3 <- calculate_cosine(sample_day3_tidy, sample_day3_tidy) %>% 
      dplyr::select(Date, Ticker, Ticker2, cos_vecs) %>% mutate(first = 3, second = 3)
  

    daily_res_multilag[[as.character(as.Date(day2))]] <- rbind(day3_day1, day3_day2, day3_day3) %>%
      group_by(Ticker, Ticker2) %>% top_n(1, abs(cos_vecs))
    
  }
  
  save_dir <- paste0("data/crypto/multilag/daily_cosinus_multilag3_", year, ".rda")
  save(daily_res_multilag, file = save_dir)
}



# load("data/crypto/multilag/daily_cosinus_multilag_2018.rda")

for (year in years) {
  
  dist_in <- paste0("data/crypto/multilag/daily_cosinus_multilag3_", year, ".rda")
  load(dist_in)
  multilags <- list()
  
  for (day in 1:length(daily_res_multilag)) {
    lagday_network_sample <- daily_res_multilag[[day]]
    lag_daya_network_sample <- spread(lagday_network_sample[,c(-1, -5, -6)], Ticker2, cos_vecs)
    lag_daya_network_sample <- as.matrix(column_to_rownames(lag_daya_network_sample, var = "Ticker"))
    sym_lag_multiday <- pmax.abs(lag_daya_network_sample, t(lag_daya_network_sample))
    
    multilags[[day]] <- sym_lag_multiday
  }
  names(multilags) <- names(daily_res_multilag)
  dist_out <- paste0("data/crypto/multilag/multilags3_", year, ".rda")
  save(multilags, file = dist_out)
  
}




# plot --------------------------------------------------------------------

# custom palette
my_palette <- colorRampPalette(c("red", "purple", "blue"))(n = 299)
# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,-0.3,length=100),  # for red
               seq(-0.3, 0.3,length=100),              # for yellow
               seq(0.3, 1,length=100))              # for green


day <- "2019-01-04"
heatmap.2(multilags[[day]],
          density.info="none",  
          trace="none",         
          margins =c(12,9),    
          col=my_palette,
          dendrogram='none',     
          Rowv=T,
          main = day,
          Colv="Rowv",
          symm= T, revC = identical("Colv", "Rowv")) 














