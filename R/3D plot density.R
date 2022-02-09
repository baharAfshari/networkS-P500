library(igraph)
library(ggnetwork)
library(tidyr)
library(dplyr)
library(tibble)
library(gplots)
library(plotly)
library(LaplacesDemon)

load("data/crypto/total_tidy.rda")

date <- as.Date("2018-02-05")

the_date <- total_tidy %>% filter(Date == date) %>% select(vel_x, vel_y)
tbl_date <- table(the_date)
joint_prob <- tbl_date / sum(tbl_date)
joint_prob_df <- joint_prob %>% as.data.frame() 

fig.n <- plot_ly(joint_prob_df, x = joint_prob_df$vel_x, y= joint_prob_df$vel_y,z = ~joint_prob_df$Freq)
fig.n <- fig.n %>% add_trace()
fig.n


joint_prob_df <- joint_prob %>% as.data.frame() 
plot_ly() %>% 
  add_trace(data = joint_prob_df, x = joint_prob_df$vel_x, joint_prob_df$vel_y,z = ~joint_prob_df$Freq, type="mesh3d" ) 


fig.n <- plot_ly(x = row.names(joint_prob), y = colnames(joint_prob) ,z = ~joint_prob)
fig.n <- fig.n %>% add_surface()
fig.n
