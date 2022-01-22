library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(magrittr)

load("data/crypto/coupled_devides.rda")
load("data/crypto/triplet_dev_crash.rda")
load("data/crypto/triplet_dev_normal.rda")



# pair --------------------------------------------------------------------

coupled_devides_max <- coupled_devides %>% 
  group_by(Date) %>% summarize(num_comp = n()) %>% 
  mutate(Year = format(Date, "%Y"))


# data of "2018-12-24" crash day & day "2018-12-11" normal day int --------

coupled_devide_crash <- coupled_devides %>% 
  filter(Date == "2018-12-24") %>% 
  mutate(Type = "Crash Day")
coupled_devide_normal <- coupled_devides %>% 
  filter(Date == "2018-12-11") %>% 
  mutate( Type ="Normal Day")
coupled_devide_both <- bind_rows(coupled_devide_crash , coupled_devide_normal)

ggplot(coupled_devide_both , aes(couple_dev , dev_count , color= Type))+
  geom_col(alpha =.5, position = "identity") +
  facet_wrap("Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  aes(fill= Type) + theme(legend.position = "none")



# triplet -----------------------------------------------------------------

triplet_dev_both <- bind_rows(triplet_dev_crash , triplet_dev_normal)

# plots -------------------------------------------------------------------

ggplot(triplet_dev_crash , aes(mean_deg , fill = Type))+
  geom_histogram(binwidth = .5, alpha =.5, position = "identity")+ 
  scale_y_continuous(trans = 'log2')

# power q -----------------------------------------------------------------

powerTriplet <- triplet_dev_crash %>% 
  mutate(lowpower = mean_deg ^ 0.5)

ggplot(powerTriplet , aes(lowpower , fill = Type))+
  geom_histogram(binwidth = .5, alpha =.5, position = "identity") 
