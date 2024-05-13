library(tidyverse)
library(ggplot2)

men_finish %>%
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & time_error == FALSE & position == 1) %>% 
  ggplot(aes(x = pos_swim))+
  geom_histogram(binwidth = 1)

men_finish %>%
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & time_error == FALSE & position == 1) %>% 
  ggplot(aes(x = pos_bike))+
  geom_histogram(binwidth = 1)

men_finish %>%
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & time_error == FALSE & position == 1) %>% 
  ggplot(aes(x = pos_run))+
  geom_histogram(binwidth = 1)

men_finish %>%
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & time_error == FALSE & position == 1) %>% 
  ggplot(aes(x = pos_t2))+
  geom_histogram(binwidth = 1)


