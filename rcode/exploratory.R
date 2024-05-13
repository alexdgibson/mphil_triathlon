library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)

swim_men %>% filter(swim/60 < 40) %>% 
  ggplot(aes(x = swim/60))+
  geom_histogram(binwidth = 1)

bike_men %>% filter(bike/60 < 100) %>% 
  ggplot(aes(x = bike/60))+
  geom_histogram(binwidth = 1)

run_men %>% filter(run/60 < 60) %>% 
  ggplot(aes(x = run/60))+
  geom_histogram(binwidth = 1)


plot(run_men$air_temp, run_men$run)
plot(run_men$age, run_men$run)
plot(run_men$swim+run_men$bike, run_men$run) +abline(lm(run ~ {bike+swim}, data = run_men), col = 'red')


run_men %>% filter(run > 1500) %>% 
  ggplot(aes(x = air_temp, y = run))+
  geom_point()


bike_men %>% 
  filter(bike > 0) %>% 
  ggplot(aes(x = bike, y = air_temp))+
  geom_point()

run_men %>% 
  filter(run > 0 & run < 3000) %>% 
  ggplot(aes(x = bike))+
  geom_histogram()

#Correlation between water and air temperatues
df4 %>% 
  group_by(program_id) %>% 
  filter(water_temp > 0 & air_temp > 0 & year >= 2000) %>% 
  ggplot(aes(x = water_temp, y = air_temp))+
  geom_point()+  
  theme_classic()+
  labs(x = "Water Temperature (celcius)",
       y = "Air Temperature (celcius)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))+
  scale_y_continuous(name = "Air Temperature (celcius)", breaks = c(10,15,20,25,30,35,40))+
  scale_x_continuous(name = "Air Temperature (celcius)", breaks = c(10,15,20,25,30,35))


# Checking latidude and water temperature
df4 %>% 
  group_by(program_id) %>% 
  filter(water_temp > 0 & air_temp > 0) %>% 
  ggplot(aes(x = latitude, y = water_temp))+
  geom_point()+
  geom_smooth(method = 'loess')+
  theme_classic()+
  labs(x = "Latitude",
       y = "Water Temp (celcius")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

# Checking latidude and air temperature
df4 %>% 
  group_by(program_id) %>% 
  filter(water_temp > 0 & air_temp > 0) %>% 
  ggplot(aes(x = latitude, y = air_temp))+
  geom_point()+
  geom_smooth(method = 'loess')


# Checking water temperature against year - has water temps increased over time
df4 %>% 
  filter(water_temp > 0, ) %>% 
  ggplot(aes(x = year, y = water_temp))+
  geom_point()+
  theme_classic()+
  labs(x = "Year",
       y = "Water Temp (celcius")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


swim_men %>% 
  filter(swim < 900 & swim > 720 & water_temp > 25 & water_temp < 30) %>% view()
  ggplot(aes(x = water_temp, y = swim))+
  geom_point()+
  theme_classic()+
  labs(x = "Water Temp (celcius",
       y = "Swim Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))
  
  
  
  
  # Quick Models for Wet Suit Legal and Non Wet suit Swim
  
swim_men %>% 
  filter(swim > 750 & water_temp > 20) %>% 
  ggplot(aes(x = water_temp, y = swim))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'red')+
  theme_classic()+
  labs(x = "Water Temp (celcius",
       y = "Swim Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

swim_men %>% 
  filter(swim > 750 & water_temp < 20) %>% 
  ggplot(aes(x = water_temp, y = swim))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'red')+
  theme_classic()+
  labs(x = "Water Temp (celcius",
       y = "Swim Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


# Mean Temperature time for swim
swim_men %>% 
  filter(swim > 750 & water_temp > 15 & year >= 2000) %>% 
  group_by(water_temp) %>% 
  mutate(mean_swim = mean(swim)) %>% 
  filter(mean_swim < 2000) %>% 
  ggplot(aes(x = water_temp, y = mean_swim/60))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(x = "Water Temp (celcius",
       y = "Swim Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))






rbind(swim_men, bike_men, run_men) %>%
  ungroup() %>% 
  distinct(program_id, .keep_all = TRUE) %>%
  filter(water_temp > 0) %>% 
  summarise(mean_water = mean(water_temp),
         sd_water = sd(water_temp),
         min_water = min(water_temp),
         max_water = max(water_temp))


rbind(swim_men, bike_men, run_men) %>%
  ungroup() %>% 
  distinct(program_id, .keep_all = TRUE) %>%
  filter(air_temp > 0) %>% 
  summarise(mean_air = mean(air_temp),
        sd_air = sd(air_temp),
        min_air = min(air_temp),
        max_air = max(air_temp))





