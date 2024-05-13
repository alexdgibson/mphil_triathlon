

# Descriptive stats for temperature paper
# Mr. Alexander D Gibson
# 31th July 2023

#load packages
library(tidyverse)
library(lme4)
library(brms)
library(tidybayes)
library(viridis)
library(cowplot)
library(emmeans)


# Water temperature summaries
rbind(swim_men, bike_men, run_men) %>% 
  ungroup() %>% 
  filter(water_temp > 0) %>% 
  distinct(program_id, .keep_all = TRUE) %>% 
  mutate(min = min(water_temp),
         max = max(water_temp),
         mean = mean(water_temp),
         sd = sd(water_temp)) %>% 
  select(min, max, mean, sd)

# Air temperature summaries
rbind(swim_men, bike_men, run_men) %>% 
  ungroup() %>% 
  filter(air_temp > 0) %>% 
  distinct(program_id, .keep_all = TRUE) %>% 
  mutate(min = min(air_temp),
         max = max(air_temp),
         mean = mean(air_temp),
         sd = sd(air_temp)) %>% 
  select(min, max, mean, sd)

# Determining how many missing age values there are


# Ages for men and women
rbind(swim_men, bike_men, run_men) %>% 
  group_by(year, category) %>% 
  distinct(athlete_id, .keep_all = TRUE) %>% 
  select(athlete_id, age) %>% 
  na.omit() %>% 
  ungroup(year) %>%
  summarise(median = median(age),
         IQR = quantile(age)) %>% 
  select(median, IQR)

# How many missing age variables
rbind(swim_men, bike_men, run_men) %>% 
  group_by(year) %>% 
  distinct(athlete_id, .keep_all = TRUE) %>% 
  select(athlete_id, age, program_id) %>% 
  ungroup() %>% 
  mutate(isna = is.na(age)) %>% 
  filter(isna == TRUE) %>% 
  reframe(n = n_distinct(program_id))


# World map for water temperature and air temperature and location

#get the world map
world <- map_data("world")

#create plot

# For water temepratures
water_map <- rbind(swim_men, bike_men, run_men) %>%
  ggplot()+
  geom_polygon(data = world, aes(x=long, y = lat, group = group),  alpha = 0.2) +
  geom_point(aes(x=longitude, y=latitude, fill = water_temp),size = 4, shape = 21, colour = 'black') +
  scale_fill_viridis(option = 'viridis')+
  theme_void()+
  labs(fill = 'Water Temperature (°C)')+
  guide_
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 13),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = NA, color = NA), 
        legend.background = element_rect(fill = NA, color = NA),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))



# For air temperatures
air_map <- rbind(swim_men, bike_men, run_men) %>%
  ggplot()+
  geom_polygon(data = world, aes(x = long, y = lat, group = group),  alpha = 0.2) +
  geom_point(aes(x = longitude, y = latitude, fill = air_temp), alpha = 0.2, size = 3.5, shape = 21, colour = 'black') +
  scale_fill_viridis(option = 'plasma')+
  theme_void()+
  labs(fill = 'Air Temperature (°C)')+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 13),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = NA, color = NA), 
        legend.background = element_rect(fill = NA, color = NA),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


plot_grid(water_map, air_map,
          align = c("hv"),
          nrow = 2)

ggsave(filename = "temperature_map_vert.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 9,
       height = 10,
       dpi = 300)





# Plotting of water temperature and air temperature for locations


# Fitting a linear model for water and air temperatures

rbind(swim_men, bike_men, run_men) %>% ungroup() %>% filter(water_temp > 0 & air_temp > 0) %>% mutate(water = min(water_temp),
                                                                                        air = min(air_temp)) %>% select(water, air) %>% view()

temp_cor <- brm(formula = air_s ~ water_s,
    data = rbind(swim_men, bike_men, run_men) %>%
      ungroup() %>% 
      distinct(program_id, .keep_all = TRUE) %>% 
      filter(water_temp > 0 & air_temp > 0) %>% 
      mutate(water_s = water_temp - min(water_temp),
             air_s = air_temp - min(air_temp)) %>% 
      distinct(program_id, .keep_all = TRUE),
    iter = 25000,
    chains = 8,
    cores = 8,
    seed = 123)

# Posterior predictive checks
pp_check(temp_cor, re_formula = NULL)
# Summary
summary(temp_cor)
# Random effects
ranef(temp_cor)
# Marginal effects
conditional_effects(temp_cor)


#Taking the conditonal effects plot for men and women and adding the observed data one
temp_corr_plot <- conditional_effects(temp_cor)[1] %>% 
  as.data.frame()

temp_cor_freq <- lm(formula = air_temp ~ water_temp, data = rbind(swim_men, bike_men, run_men) %>%
     ungroup() %>% 
     distinct(program_id, .keep_all = TRUE) %>% 
     filter(water_temp > 0 & air_temp > 0))

#Plotting
temp_corr_plot %>% 
  ggplot()+
  geom_point(aes(x = water_temp, y = air_temp), shape = 1, data = rbind(swim_men, bike_men, run_men) %>%
               filter(water_temp > 0 & air_temp > 0))+
  # geom_smooth(aes(x = water_temp, y = air_temp), method = 'lm', colour = 'blue', data = rbind(swim_men, bike_men, run_men) %>%
  #               distinct(program_id, .keep_all = TRUE) %>% 
  #               filter(water_temp > 0 & air_temp > 0))+
  theme_classic()+
  labs(x = 'Water Temperature (°C)',
       y = 'Air Temperature (°C)')+
  scale_y_continuous(limits = c(10,40), breaks = c(10,15,20,25,30,35,40))+
  scale_x_continuous(limits = c(14,32), breaks = c(14,17,20,23,26,29,32))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")




rbind(swim_men, bike_men, run_men) %>%
  filter(water_temp > 0 & air_temp > 0) %>% 
  distinct(program_id, .keep_all = TRUE) %>% 
  ggplot()+
  geom_point(aes(x = water_temp, y = air_temp), shape = 1)+
  geom_smooth(aes(x = water_temp, y = air_temp), method = 'lm', fullrange = FALSE)+
  theme_classic()+
  labs(x = 'Water Temperature (°C)',
       y = 'Air Temperature (°C)')+
  scale_y_continuous(limits = c(10,40), breaks = c(10,15,20,25,30,35,40))+
  scale_x_continuous(limits = c(14,32), breaks = c(14,17,20,23,26,29,32))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave(filename = "temperature_correlation.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 6,
       height = 4,
       dpi = 300)

# How many races had both water and air temperatures

rbind(swim_men, bike_men, run_men) %>% filter(water_temp > 0 & air_temp > 0) %>% distinct(program_id) %>% summarise(n = n())


# How many races had only water or air temperature results for
rbind(swim_men, bike_men, run_men) %>% filter(water_temp > 0) %>% distinct(program_id) %>% summarise(n = n())
rbind(swim_men, bike_men, run_men) %>% filter(air_temp > 0) %>% distinct(program_id) %>% summarise(n = n())


# How many observations for men and women for each segment

rbind(swim_men) %>% filter(category == "Elite Men" & water_temp > 0) %>% summarise(n = n())
rbind(swim_men) %>% filter(category == "Elite Women" & water_temp > 0) %>% summarise(n = n())

rbind(bike_men) %>% filter(category == "Elite Men" & air_temp > 0) %>% summarise(n = n())
rbind(bike_men) %>% filter(category == "Elite Women" & air_temp > 0) %>% summarise(n = n())

rbind(run_men) %>% filter(category == "Elite Men" & air_temp > 0) %>% summarise(n = n())
rbind(run_men) %>% filter(category == "Elite Women" & air_temp > 0) %>% summarise(n = n())

# How many races were total before selecting for ones with temperature
df2 %>% filter(year >= 2015) %>% group_by(category) %>% summarise(n = n_distinct(program_id))
rbind(swim_men, bike_men, run_men) %>% summarise(n = n_distinct(program_id))

# the age of athletes for each year as a plot
rbind(swim_men, bike_men, run_men) %>% 
  group_by(category, year) %>% 
  select(athlete_id, year, age) %>% 
  distinct(athlete_id, .keep_all = TRUE) %>% 
  na.omit() %>% 
  reframe(mu = median(age),
          iqr25 = quantile(age, probs = 0.25),
          iqr75 = quantile(age, probs = 0.75)) %>% 
  ggplot()+
  geom_point(aes(x = year, y = mu, group = category, colour = category), size = 2.5, position = position_dodge(width = 0.3))+
  geom_errorbar(aes(ymin = iqr25, ymax = iqr75, x = year, group = category, colour = category), width = 0.6, position = position_dodge(width = 0.3))+
  theme_minimal(base_size = 12)+
  scale_colour_manual(values = c('black', 'grey60'))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.4,0.9))+
  scale_y_continuous(limits = c(20, 30), 
                     breaks = c(20,22,24,26,28,30))+
  labs(x = 'Years',
       y = 'Age')
  
  
ggsave(filename = "temp_age.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 6,
       height = 4,
       dpi = 300)

  
# the mean water temperature across all the years
rbind(swim_men, bike_men, run_men) %>% 
  group_by(category, year) %>% 
  select(program_id, water_temp, air_temp) %>% 
  distinct(program_id, .keep_all = TRUE) %>% 
  na.omit() %>% 
  reframe(mu = mean(water_temp),
          sd = sd(water_temp)) %>% 
  ggplot()+
  geom_point(aes(x = year, y = mu, group = category, colour = category), size = 2.5, position = position_dodge(width = 0.3))+
  geom_errorbar(aes(x = year, ymin = mu-sd, ymax = mu+sd, group = category, colour = category), width = 0.7, position = position_dodge(width = 0.3))+
  theme_minimal(base_size = 12)+
  scale_colour_manual(values = c('black', 'grey60'))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.3,0.1))+
  scale_y_continuous(limits = c(16, 30),
                     breaks = c(18,22,26,30))+
  labs(x = 'Years',
       y = 'Water Temperature (°C)')

ggsave(filename = "water_temp_years.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 6,
       height = 4,
       dpi = 300)


# the mean air temperature across all the years
rbind(swim_men, bike_men, run_men) %>% 
  group_by(category, year) %>% 
  select(program_id, water_temp, air_temp) %>% 
  distinct(program_id, .keep_all = TRUE) %>% 
  na.omit() %>% 
  reframe(mu = mean(air_temp),
          sd = sd(air_temp)) %>% 
  ggplot()+
  geom_point(aes(x = year, y = mu, group = category, colour = category), size = 2.5, position = position_dodge(width = 0.3))+
  geom_errorbar(aes(x = year, ymin = mu-sd, ymax = mu+sd, group = category, colour = category), width = 0.7, position = position_dodge(width = 0.3))+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.3,0.1))+
  scale_colour_manual(values = c('black', 'grey60'))+
  scale_y_continuous(limits = c(16, 32),
                     breaks = c(16,20,24,28,32))+
  labs(x = 'Years',
       y = 'Air Temperature (°C)')

ggsave(filename = "air_temp_years.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 6,
       height = 4,
       dpi = 300)


# The amount of male and females races per year for the 
rbind(swim_men, bike_men, run_men) %>% 
  group_by(category, year) %>% 
  select(program_id) %>%
  reframe(n = n_distinct(program_id)) %>%
  ggplot()+
  geom_col(aes(x = year, y = n, fill = category), position = 'dodge')+
  scale_fill_manual(values = c('black', 'grey60'))+
  scale_y_continuous(limits = c(0, 35), 
                     breaks = c(0,5,10,15,20,25,30,35))+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8))+
  labs(x = 'Years',
       y = 'Amount of Races')

ggsave(filename = "races_temp_years.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 6,
       height = 4,
       dpi = 300)
