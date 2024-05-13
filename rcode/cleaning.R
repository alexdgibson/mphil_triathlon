#load in packages

library(tidyverse)
library(janitor)
library(stringr)
library(readr)
library(ggplot2)
library(lubridate)
library(lme4)
library(cowplot)

#load in the data

df <- read_csv(file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/raw-data-temp.csv") %>% 
  clean_names()
para <- read_rds(file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/para_dataset.RDS") %>%
  clean_names()

#selecting the elite category
df2 <- df %>% 
  filter(category == "Elite Men" | category == "Elite Women")

#Parse the temperature out from the program notes variable
water_temp <- str_extract(df2$program_notes, "(?i)water temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
water_temp <- parse_number(water_temp)

air_temp <- str_extract(df2$program_notes, "(?i)air temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
air_temp <- parse_number(air_temp)

duathlon <- str_detect(df2$program_notes, "(?i)duathlon")
short_swim <- str_detect(df2$program_notes, "(?i)(750|0\\.75|1200)")

air_temp <- as.data.frame(air_temp)
water_temp <- as.data.frame(water_temp)

# Combining both temperature to final data frame
df3 <- cbind(water_temp, air_temp, duathlon, short_swim, df2)

# Finalising the data frame for modelling
rm_races <- df3 %>% filter(duathlon == TRUE | short_swim == TRUE) %>% 
  distinct(program_id)

df3 %>% filter(water_temp > 0 | air_temp > 0 & duathlon == FALSE & short_swim == FALSE) %>% distinct(program_id)

df4 <- subset(df3, !program_id %in% rm_races$program_id) %>%
  filter(water_temp > 0 | air_temp > 0) %>% 
  filter(year < 2023)

df4 %>% filter(category == 'Elite Men') %>% distinct(program_id) %>% count()
# Turning Time into seconds

df3$total_time <- strptime(df3$total_time, format = "%H:%M:%OS")
df3$total_time <- hour(df3$total_time) * 3600 + minute(df3$total_time) * 60 + second(df3$total_time)

df3$swim <- strptime(df3$swim, format = "%H:%M:%OS")
df3$swim <- hour(df3$swim) * 3600 + minute(df3$swim) * 60 + second(df3$swim)

df3$bike <- strptime(df3$bike, format = "%H:%M:%OS")
df3$bike <- hour(df3$bike) * 3600 + minute(df3$bike) * 60 + second(df3$bike)

df3$run <- strptime(df3$run, format = "%H:%M:%OS")
df3$run <- hour(df3$run) * 3600 + minute(df3$run) * 60 + second(df3$run)


# Check how many total races have both temperatures
df3 %>% na.omit() %>% distinct(program_id, .keep_all = TRUE) %>% view()
df3 %>% filter(air_temp > 0 | water_temp > 0) %>% view()
df3 %>% filter(air_temp > 0 | water_temp > 0) %>% filter(year > 1999) %>% distinct(year)
min(df3$air_temp %>% na.omit())
min(df3$water_temp %>% na.omit())
max(df3$air_temp %>% na.omit())
df3 %>% view()
df3 %>% filter(duathlon == FALSE) %>% distinct(program_id)

df3 %>% filter(swim < 800 & swim > 0 & air_temp > 0 & water_temp > 0) %>% view()

# Visualise water temperature distribution
df4 %>% distinct(program_id, .keep_all = TRUE) %>%
  filter(water_temp < 500) %>% 
  ggplot(aes(x = water_temp)) +
  geom_histogram(binwidth = 1)+
  theme_classic()+
  labs(title = "Water Temperature",
       x = "Temperature (Celcius)",
       y = " Count")

# Visualise air temperature distribution
df4 %>% distinct(program_id, .keep_all = TRUE) %>%
  ggplot(aes(x = air_temp)) +
  geom_histogram(binwidth = 1)+
  theme_classic()+
  labs(title = "Air Temperature",
       x = "Temperature (Celcius)",
       y = " Count")




# Initial Graphs

df4 %>% filter(water_temp > 0 & water_temp < 100 & swim > 750 & swim < 1750) %>%
  group_by(water_temp) %>% 
  mutate(mean_swim = mean(swim)) %>% 
  ggplot(aes(x = water_temp, y = mean_swim))+
  geom_point()+
  geom_smooth(method = 'loess')+
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'red')+
  # geom_smooth(method = "lm", formula = y ~ poly(x, 3), colour = 'purple')+
  # geom_smooth(method = "lm", colour = 'red')+
  geom_smooth(method = "gam", colour = 'green')+
  theme_classic()+
  facet_grid(~category)+
  labs(title = "Men - Swim",
       x = "Water Temp (celcius)",
       y = "Swim Time (seconds)")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave(filename = "fits_swim.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 6,
       dpi = 300)

df4 %>% filter(air_temp > 0 & run > 1500 & run < 3800) %>% 
  group_by(program_id) %>% 
  mutate(mean_run = mean(run)) %>% 
  ggplot(aes(x = air_temp, y = mean_run))+
  geom_point()+
  #geom_smooth(method = 'loess')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'purple')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), colour = 'red')+
  #geom_smooth(method = "lm", colour = 'red')+
  #geom_smooth(method = "gam", colour = 'green')+
  theme_classic()+
  facet_grid(~category)+
  labs(title = "Men - Run",
       x = "Air Temp (celcius)",
       y = "Run Time (seconds)")+  
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave(filename = "fits_run.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 6,
       dpi = 300)





df4 %>% filter(air_temp > 0 & bike > 2500 & bike < 6000) %>% 
  group_by(program_id) %>% 
  mutate(mean_bike = mean(bike)) %>% 
  ggplot(aes(x = air_temp, y = mean_bike))+
  geom_point()+
  facet_grid(~category)+
  #geom_smooth(method = 'loess')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), colour = 'red')+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), colour = 'purple')+
  #geom_smooth(method = "lm", colour = 'red')+
  #geom_smooth(method = "gam", colour = 'green')+
  theme_classic()+
  labs(title = "Men - Bike",
       x = "Air Temp (celcius)",
       y = "Bike Time (seconds)")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")
  
ggsave(filename = "fits_bike.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 6,
       dpi = 300)



temp_time <- plot_grid(men_swim, women_swim, men_bike, women_bike, men_run, women_run,
          nrow = 3)

ggsave(filename = "temp_time.png", 
        plot = temp_time, 
        path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
        width = 10,
        height = 12,
        dpi = 300)


### PARA STUFF

#Para Triathlon Temperature

#all program id from para
pp <- para %>% 
  distinct(program_id)


pdf <- subset(df, program_id %in% pp$program_id)

#Parse the temperature out from the program notes variable
pwater_temp <- str_extract(pdf$program_notes, "(?i)water temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
pwater_temp <- parse_number(pwater_temp)

pair_temp <- str_extract(pdf$program_notes, "(?i)air temperature\\s*:?\\s*([0-9]+(?:\\.[0-9]+)?)")
pair_temp <- parse_number(pair_temp)

pair_temp <- as.data.frame(pair_temp)
pwater_temp <- as.data.frame(pwater_temp)

# Combining both temperature to final data frame
pdf3 <- cbind(pwater_temp, pair_temp, pdf)

# Check how many total races have both temperatures
pdf3

saveRDS(pdf3, "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/para_temp.RDS")
