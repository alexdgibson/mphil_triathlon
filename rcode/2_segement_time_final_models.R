
library(tidyverse)
library(ggh4x)
library(janitor)
library(lme4)
library(car)
library(glmmTMB)
library(mgcv)
library(splines)
library(emmeans)

# Model segment times
# A Gibson, DN Borg
# March, 2023

# Default for plots
theme_set(theme_minimal())


# Load data
dw <- readRDS("C:/Users/borgdn/Dropbox/Research projects/Project - Triathlon performance/triathlon-performance/Data/complete_women.RDS") # women
dm <- readRDS("C:/Users/borgdn/Dropbox/Research projects/Project - Triathlon performance/triathlon-performance/Data/complete_men.RDS") # men


# Clean and wrangle women
gam_women_swim <-
  complete_women %>% filter(status == '' &
                  time_error == FALSE &
                  swim > 0 &
                  year >=1990 &
                  swim <10000) %>%
  filter(position >0 & position < 21) %>% # Added filter because there is one result near 50k seconds
  mutate(seg = 'swim',
         sex = 'women') %>%
  rename(time = swim) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_women_bike <-
  complete_women %>% filter(status == '' &
                  time_error == FALSE &
                  bike > 0 &
                  year >=1990) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'bike',
         sex = 'women') %>%
  rename(time = bike) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_women_run <-
  complete_women %>% filter(status == '' &
                  time_error == FALSE &
                  run > 0 &
                  year >=1990) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'run',
         sex = 'women') %>%
  rename(time = run) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_women_tt <-
  complete_women %>% filter(status == '' &
                            time_error == FALSE &
                            total_time > 0 &
                            year >=1990) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'total_time',
         sex = 'women') %>%
  rename(time = total_time) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)



# Clean and wrangle men
gam_men_swim <-
  complete_men %>% filter(status == '' &
                  time_error == FALSE &
                  swim > 0 &
                  year >=1990 &
                  swim <10000) %>% 
  filter(position >0 & position < 21) %>%# Added filter because there is one result near 50k seconds
  mutate(seg = 'swim',
         sex = 'men') %>%
  rename(time = swim) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_men_bike <-
  complete_men %>% filter(status == '' &
                  time_error == FALSE &
                  bike > 0 &
                  year >=1990) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'bike',
         sex = 'men') %>%
  rename(time = bike) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_men_run <-
  complete_men %>% filter(status == '' &
                  time_error == FALSE &
                  run > 0 &
                  year >=1990) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'run',
         sex = 'men') %>%
  rename(time = run) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)

gam_men_tt <-
  complete_men %>% filter(status == '' &
                            time_error == FALSE &
                            total_time > 0 &
                            year > 1999) %>%
  filter(position >0 & position < 21) %>%
  mutate(seg = 'total_time',
         sex = 'men') %>%
  rename(time = total_time) %>%
  select(time, year, seg, sex, program_id, athlete_id, position)


# Join
gam <-
  rbind(gam_women_swim,
        gam_women_bike,
        gam_women_run,
        gam_women_tt,
        gam_men_swim,
        gam_men_bike,
        gam_men_run,
        gam_men_tt) %>%
  as.data.frame() %>%
  mutate(seg = relevel(
    factor(seg,
           levels = c('swim','bike','run', 'total_time')),
    ref = 'swim'))



# Summary
gam_sum <- gam %>%
  group_by(sex,
           seg,
           year) %>%
  summarise(mu = mean(time, na.rm = T))


gam_sum_2000 <- gam_sum %>% 
  filter(year > 1999)

# Plot
gam %>%
  ggplot(aes(x = year, y = time))+
  geom_point(data = gam_sum,
             aes(x = year,
                 y = mu),
             colour = 'red',
             size = 2)+
  facet_wrap(~seg + sex,
             ncol = 2,
             scales = 'free_y')+
  facetted_pos_scales(
    y = rep(list(
      scale_y_continuous(limits = c(1100, 1600), n.breaks = 5),
      scale_y_continuous(limits = c(3200, 5000), n.breaks = 5),
      scale_y_continuous(limits = c(2000, 2800), n.breaks = 5),
      scale_y_continuous(limits = c(6000, 9000), n.breaks = 5)
    ), each = 2)
  )+
  #theme_cowplot()+
  stat_smooth(se = 0, colour = 'black')+
  stat_smooth(aes(y = mu), data = gam_sum %>% filter(year > 1999), method = 'lm', colour = 'blue', se = 0)
  

# Model
mod <- lm(time ~ year + seg,
          data = gam)

hist(residuals(mod), breaks = 1000)
car::qqPlot(residuals(mod))

gam_sum %>% 
  filter(sex == 'men' & seg == 'total_time') %>% 
  ggplot(aes(x = year, y = mu/60))+
  geom_point()
  

mod_sum <- lm(mu ~ year,
                data = gam_sum %>% filter(seg == 'swim' & sex == 'women'))

summary(mod_sum)
confint(mod_sum)

hist(residuals(mod_sum))
car::qqPlot(residuals(mod_sum))

# Save plot
ggsave(file = 'seg_times.pdf', 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete",
       dpi = 900)




fit_linear_men <- lmer(swim ~ year,
            data = gam %>% filter(year > 1999))

res <- residuals(fit) %>%
  as.data.frame()

res %>% ggplot() + geom_histogram(aes(.), colour = 'white', bins = 100)

qqPlot(res$.) # Residuals are skewed, much taller than a 'normal' response



