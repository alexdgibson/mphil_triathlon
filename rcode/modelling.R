# Load Packages

library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(emmeans)
library(splines)


# Swim times > 700 are correct
# Bike times > 2500 are correct
# Run times > 1500 are correct

# Determine through BIC which model is best appropriate for results
#Swim BIC Male and Female is 3rd Order
BIC(df4 %>% 
  filter(swim > 700 & category == 'Elite Men') %>%
  lm(formula = swim ~ poly(water_temp, degree = 3, raw = T)))

BIC(df4 %>% 
      filter(swim > 700 & category == 'Elite Women') %>%
      lm(formula = swim ~ poly(water_temp, degree = 3, raw = T)))



# Bike BIC Male is 3rd Order and Female is 4th Order
BIC(df4 %>% 
      filter(bike > 2500 & category == 'Elite Men') %>%
      lm(formula = swim ~ poly(water_temp, degree = 3, raw = T)))

BIC(df4 %>% 
      filter(bike > 2500 & category == 'Elite Women') %>%
      lm(formula = swim ~ poly(water_temp, degree = 4, raw = T)))



# Run BIC Male is 3rd Order and Female is 4th Order
BIC(df4 %>% 
      filter(run > 1500 & category == 'Elite Men') %>%
      lm(formula = swim ~ poly(water_temp, degree = 4, raw = T)))

BIC(df4 %>% 
      filter(run > 1500 & category == 'Elite Women') %>%
      lm(formula = swim ~ poly(water_temp, degree = 4, raw = T)))



# Organising a final plot type

df4 %>% filter(water_temp > 0 & swim > 750 & swim < 2500) %>% 
  group_by(program_id) %>% 
  ggplot(aes(x = water_temp, y = swim))+
  geom_point()+
  facet_grid(~category)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), colour = 'blue', fill = 'blue')+
  geom_smooth(method = 'loess')+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Swim Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

ggsave(filename = "fits_swim.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 5,
       dpi = 300)

df4 %>% filter(air_temp > 0 & bike > 2500 & bike < 6000) %>% 
  group_by(program_id) %>% 
  ggplot(aes(x = air_temp, y = bike))+
  #geom_point()+
  facet_grid(~category)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'darkgreen', fill = 'darkgreen')+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Bike Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

ggsave(filename = "fits_bike.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 5,
       dpi = 300)


df4 %>% filter(air_temp > 0 & run > 1500 & run < 4000) %>% 
  group_by(program_id) %>% 
  ggplot(aes(x = air_temp, y = run))+
  #geom_point()+
  facet_grid(~category)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), colour = 'darkorange', fill = 'darkorange')+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Run Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

ggsave(filename = "fits_run.png", 
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images",
       width = 8,
       height = 5,
       dpi = 300)

df4 %>% 
  filter(total_time > 0 & year >= 2000) %>% 
  mutate(age = year-yob) %>%
  filter(age >18, age <44) %>%
  group_by(age,
           category) %>%
  summarise(mu = mean(total_time, na.rm = T)/60) %>%
  ggplot(aes(x = age, y = mu))+
  geom_point()+
  geom_smooth()+
  ylim(100,175)+
  facet_grid(~category)

df4 %>% 
  filter(run > 0 & year >= 2001) %>% 
  mutate(age = year-yob) %>%
  filter(age >18, age <44) %>%
  group_by(year,
           category) %>%
  summarise(mu = mean(run, na.rm = T)/60) %>%
  ggplot(aes(x = year, y = mu))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_grid(~category)


swim_men %>% ggplot(aes(x = swim))+
  geom_histogram()

bike_men %>% ggplot(aes(x = bike))+
  geom_histogram()

run_men %>% ggplot(aes(x = run))+
  geom_histogram()


# Modelling with lmer
# setting data frames to models

swim_men <- df4 %>% mutate(age = year - yob) %>% filter(swim > 0 & air_temp > 0 & duathlon == FALSE & short_swim == FALSE & year >= 2015) %>% 
  mutate(age = year - yob) %>% 
  dplyr::select(program_id, athlete_id, air_temp, swim, bike, run, age, water_temp, category, year, longitude, latitude) %>% mutate(wet_suit = case_when(water_temp <= 15.9 ~ 'mandatory',
                                                                                                                water_temp >= 20 ~ 'illegal',
                                                                                                                TRUE ~ 'optional')) %>% 
  group_by(category) %>% 
  mutate(program_id_scaled = scale(program_id, scale = T),
         athlete_id_scaled = scale(athlete_id, scale = T))

bike_men <- df4 %>% mutate(age = year - yob) %>% filter(bike > 0 & air_temp > 0 & duathlon == FALSE & short_swim == FALSE & year >= 2015) %>% 
  dplyr::select(program_id, athlete_id, air_temp, swim, bike, run, age, water_temp, category, longitude, latitude) %>% 
  group_by(category) %>% 
  mutate(swim_scaled = scale(swim, center = TRUE, scale = TRUE))


run_men <- df4 %>% mutate(age = year - yob) %>% filter(run > 0 & air_temp > 0 & duathlon == FALSE & short_swim == FALSE & year >= 2015) %>% 
  dplyr::select(program_id, athlete_id, air_temp, swim, bike, run, age, water_temp, category, longitude, latitude) %>% 
  group_by(category) %>% 
  mutate(swim_scaled = as.numeric(scale(swim, center = T, scale = T)),
         bike_scaled = as.numeric(scale(bike, center = T, scale = T)))

df4 %>% filter(year >= 2015)
rbind(swim_men, bike_men, run_men)

saveRDS(swim_men, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/swim_men.rds")
saveRDS(bike_men, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/bike_men.rds")
saveRDS(run_men, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/run_men.rds")


# Swim Models
swim_model_1 <- lmer(formula = swim ~ poly(water_temp, degree = 3) + age + wet_suit + (1|program_id) + (1|athlete_id), data = swim_men)
swim_model_2 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 2) + age + wet_suit + (1|program_id) + (1|athlete_id), data = swim_men)
swim_model_3 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 3)*category + category + age + wet_suit*category + (1|program_id) + (1|athlete_id), data = swim_men)
swim_model_4 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 4) + age + wet_suit + (1|program_id) + (1|athlete_id), data = swim_men)
swim_model_5 <- glmer.nb(formula = swim ~ bs(water_temp, knots = c(18,19,20))*category + wet_suit + age + category + (1|program_id_scaled) + (1|athlete_id_scaled), data = swim_men, verbose = TRUE)

#Bike Models
bike_model_1 <- lmer(formula = bike ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = bike_men)
bike_model_2 <- glmer.nb(formula = bike ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = bike_men)
bike_model_3 <- glmer.nb(formula = bike ~ poly(air_temp, degree = 2)*category + swim_scaled*category + category + age + (1|program_id) + (1|athlete_id), data = bike_men)
bike_model_4 <- lmer(formula = bike ~ poly(air_temp, degree = 2)*category + swim_scaled*category + category + age + (1|program_id) + (1|athlete_id), data = bike_men)
bike_model_5 <- glmer(formula = bike ~ poly(air_temp, degree = 2) + swim_scaled + category + age + (1|program_id) + (1|athlete_id), data = bike_men, family = binomial(link = 'logit'))

# Run Models
run_model_1 <- lmer(formula = run ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = run_men)
run_model_2 <- lmer(formula = run ~ poly(air_temp, degree = 2) + swim_scaled + bike_scaled + age + (1|program_id) + (1|athlete_id), data = run_men)
run_model_3 <- glmer.nb(formula = run ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = run_men)
run_model_4 <- glmer.nb(formula = run ~ poly(air_temp, degree = 2)*category + swim_scaled*category + bike_scaled*category + category + age + (air_temp|category) + (1|program_id) + (1|athlete_id), data = run_men)
run_model_5 <- glmer(formula = run ~ poly(air_temp, degree = 2)*category + swim_scaled*category + bike_scaled*category + category + age + (1|program_id) + (1|athlete_id), data = run_men, family = Gamma)


anova(run_model_1, run_model_2, run_model_3, run_model_4)
car::qqPlot(residuals(swim_model_5))
anova(swim_model_5)
hist(residuals(swim_model_1), breaks = 100)

anova(swim_model_1, swim_model_3, swim_model_5)
anova(bike_model_1, bike_model_2, bike_model_3, bike_model_4)
anova(run_model_1, run_model_2, run_model_3, run_model_4)


car::qqPlot(residuals(bike_model_3))

# Plotting the model
emm_options(rg.limit = 1000000)
emm_options(lmerTest.limit = 100000)

#Get prediction
#swim_men$pred <- predict(swim_model_3, type = 'response')

# Potting
refgrid_swim <- list(age = seq(min(swim_men$age), max(swim_men$age), by = 1),
                    water_temp = seq(min(swim_men$water_temp), max(swim_men$water_temp), by = 0.1),
                     wet_suit = c('illegal','mandatory','optional'),
                     category = c('Elite Men','Elite Women'))

swim_mar_ef95 <- emmip(swim_model_5, ~ water_temp|category, at = refgrid_swim, CIs = T, plotit = F, level = 0.95, type = 'response')
swim_mar_ef66 <- emmip(swim_model_5, ~ water_temp|category, at = refgrid_swim, CIs = T, plotit = F, level = 0.66, type = 'response')


swim_mar_ef95 %>% 
  ggplot(aes(x = water_temp,
             y = yvar/60))+
  geom_point(data = swim_men %>%
               group_by(category, water_temp) %>%
               mutate(mu = mean(swim/60)),
             aes(x = water_temp, y = mu),
             colour = 'red')+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = swim_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_line(colour = 'black')+
  facet_grid(~ category) +
  theme_classic()+
  labs(x = "Water Temperature (celcius)",
       y = "Swim Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))



# Bike Plotting
#bike_men$pred <- predict(bike_model_3, type = 'response')

refgrid_bike <- list(age = seq(min(bike_men$age), max(bike_men$age), by = 1),
                     air_temp = seq(min(bike_men$air_temp), max(bike_men$air_temp), by = 0.1),
                     swim = seq(min(bike_men$swim), max(bike_men$swim), by = 1),
                     category = c('Elite Men','Elite Women'))

bike_mar_ef95 <- emmip(bike_model_3, ~ air_temp|category, at = refgrid_bike, CIs = T, plotit = F, level = 0.95, type = 'response')
bike_mar_ef66 <- emmip(bike_model_3, ~ air_temp|category, at = refgrid_bike, CIs = T, plotit = F, level = 0.66, type = 'response')

bike_mar_ef95 %>% 
  ggplot(aes(x = air_temp,
             y = yvar/60))+
  geom_point(data = bike_men %>% 
               group_by(air_temp, category) %>%
               mutate(mu = mean(bike/60)),
             aes(x = air_temp, y = mu),
             colour = 'red',
             shape = 3)+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = bike_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_line(colour = 'black')+ 
  facet_grid(~category)+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Bike Time (minutes)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


# Run Plotting
#run_men$pred <- predict(run_model_4, type = 'response')
refgrid_run <- list(age = seq(min(run_men$age), max(run_men$age), by = 1),
                    air_temp = seq(min(run_men$air_temp), max(run_men$air_temp), by = 0.1),
                    category = c('Elite Men','Elite Women'))

run_mar_ef95 <- emmip(run_model_4, ~ air_temp|category, at = refgrid_run, CIs = T, plotit = F, level = 0.95, type = 'response')
run_mar_ef66 <- emmip(run_model_4, ~ air_temp|category, at = refgrid_run, CIs = T, plotit = F, level = 0.66, type = 'response')

run_mar_ef95 %>% 
  ggplot(aes(x = air_temp,
             y = yvar/60))+
  geom_point(data = run_men %>% 
               group_by(air_temp, category) %>%
               mutate(mu = mean(run/60)),
             aes(x = air_temp, y = mu),
             colour = 'red',
             shape = 3)+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = run_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_line(colour = 'black')+
  facet_grid(~category)+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Run Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))



run_men %>% 
  ggplot(aes(x = run/60))+
  geom_histogram(binwidth = 0.5, colour = 'white')


run_men <- run_men %>%
  mutate(c_bike = swim + bike)

run_men <- run_men %>%
  mutate(c_bike = ifelse(bike == 0, 0, swim + bike))

run_men <- run_men %>%
  group_by(program_id) %>%
  mutate(pos_bike = ifelse(c_bike > 0, rank(c_bike[c_bike > 0], ties.method='min'), 0))



run_men %>% 
  ggplot(aes(x = pos_bike, y = run))+
  geom_point()+
  geom_smooth(method = 'lm')


