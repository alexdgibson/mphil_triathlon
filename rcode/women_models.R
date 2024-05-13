# Load Packages

library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(emmeans)


# Swim times > 700 are correct
# Bike times > 2500 are correct
# Run times > 1500 are correct


swim_women <- df4 %>% mutate(age = year - yob) %>% filter(swim > 700 & water_temp > 0 & duathlon == FALSE & short_swim == FALSE & category=="Elite Women") %>% 
  dplyr::select(program_id, athlete_id, water_temp, swim, bike, run, age) 

bike_women <- df4 %>% mutate(age = year - yob) %>% filter(swim > 700 & bike > 2500 & air_temp > 0 & duathlon == FALSE & short_swim == FALSE & category=="Elite Women") %>% 
  dplyr::select(program_id, athlete_id, air_temp, swim, bike, run, age, water_temp) %>%  mutate(swim_scaled = as.numeric(scale(swim, center = T, scale = T)), bike_scaled = as.numeric(scale(bike, center = T, scale = T)))

run_women <- df4 %>% mutate(age = year - yob) %>% filter(bike > 2500 & swim > 700 & run > 1500 & air_temp > 0 & duathlon == FALSE & short_swim == FALSE & category=="Elite Women") %>% 
  dplyr::select(program_id, athlete_id, air_temp, swim, bike, run, age, water_temp) %>%  mutate(swim_scaled = as.numeric(scale(swim, center = T, scale = T)), bike_scaled = as.numeric(scale(bike, center = T, scale = T)))


w_swim_model_1 <- lmer(formula = swim ~ poly(water_temp, degree = 3) + age + (1|program_id) + (1|athlete_id), data = swim_women)
w_swim_model_2 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 3) + age + (1|program_id) + (1|athlete_id), data = swim_women)
w_swim_model_3 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = swim_women)
w_swim_model_4 <- glmer.nb(formula = swim ~ poly(water_temp, degree = 4) + age + (1|program_id) + (1|athlete_id), data = swim_women)


w_bike_model_1 <- lmer(formula = bike ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = bike_women)
w_bike_model_2 <- glmer.nb(formula = bike ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = bike_women)
w_bike_model_3 <- glmer.nb(formula = bike ~ poly(air_temp, degree = 2) + swim_scaled + age + (1|program_id) + (1|athlete_id), data = bike_women)
w_bike_model_4 <- lmer(formula = bike ~ poly(air_temp, degree = 2) + swim_scaled + age + (1|program_id) + (1|athlete_id), data = bike_women)


w_run_model_1 <- lmer(formula = run ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = run_women)
w_run_model_2 <- lmer(formula = run ~ poly(air_temp, degree = 2) + swim_scaled + bike_scaled + age + (1|program_id) + (1|athlete_id), data = run_women)
w_run_model_3 <- glmer.nb(formula = run ~ poly(air_temp, degree = 2) + age + (1|program_id) + (1|athlete_id), data = run_women)
w_run_model_4 <- glmer.nb(formula = run ~ poly(air_temp, degree = 2) + swim_scaled + bike_scaled + age + (1|program_id) + (1|athlete_id), data = run_women)
w_run_model_5 <- glmer(formula = run ~ poly(air_temp, degree = 2) + swim_scaled + bike_scaled + age + (1|program_id) + (1|athlete_id), data = run_women, family = Gamma)


car::qqPlot(residuals(run_model_4))

anova(w_swim_model_1, w_swim_model_2, w_swim_model_3, w_swim_model_4)
anova(w_bike_model_1, w_bike_model_2, w_bike_model_3, w_bike_model_4)
anova(w_run_model_1, w_run_model_2, w_run_model_3, w_run_model_4)


car::qqPlot(residuals(bike_model_3))

# Plotting the model
emm_options(rg.limit = 1000000)
emm_options(lmerTest.limit = 10000)

#Get prediction
swim_women$pred <- predict(w_swim_model_2, type = 'response')

# Potting
w_refgrid_swim <- list(age = seq(min(swim_women$age), max(swim_women$age), by = 1),
                     water_temp = seq(min(swim_women$water_temp), max(swim_women$water_temp), by = 0.1))

w_swim_mar_ef95 <- emmip(w_swim_model_2, ~ water_temp, at = w_refgrid_swim, CIs = T, plotit = F, level = 0.95, type = 'response')
w_swim_mar_ef66 <- emmip(w_swim_model_2, ~ water_temp, at = w_refgrid_swim, CIs = T, plotit = F, level = 0.66, type = 'response')


w_swim_mar_ef95 %>% 
  ggplot(aes(x = water_temp,
             y = yvar/60))+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = w_swim_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_line(colour = 'black')+
  theme_classic()+
  labs(x = "Water Temperature (celcius)",
       y = "Swim Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))



# Bike Plotting
bike_women$pred <- predict(w_bike_model_3, type = 'response')

w_refgrid_bike <- list(age = seq(min(bike_women$age), max(bike_women$age), by = 1),
                     air_temp = seq(min(bike_women$air_temp), max(bike_women$air_temp), by = 0.1),
                     swim = seq(min(bike_women$swim), max(bike_women$swim), by = 1))

w_bike_mar_ef95 <- emmip(w_bike_model_3, ~ air_temp, at = w_refgrid_bike, CIs = T, plotit = F, level = 0.95, type = 'response')
w_bike_mar_ef66 <- emmip(w_bike_model_3, ~ air_temp, at = w_refgrid_bike, CIs = T, plotit = F, level = 0.66, type = 'response')

w_bike_mar_ef95 %>% 
  ggplot(aes(x = air_temp,
             y = yvar/60))+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = w_bike_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_line(colour = 'black')+ 
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Bike Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


# Run Plotting

run_women$pred <- predict(w_run_model_4, type = 'response')
w_refgrid_run <- list(age = seq(min(run_women$age), max(run_women$age), by = 1),
                    air_temp = seq(min(run_women$air_temp), max(run_women$air_temp), by = 0.1))

w_run_mar_ef95 <- emmip(w_run_model_4, ~ air_temp, at = w_refgrid_run, CIs = T, plotit = F, level = 0.95, type = 'response')
w_run_mar_ef66 <- emmip(w_run_model_4, ~ air_temp, at = w_refgrid_run, CIs = T, plotit = F, level = 0.66, type = 'response')

w_run_mar_ef95 %>% 
  ggplot(aes(x = air_temp,
             y = yvar/60))+
  geom_ribbon(aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.3, fill = '#08519C')+
  geom_ribbon(data = w_run_mar_ef66, aes(ymin = LCL/60, ymax = UCL/60), alpha = 0.4, fill = '#08519C')+
  geom_line(colour = 'black')+
  theme_classic()+
  labs(x = "Air Temperature (celcius)",
       y = "Run Time (seconds)")+
  theme(axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))
