

# Packages
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(viridis)
library(tidybayes)
library(data.table)
library(ordinal)
library(lme4)

# First section is modelling men in a bayes framework.
# Load data
d <- readRDS("~/Dropbox/Research projects/Project - Triathlon performance/triathlon-performance/elite_women_positions.RDS") # MAC
d <- readRDS("C:/Users/borgdn/Dropbox/Research projects/Project - Triathlon performance/triathlon-performance/elite_women_positions.RDS")


# men_pos_model <- merge(men_finish, complete_men_yobll) %>%
#   filter(status == '' &
#            time_error == FALSE &
#            t1 > 0 &
#            t2 > 0 &
#            pos_swim > 0 &
#            pos_bike > 0 &
#            total_time > 0) %>%
#   filter(year > 2017) %>% 
#   mutate(year_s = year-(min(.$year)))
# 
# women_pos_model <- merge(women_finish, complete_women_yobll) %>%
#   filter(status == '' &
#            time_error == FALSE &
#            t1 > 0 &
#            t2 > 0 &
#            pos_swim > 0 &
#            pos_bike > 0 &
#            total_time > 0) %>%
#   filter(year > 2017) %>% 
#   mutate(year_s = year-(min(.$year)))

## Checking another way to get data to model for races with only all times there
# A final version way to do the positioning for the model

# Updating this by change 'complete_men_yobll' to 'complete_men' to update with 2023 data. add _yobll to get back
men_df1 <- complete_men %>%
  group_by(program_id) %>% 
  filter(year > 2017) %>% 
  mutate(c_swim = swim,
         c_t1 = swim + t1,
         c_bike = swim + t1 + bike,
         c_t1 = ifelse(c_swim == c_t1, 0, c_t1),
         c_bike = ifelse(c_t1 == c_bike, 0, c_bike))

men_finish <- men_df1 %>% 
  group_by(program_id) %>% 
  mutate(pos_swim = ifelse(c_swim == 0, 0, rank(c_swim[c_swim > 0], ties.method='min')),
         pos_t1 = ifelse(c_t1 == 0 | c_swim == 0, 0, rank(c_t1[c_t1 > 0], ties.method='min')),
         pos_bike = ifelse(c_bike == 0 | c_swim == 0 | c_t1 == 0, 0, rank(c_bike[c_bike > 0], ties.method='min')))

# Same as above, changing from _yobll to complete_women to update for 2023 data
women_df1 <- complete_women %>%
  group_by(program_id) %>% 
  filter(year > 2017) %>% 
  mutate(c_swim = swim,
         c_t1 = swim + t1,
         c_bike = swim + t1 + bike,
         c_t1 = ifelse(c_swim == c_t1, 0, c_t1),
         c_bike = ifelse(c_t1 == c_bike, 0, c_bike))

women_finish <- women_df1 %>% 
  group_by(program_id) %>% 
  mutate(pos_swim = ifelse(c_swim == 0, 0, rank(c_swim[c_swim > 0], ties.method='min')),
         pos_t1 = ifelse(c_t1 == 0 | c_swim == 0, 0, rank(c_t1[c_t1 > 0], ties.method='min')),
         pos_bike = ifelse(c_bike == 0 | c_swim == 0 | c_t1 == 0, 0, rank(c_bike[c_bike > 0], ties.method='min')))

# Wrangle
men_pos_model2 <-
  men_finish %>%
  mutate(position_cat = cut(position,
                            breaks = c(0,3,1000),
                            labels = c('podium','not podium')),
         position_cat = as.factor(position_cat),
         position_int = as.integer(position_cat))

women_pos_model2 <-
  women_finish %>%
  mutate(position_cat = cut(position,
                            breaks = c(0,3,1000),
                            labels = c('podium','not podium')),
         position_cat = as.factor(position_cat),
         position_int = as.integer(position_cat))

table(men_pos_model2$position_cat)
table(men_pos_model2$position_int)

saveRDS(men_pos_model2, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/men_pos_model2.RDS")

# Model
fit_pos_performance <-
  brm(
  formula = position_int ~ pos_swim*pos_bike + (1|program_id),
  family = cumulative(link = "logit"),
  chains = 2,
  cores = 2,
  iter = 1000,
  control = list(adapt_delta = 0.8),
  seed = 123,
  data = men_pos_model2)


# Save
save(fit_pos_performance, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/bayes_tri_model_7_year_2000iter.RData")

load(file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/bayes_tri_model_7_year_fixed_effect.RData")
#load(file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/bayes_tri_model_5_year_fixed_effect.RData")

# Posterior predictive checks
pp_check(fit_pos_performance, re_formula = NULL)

# Results and Plots
# Summary
summary(fit_pos_performance)

# Random effects
ranef(fit_pos_performance)

# Marginal effects
conditional_effects(fit_pos_performance, ordinal = TRUE)



# The final data which plots the results for us. Need to fit with more than just the one year of data
# More than one year may be tricky as R may crash with the amount of data
plot_model_bayes <- 
  men_pos_model2 %>% 
  group_by(athlete_id) %>% 
  data_grid(pos_swim, pos_bike, program_id) %>%
  filter(pos_swim %in% c(1:20) & pos_bike %in% c(1:20)) %>% 
  add_epred_draws(fit_pos_performance) %>% 
  group_by(pos_swim, pos_bike, .category) %>% 
  summarise(mu = mean(.epred)) %>% 
  mutate(.category = recode_factor(.category,
                                   '1' = '1-3',
                                   '2' = '4-10th',
                                   '3' = '11-20th'))
plot_model_bayes %>% 
  ggplot()+
  geom_raster(aes(x = pos_swim, y = pos_bike, fill = mu))+
  facet_wrap(~.category)+
  theme_minimal(base_size = 12)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(limits = c(0,21), n.breaks = 20)+
  scale_x_continuous(limits = c(0,21), n.breaks = 20)+
  scale_fill_viridis_c(option = 'A')+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))
  

ggsave(filename = 'position_probability_5_year_men.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 4)



#
# Modelling the prediciton of performance for men in a frequentist framework

# Ordinal Model
men_pos_model3 <- men_pos_model2 %>% ungroup() %>% mutate(race_id = scale(program_id, center = TRUE, scale = TRUE)) %>% 
  select(position_cat, pos_swim, pos_bike, race_id)

model_freq <- clm(position_cat ~ pos_swim*pos_bike + (1|race_id), 
                  data = men_pos_model3,
                  clm.control(maxIter = 100L))

# Summary
summary(model_freq)

# Data grid for the ordinal model
newdata = men_pos_model2 %>%
  group_by(athlete_id) %>% 
  data_grid(pos_swim, pos_bike, program_id)

# Taking predictions from the ordinal model
predictions <- predict(model_freq, newdata, type = "prob") %>%
  as.data.frame()

# Adding predictions to the data grid
dplot = cbind(newdata, predictions)

# Plotting to visualise
dplot %>%
  pivot_longer(cols = c(fit.1.3,fit.4.10,fit.11.20),
               values_to = 'Probability',
               names_to = 'Position') %>%
  mutate(Position = recode_factor(Position,
                                  'fit.1.3' = '1-3',
                                  'fit.4.10' = '4-10',
                                  'fit.11.20' = '11-20')) %>%
  ggplot(aes(x = pos_swim,
             y = pos_bike,
             fill = Probability))+
  geom_tile(size = 0.5) +
  theme_minimal(base_size = 12)+
  facet_grid(~Position)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(limits = c(0,21), n.breaks = 20)+
  scale_x_continuous(limits = c(0,21), n.breaks = 20)+
  scale_fill_viridis_c(option = 'A')+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))

ggsave(filename = 'position_probability_5_year.png',
       file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 9,
       height = 4)



# Differing plots to visualise the change in probability
dplot %>% select(program_id, pos_swim, pos_bike, program_id, fit.1.3) %>% 
  filter(pos_swim == 1 & pos_bike <21) %>% 
  pivot_longer(cols = c(fit.1.3),
               values_to = 'Probability',
               names_to = 'Position') %>% 
  ggplot(aes(x = pos_bike, y = Probability, group = pos_bike))+
  geom_boxplot()+
  theme_minimal(base_size = 12)+
  labs(x = 'Position into transition two',
       y = 'Probability')

dplot %>% select(program_id, pos_swim, pos_bike, race_id, fit.1, fit.2, fit.3) %>% 
  filter(pos_swim == 20 & pos_bike == 20) %>% 
  pivot_longer(cols = c(fit.1),
               values_to = 'Probability',
               names_to = 'Position') %>% 
  group_by(Position) %>% 
  mutate(m = (mean(Probability))*100) %>% view()





# Modelling it for women now 
women_pos_model <- women_finish %>%
  filter(status == '' &
           time_error == FALSE &
           t1 > 0 &
           t2 > 0 &
           pos_swim > 0 &
           pos_bike > 0 &
           total_time > 0) %>%
  filter(year == 2022 | year == 2021 | year == 2020 | year == 2019 | year == 2018) %>% 
  mutate(year_s = year-(min(.$year)))


# Wrangle
women_pos_model2 <-
  women_pos_model %>%
  mutate(position_cat = cut(position,
                            breaks = c(0,3,10,20),
                            labels = c('1-3','4-10','11-20')),
         position_cat = as.factor(position_cat),
         position_int = as.integer(position_cat))


table(women_pos_model2$position_cat)
table(women_pos_model2$position_int)

save(women_pos_model2, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/women_pos_model2.RData")


# Model
fit_pos_performance_w <-
  brm(
    formula = position_int ~ pos_swim*pos_bike + (1|program_id),
    family = cumulative(link = "logit"),
    chains = 2,
    cores = 2,
    iter = 1000,
    control = list(adapt_delta = 0.8),
    seed = 123,
    data = women_pos_model2)


# Save
save(fit_pos_performance_w, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/bayes_tri_model_7_year_fixed_effect_women.RData")

# Posterior predictive checks
pp_check(fit_pos_performance_w)

# Summary
summary(fit_pos_performance_w)

# Random effects
ranef(fit_pos_performance_w)

# Marginal effects
conditional_effects(fit_pos_performance_w, ordinal = TRUE)



# The final data which plots the results for us. Need to fit with more than just the one year of data
# More than one year may be tricky as R may crash with the amount of data
plot_model_bayes_w <- 
  women_pos_model2 %>%
  group_by(athlete_id) %>% 
  data_grid(pos_swim, pos_bike, program_id) %>%
  filter(pos_swim %in% c(1:20) & pos_bike %in% c(1:20)) %>% 
  add_epred_draws(fit_pos_performance_w) %>% 
  group_by(pos_swim, pos_bike, .category) %>% 
  summarise(mu = mean(.epred)) %>% 
  mutate(.category = recode_factor(.category,
                                   '1' = '1-3',
                                   '2' = '4-10th',
                                   '3' = '11-20th'))
plot_model_bayes_w %>% 
  ggplot()+
  geom_raster(aes(x = pos_swim, y = pos_bike, fill = mu))+
  facet_wrap(~.category)+
  theme_minimal(base_size = 12)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_y_continuous(limits = c(0,21), n.breaks = 20)+
  scale_x_continuous(limits = c(0,21), n.breaks = 20)+
  scale_fill_viridis_c(option = 'A')+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))


ggsave(filename = 'position_probability_5_year_women.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 4)









# Women as a frequentist but have not touched in a long time

women_dsub2 <- men_pos_model2 %>% filter(position_int < 4)

women_pos_model2 %>%
  group_by(program_id) %>%
  summarise(n=n()) %>%
  mutate(race_id = 1:nrow(.)) %>%
  select(-n) -> wom_new_race_id2

wom_dsub2 <- left_join(women_pos_model2, wom_new_race_id2,
                   by = 'program_id')

head(wom_dsub2,5)
wom_model_freq <- clm(position_cat ~ pos_swim*pos_bike + (1|race_id), data = wom_dsub2)

names(women_pos_model2)

summary(wom_model_freq)


wom_newdata = wom_dsub2 %>%
  select(pos_swim, pos_bike, race_id) %>%
  data_grid(pos_swim, pos_bike,race_id)

wom_predictions <- predict(wom_model_freq, wom_newdata, type = "prob") %>%
  as.data.frame()

wom_dplot = cbind(wom_newdata, wom_predictions)


wom_dplot %>%
  pivot_longer(cols = c(fit.1, fit.2,fit.3),
               values_to = 'Probability',
               names_to = 'Position') %>%
  mutate(Position = recode_factor(Position,
                                  'fit.1' = '1st',
                                  'fit.2' = '2nd',
                                  'fit.3' = '3rd')) %>%
  ggplot(aes(x = pos_swim,
             y = pos_bike,
             fill = Probability))+
  geom_tile(size = 0.5) +
  theme_minimal(base_size = 12)+
  facet_grid(~Position)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  scale_y_continuous(limits = c(0,21), n.breaks = 20)+
  scale_x_continuous(limits = c(0,21), n.breaks = 20)+
  scale_fill_viridis_c(option = 'A')+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))

ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete/figure_win_1-3_women.pdf',
       dpi = 900,
       width = 9,
       height = 4)

wom_dplot %>% select(program_id, pos_swim, pos_bike, race_id, fit.1, fit.2, fit.3) %>% 
  filter(pos_swim == 20 & pos_bike == 20) %>% 
  pivot_longer(cols = c(fit.1, fit.2, fit.3),
               values_to = 'Probability',
               names_to = 'Position') %>% 
  group_by(Position) %>% 
  mutate(m = (mean(Probability))*100) %>% view()


# Joshua Bon plotting
men_pos_model2 %>%
  group_by(athlete_id) %>%
  data_grid(pos_swim, pos_bike, program_id) %>%
  filter(pos_swim %in% c(1:20) & pos_bike %in% c(1:20)) %>%
  add_epred_draws(fit_pos_performance, allow_new_levels = TRUE) %>%
  mutate(pos_swim_s = factor(pos_swim, levels = c(1,2,3,10,20,30)),
         pos_bike_s = factor(pos_bike, levels = c(1,2,3,10,20,30),
                             (.category = recode_factor(.category,
                                                        '1' = 'Winner',
                                                        '2' = 'Second',
                                                        '3' = 'Third',
                                                        '4' = '4-10th',
                                                        '5' = '11-20th',
                                                        '6' = '20+')))) %>%
  filter(.category %in% c(1:3)) %>%
  select(pos_swim, pos_bike, .value, .category) %>%
  group_by(pos_swim, pos_bike, .category) %>%
  summarise(mv = mean(.value)) %>%
  ggplot()+
  geom_tile(aes(x = pos_swim, y = pos_bike, fill = mv))+
  facet_wrap(~.category)+
  scale_fill_viridis(begin = 0.1)
