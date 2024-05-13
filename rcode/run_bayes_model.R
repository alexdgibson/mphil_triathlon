

# Modelling in Bayes for the effect of temperature on cycling time
# Mr. Alexander D Gibson
# 4th July 2023

#load packages
library(tidyverse)
library(brms)
library(car)
library(tidybayes)
library(modelr)
library(splines)
library(ggdist)

# The first model in Bayes for swim time. Copying the frequentest model and imputing it as the formula
run_bayes_1 <- brm(formula = run ~ poly(air_temp, degree = 2)*category + swim_scaled*category + bike_scaled*category + age +
                    (1|program_id) + (1|athlete_id),
                    family = negbinomial(link = 'log'),
                    chains = 8,
                    cores = 8,
                    iter = 20000,
                    control = list(adapt_delta = 0.95,
                                   max_treedepth = 12),
                    seed = 123,
                    data = run_men)

# Posterior predictive checks
pp_check(run_bayes_1, re_formula = NULL)
# Summary
summary(run_bayes_1)
# Random effects
ranef(run_bayes_1)
# Marginal effects
conditional_effects(run_bayes_1)


#Taking the conditonal effects plot for men and women and adding the observed data one
run_plot2 <- conditional_effects(run_bayes_1, resolution = 10000)[6] %>% 
  as.data.frame()

#Plotting
run_plot2 %>% 
  ggplot()+
  geom_lineribbon(aes(x = air_temp.category.effect1__, 
                      y = air_temp.category.estimate__/60, 
                      ymin = air_temp.category.lower__/60, 
                      ymax = air_temp.category.upper__/60,
                      group = air_temp.category.effect2__))+
  geom_point(data = run_men %>% 
               group_by(category, air_temp) %>%
               mutate(mu = mean(run),
                      air_temp.category.effect2__ = category),
             aes(x = air_temp, y = mu/60,
                 colour = category),
             shape = 3)+
  facet_wrap(~ air_temp.category.effect2__)


run_plot2 %>% 
  ggplot()+
  geom_lineribbon(aes(x = air_temp.category.effect1__, 
                      y = air_temp.category.estimate__/60, 
                      ymin = air_temp.category.lower__/60, 
                      ymax = air_temp.category.upper__/60,
                      group = air_temp.category.effect2__,
                      fill = air_temp.category.effect2__),
                   show.legend = TRUE,
              alpha = 0.75)+
  geom_line(aes(x = air_temp.category.effect1__,
                y = air_temp.category.estimate__/60,
                group = air_temp.category.effect2__),
            color = "black") +
  theme_classic()+
  labs(x = 'Air Temperature (celcius)',
       y = 'Run Time (minutes)')+
  scale_y_continuous(limits = c(32,44),
                     breaks = c(32,34,36,38,40,42,44))+
  scale_x_continuous(limits = c(10,38),
                     breaks = c(10,14,18,22,26,30,34,38))+
  scale_fill_manual(values = c('Elite Men' = "#0072B2", "Elite Women" = '#E69F00'))+
  guides(fill = guide_legend(title = "Sex"))+
  theme(legend.position = c(0.3,0.85))

ggsave(filename = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images/run.png',
       width = 8,
       height = 6,
       dpi = 300)

