

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
library(loo)
library(lme4)


#set cores to 8 for faster comp times through loo
options(mc.cores = 8)

# The first model in Bayes for swim time. Copying the frequentest model and imputing it as the formula
# bike_bayes_1 <- brm(formula = bike ~ air_temp*category + swim_cwc*category + age +
#                       (1|program_id) + (1|athlete_id),
#                     family = negbinomial(link = 'log'),
#                     chains = 2,
#                     cores = 8,
#                     iter = 1000,
#                     control = list(adapt_delta = 0.95,
#                                    max_treedepth = 10),
#                     seed = 123,
#                     data = bike_men)

bike_bayes_2a <- brm(formula = bike ~ air_temp*category + swim_scaled*category + age +
                      (1|program_id) + (1|athlete_id),
                    family = negbinomial(link = 'log'),
                    chains = 8,
                    cores = getOption('mc.cores', 8),
                    iter = 20000,
                    control = list(adapt_delta = 0.99,
                                   max_treedepth = 10),
                    seed = 123,
                    data = bike_men)


#Saving the model as an RDS
save(bike_bayes_2, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/bike_model_8_10000.RData')

# Frequentist fitting to check for centering and scaling swim as a predictor

bikefit1 <- lmer(formula = bike ~ air_temp*category + swim_scaled*category + age +
                 (1|program_id) + (1|athlete_id), data = bike_men)

bikefit2 <- lmer(formula = bike ~ air_temp*category + swim*category + age +
                   (1|program_id) + (1|athlete_id), data = bike_men)

bikefit3 <- lmer(formula = bike ~ air_temp*category + swim_gscaled*category + age +
                   (1|program_id) + (1|athlete_id), data = bike_men)


# Comparing the two linear models with swim and swim that is centred and scaled
anova(bikefit1, bikefit2, bikefit3)

summary(bikefit1)
summary(bikefit2)
summary(bikefit3)

# Comparing a 2nd order polynomial (1) and linear model (2) to see what fits better.
loo_compare(loo(bike_bayes_1, save_psis = TRUE),
            loo(bike_bayes_2, save_psis = TRUE))

# Loo compared favored the polynomial model over the linear model

# Posterior predictive checks
pp_check(bike_bayes_1, re_formula = NULL)
# Summary
summary(bike_bayes_1)
# Random effects
ranef(bike_bayes_1)
# Marginal effects
conditional_effects(bike_bayes_1)


# Posterior predictive checks
pp_check(bike_bayes_2, re_formula = NULL)
# Summary
summary(bike_bayes_2)
# Random effects
ranef(bike_bayes_2)
# Marginal effects
conditional_effects(bike_bayes_2)



#Taking the conditonal effects plot for men and women and adding the observed data one
bike_plot <- conditional_effects(bike_bayes_2)[5] %>% 
  as.data.frame()

#Plotting
bike_plot %>% 
  ggplot()+
  geom_lineribbon(aes(x = air_temp.category.effect1__, 
                      y = air_temp.category.estimate__/60, 
                      ymin = air_temp.category.lower__/60, 
                      ymax = air_temp.category.upper__/60,
                      group = air_temp.category.effect2__))+
  geom_point(data = bike_men %>% 
               group_by(category, air_temp) %>%
               mutate(mu = mean(bike),
                      air_temp.category.effect2__ = category),
             aes(x = air_temp, y = mu/60,
                 colour = category),
             shape = 3)+
  facet_wrap(~ air_temp.category.effect2__)



bike_plot %>% 
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
  labs(x = 'Water Temperature (celcius',
       y = 'Bike Time (minutes)')+
  scale_y_continuous(limits = c(56,72),
                     breaks = c(56,60,64,68,72))+
  scale_x_continuous(limits = c(10,38),
                     breaks = c(10,14,18,22,26,30,34,38))+
  scale_fill_manual(values = c('Elite Men' = "#0072B2", "Elite Women" = '#E69F00'))+
  guides(fill = guide_legend(title = "Sex"))+
  theme(legend.position = c(0.35,0.88))


ggsave(filename = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images/bike.png',
       width = 8,
       height = 6,
       dpi = 300)
