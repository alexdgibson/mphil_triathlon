

# Summarize parameter estimates
# Borg DN
# 2023-07-06

# Packages
library(tidybayes)
library(tidyverse)
library(brms)
library(emmeans)

# Summary
summary(swim_bayes_1)

# Pull a couple of parameters example
gather_draws(swim_bayes_1,
             b_bswater_tempknotsEQc1819201,
             b_bswater_tempknotsEQc1819202,
             b_bswater_tempknotsEQc1819206,
             b_categoryEliteWomen,
             b_wet_suitmandatory,
             b_wet_suitoptional,
             `b_bswater_tempknotsEQc1819201:categoryEliteWomen`) %>% # note the quotations needed for interactions
  mutate(effect = .value) %>%
  ggplot(aes(y = .variable))+
  stat_halfeye(aes(x = effect),
               .width = c(2/3, 0.95))+
  #facet_wrap(~.variable)+
  geom_vline(xintercept = 0, colour = 'maroon')

# Pull temp parameters: collapse
temp_effect <-
  gather_draws(swim_bayes_1,
             b_bswater_tempknotsEQc1819201,
             b_bswater_tempknotsEQc1819202,
             b_bswater_tempknotsEQc1819203,
             b_bswater_tempknotsEQc1819204,
             b_bswater_tempknotsEQc1819205,
             b_bswater_tempknotsEQc1819206) %>%
  pivot_wider(names_from = .variable,
              values_from = .value) %>%
  rowwise() %>%
  mutate(effect = sum(across(starts_with("b_bswater_")))/6) # Divide by 6 takes the average across the 6 temp coefficents

swim_effect <- 
  temp_effect %>%
  ggplot()+
  stat_halfeye(aes(x = effect),
               .width = c(2/3, 0.95))+
  geom_vline(xintercept = 0, colour = 'maroon')+
  theme_classic()+
  labs(x = 'Effect',
       y = '',
       title = 'Swim')

# Probability that the effect is less than zero (0 = null)
mean(temp_effect$effect < 0)

# Summary of temp effect (average of the 6 coefficients)
mean_qi(temp_effect$effect, .width = c(2/3,0.95))


# Sex effect
gather_draws(swim_bayes_1,
             b_bswater_tempknotsEQc1819206) %>%
  mean_qi(.value<0)





# Bike results
# Summary
summary(bike_bayes_2)

# Pull a couple of parameters example
gather_draws(bike_bayes_2,
             b_air_temp,
             b_categoryEliteWomen,
             b_swim_scaled,
             `b_air_temp:categoryEliteWomen`) %>% # note the quotations needed for interactions
  mutate(effect = .value) %>%
  ggplot()+
  stat_halfeye(aes(x = effect),
               .width = c(1/3, 0.95))+
  facet_wrap(~.variable)+
  geom_vline(xintercept = 0, colour = 'maroon')

# Pull temp parameters: collapse
temp_effect_bike <-
  gather_draws(bike_bayes_2,
               b_air_temp) %>%
  pivot_wider(names_from = .variable,
              values_from = .value) %>%
  rowwise() %>%
  mutate(effect = b_air_temp) # Divide by 6 takes the average across the 6 temp coefficents

bike_effect <- 
  temp_effect_bike %>%
  ggplot()+
  stat_halfeye(aes(x = effect),
               .width = c(2/3, 0.95))+
  geom_vline(xintercept = 0, colour = 'maroon')+
  theme_classic()+
  labs(x = 'Effect',
       y = '',
       title = 'Bike')

# Probability that the effect is less than zero (0 = null)
mean(temp_effect_bike$effect<0)

# Summary of temp effect
mean_qi(temp_effect_bike$effect, .width = c(2/3,0.95))




# RUn results
# Summary
summary(run_bayes_1)

# Pull a couple of parameters example
gather_draws(run_bayes_1,
             b_polyair_tempdegreeEQ21,
             b_polyair_tempdegreeEQ22,
             `b_polyair_tempdegreeEQ21:categoryEliteWomen`,
             `b_polyair_tempdegreeEQ22:categoryEliteWomen`) %>% # note the quotations needed for interactions
  mutate(effect = .value) %>%
  ggplot()+
  stat_halfeye(aes(x = effect),
               .width = c(1/3, 0.95))+
  facet_wrap(~.variable)+
  geom_vline(xintercept = 0, colour = 'maroon')

# Pull temp parameters: collapse
temp_effect_run <-
  gather_draws(run_bayes_1,
               b_polyair_tempdegreeEQ21,
               b_polyair_tempdegreeEQ22) %>%
  pivot_wider(names_from = .variable,
              values_from = .value) %>%
  rowwise() %>%
  mutate(effect = sum(across(starts_with("b_polyair_")))/2) # Divide by 6 takes the average across the 6 temp coefficents

run_effect <-
  temp_effect_run %>%
  ggplot()+
  stat_halfeye(aes(x = effect),
               .width = c(2/3, 0.95))+
  geom_vline(xintercept = 0, colour = 'maroon')+
  theme_classic()+
  labs(x = 'Effect',
       y = '',
       title = 'Run')

# Probability that the effect is less than zero (0 = null)
mean(temp_effect_run$effect<0)

# Summary of temp effect (average of the 2 coefficients)
mean_qi(temp_effect_run$effect, .width = c(2/3,0.95))




cowplot::plot_grid(swim_effect, bike_effect, run_effect,
                   nrow = 1)




