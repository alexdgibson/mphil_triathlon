# Modelling in Bayes for the effect of temperature on swim time
# Mr. Alexander D Gibson
# 3rd July 2023

#load packages
library(tidyverse)
library(brms)
library(car)
library(tidybayes)
library(modelr)
library(splines)



# The first model in Bayes for swim time. Copying the frequentest model and imputing it as the formula
swim_bayes_1 <- brm(formula = swim ~ bs(water_temp, knots = c(18,19,20))*category + wet_suit + age +
                    (1|program_id_scaled) + (1|athlete_id_scaled),
                    family = negbinomial(link = 'log'),
                    chains = 4,
                    cores = 8,
                    iter = 2000,
                    control = list(adapt_delta = 0.95,
                                   max_treedepth = 12),
                    seed = 123,
                    data = swim_men)

swim_bayes_2 <- brm(formula = swim ~ bs(water_temp, knots = c(18,19,20))*category + wet_suit + age + category,
                    family = negbinomial(link = 'log'),
                    chains = 2,
                    cores = 8,
                    iter = 1000,
                    control = list(adapt_delta = 0.95),
                    seed = 123,
                    data = swim_men)


#Saving the model as an RDS
save(swim_bayes_1, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/swim_bayes.RData')
save(swim_men, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/swim_df.RData')

swim_bayes_1 <- load(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/data/swim_bayes.RData')
# Posterior predictive checks
pp_check(swim_bayes_1, re_formula = NULL)
# Summary
summary(swim_bayes_1)
# Random effects
ranef(swim_bayes_1)
# Marginal effects
conditional_effects(swim_bayes_1)

# Posterior predictive checks
pp_check(swim_bayes_2, re_formula = NULL)
# Summary
summary(swim_bayes_2)
# Random effects
ranef(swim_bayes_2)
# Marginal effects
conditional_effects(swim_bayes_2)


#Taking the conditonal effects plot for men and women and adding the observed data one
swim_plot<- conditional_effects(swim_bayes_1)[5] %>% 
  as.data.frame()

#Plotting

swim_plot %>% 
  ggplot()+
  geom_lineribbon(aes(x = water_temp.category.water_temp, 
                  y = water_temp.category.estimate__, 
                  ymin = water_temp.category.lower__, 
                  ymax = water_temp.category.upper__,
                  group = water_temp.category.effect2__))+
  geom_point(data = swim_men %>% 
               group_by(category, water_temp) %>%
               mutate(mu = mean(swim),
                      water_temp.category.effect2__ = category),
               aes(x = water_temp, y = mu,
                   colour = category),
             shape = 3)+
  facet_wrap(~water_temp.category.effect2__)
  
# Plotting a final visual for swim model

swim_plot %>% 
  select(water_temp.category.effect1__, water_temp.category.estimate__, water_temp.category.lower__, water_temp.category.upper__, water_temp.category.effect2__) %>% 
  ggplot()+
  geom_lineribbon(aes(x = water_temp.category.effect1__, 
                      y = water_temp.category.estimate__/60, 
                      ymin = water_temp.category.lower__/60, 
                      ymax = water_temp.category.upper__/60,
                      fill = water_temp.category.effect2__,
                      group = water_temp.category.effect2__),
                  show.legend = TRUE,
                  alpha = 0.75)+
  geom_line(aes(x = water_temp.category.effect1__,
                y = water_temp.category.estimate__/60,
            group = water_temp.category.effect2__),
            color = "black")+
  geom_segment(aes(x = 15.9, xend = 15.9, y = 16, yend = 26),
               linetype = "dotted",
               color = "grey35") +
  geom_segment(aes(x = 20.0, xend = 20.0, y = 16, yend = 26),
               linetype = "dotted",
               color = "grey35") +
  theme_classic()+
  labs(x = 'Water Temperature (celcius)',
       y = 'Swimming Time (minutes)')+
  scale_y_continuous(limits = c(15,30),
                     breaks = c(16,20,24,28,32))+
  scale_x_continuous(limits = c(14,31))+
  scale_fill_manual(values = c('Elite Men' = "#0072B2", "Elite Women" = '#E69F00'))+
  guides(fill = guide_legend(title = "Sex"))+
  theme(legend.position = c(0.7,0.8))



ggsave(filename = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-temperature/Images/swim.png',
       width = 8,
       height = 6,
       dpi = 300)


# Gather draws for effect of temperature

tdf <- gather_draws(swim_bayes_1, b_categoryEliteWomen, 
                    b_bswater_tempknotsEQc1819201, b_bswater_tempknotsEQc1819202, b_bswater_tempknotsEQc1819203, b_bswater_tempknotsEQc1819204, b_bswater_tempknotsEQc1819205, b_bswater_tempknotsEQc1819206,
                    b_wet_suitmandatory, b_wet_suitoptional) %>%
mutate(effect = .value)

# Plot completion time for each phase

swim_men %>% group_by(program_id_scaled, athlete_id_scaled) %>%
  data_grid(swim, water_temp, category, wet_suit, age) %>%
  add_epred_draws(swim_bayes_1) %>%
  ggplot(aes(x = water_temp, y = swim)) +
  stat_pointinterval(aes(y = .epred), .width = c(0.50,0.95)) +
  labs(x = "Water Temp", y = "Swim") +
  #scale_x_discrete(labels=c("Early Follicular", "Mid-luteal")) +
  theme_bw(base_size = 10) +
  facet_grid(~category) +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())
  #geom_line(data = d, aes(x = Phase, y = Finish_min, group = interaction(ID)), size = .35, alpha = .4)



# Plot difference

tdf %>% ggplot() +
  stat_halfeye(aes(x = effect, fill = after_stat(abs(x) < 1.15)), .width = c(0.66, 0.95)) +
  theme_bw(base_size = 10) +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = "Difference (min)", y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "black") +
  scale_fill_manual(values = c("gray80", "gray35"), name = "ROPE")  +
  geom_vline(xintercept = c(-1.15, 1.15), linetype = "dashed", linewidth = 0.35) +
  guides(fill = F)+
  facet_wrap(~.variable)
