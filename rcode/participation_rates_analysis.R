library(brms)
library(tidybayes)
library(modelr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(ggdist)

# Participation

# Model Beta
mod_bayes_part_rates <- brm(
  formula = perc_w ~ year,
  family = Beta(link = "logit"),
  #prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4,
  cores = 8,
  iter = 2000,
  control = list(adapt_delta = 0.99),
  thin = 1,
  seed = 123,
  data = year_participation_3 %>% filter(year >2002))


fit_part_rates <- lm(formula = perc_w ~ year,
  data = year_participation_3 %>% filter(year > 2002))


#save(fit, file = "model.RData")

# Posterior predictive check
pp_check(mod_bayes_part_rates, re_formula = NULL, nsamples = 100)

# Summary
summary(mod_bayes_part_rates)

# Prior summary
prior_summary(mod_bayes_part_rates)

# Marginal effect
conditional_effects(mod_bayes_part_rates)

# Slope
# Effect of Condition
plotdata_part_rates <-
  conditional_effects(mod_bayes_part_rates, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  as_tibble() %>%
  mutate(ci = "95%")

plotdata_part_rates %>% 
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__*100, ymin = lower__, ymax = upper__))+
  geom_point(data = year_participation_3 %>% filter(year > 1999), aes(x = year, y = perc_w*100))+
  theme_classic()+
  labs(x = 'Year',
       y = '%',
       title = "Bayes Beta Fit")+
  scale_y_continuous(limits = c(25,50),
                     n.breaks = 5)+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave(filename = 'bayes_beta.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete')



year_participation_3 %>% filter(year > 2002) %>% 
  ggplot(aes(x = year, y = perc_w*100))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_classic()+
  labs(x = 'Year',
       y = '%',
       title = 'Frequentist Linear Model')+
  scale_y_continuous(limits = c(25,50),
                     n.breaks = 5)+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave(filename = 'frequentist_lm.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete')





# Model Logistic Reg
mod_bayes_part_rates_logistic <- brm(
  formula = sex ~ year,
  # family = Beta(link = "logit"),
  family = bernoulli(),
  #prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4,
  cores = 8,
  iter = 20000,
  control = list(adapt_delta = 0.99),
  thin = 1,
  seed = 123,
  data = year_participation_bayes %>% filter(year >2002))

year_participation_bayes <- bind_rows(
  complete_men %>%
    filter(status != "DNS") %>% 
    group_by(year) %>% 
    distinct(athlete_id) %>% 
    mutate(sex = "Men"),
  complete_women %>% 
    filter(status != "DNS") %>% 
    group_by(year) %>% 
    distinct(athlete_id) %>% 
    mutate(sex = "Women"))



# Posterior predictive check
pp_check(mod_bayes_part_rates_logistic, re_formula = NULL, nsamples = 1000)

# Summary
summary(mod_bayes_part_rates_logistic)

# Prior summary
prior_summary(mod_bayes_part_rates_logistic)

# Marginal effect
conditional_effects(mod_bayes_part_rates_logistic)

# Slope
# Effect of Condition
plotdata_part_rates_logistic <-
  conditional_effects(mod_bayes_part_rates_logistic, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  as_tibble() %>%
  mutate(ci = "95%")


model_pp <- plotdata_part_rates_logistic %>% 
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__*100, ymin = lower__*100, ymax = upper__*100))+
  geom_point(data = year_participation_3 %>%
               filter(year > 2002), aes(x = year, y = perc_w*100))+
  theme_classic()+
  labs(x = 'Year',
       y = 'Proportion')+
  scale_y_continuous(limits = c(25,50),
                     n.breaks = 5)+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.title.y = element_text(vjust = 5),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


# Comparing Models

waic1 <- waic(mod_bayes_part_rates_logistic)
waic2 <- waic(mod_bayes_part_rates)

summary(waic1)
summary(waic2)

# Final Plot

plot_grid(men_pp, women_pp, participation_plot, model_pp,
          align = c("hv"),
          scale = 1,
          labels = c('A','B','C','D'))

ggsave(filename = 'Participation_complete.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 6)
