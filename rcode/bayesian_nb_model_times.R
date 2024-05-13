

# Packages
library(brms)
library(tidybayes)
library(modelr)
library(ggplot2)
library(cowplot)

gam_sum1 <-
  gam_sum %>%
  ungroup() %>% filter(year>=2000) %>% 
  mutate(year_s = year-(min(year)),
         mean = ceiling(mu)) # note for AG next time call the transformned _s


ggplot()+
  geom_point(aes(x = year,
                 y = mean),
             data = gam_sum1)

# Model
mod_bayes_men_tt <- brm(
  formula = time ~ year,
  family = negbinomial(link = "log", link_shape = "log"),
  #prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4,
  cores = 8,
  iter = 2000,
  control = list(adapt_delta = 0.99),
  thin = 1,
  seed = 123,
  data = gam_men_tt %>% filter(year>1999))

mod_bayes_men_bike <- brm(
  formula = time ~ year,
  family = negbinomial(link = "log", link_shape = "log"),
  #prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 4,
  cores = 8,
  iter = 2000,
  control = list(adapt_delta = 0.99),
  thin = 1,
  seed = 123,
  data = gam_men_bike %>% filter(year>1999))

#save(fit, file = "model.RData")

# Posterior predictive check
pp_check(mod_bayes, re_formula = NULL, nsamples = 100)

# Summary
summary(mod_bayes)

# Prior summary
prior_summary(mod_bayes)

# Marginal effect
conditional_effects(mod_bayes)

# Slope
# Effect of Condition
plotdata <-
  conditional_effects(mod_bayes, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  as_tibble() %>%
  mutate(ci = "95%")

plotdata_men_tt <-
  conditional_effects(mod_bayes_men_tt, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  as_tibble() %>%
  mutate(ci = "95%")

plot_sum_women_tt <-
  #gam_sum1 %>% filter(sex == 'women' & seg == 'swim' & year>5) %>%
  gam_women_tt %>%
  group_by(year) %>%
  summarise(mu = mean(time, na.rm = T))

plot_sum_men_tt <-
  #gam_sum1 %>% filter(sex == 'women' & seg == 'swim' & year>5) %>%
  gam_men_tt %>%
  group_by(year) %>%
  summarise(mu = mean(time, na.rm = T))
  

pws <- plotdata_women_swim %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_women_swim %>% filter(year > 1999), aes(y = mu/60, x = year), colour = '#E69F00', size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(18,23),
                     n.breaks = 5)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pwb <- plotdata_women_bike %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_women_bike %>% filter(year > 1999), aes(y = mu/60, x = year), colour = '#E69F00', size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(58,72),
                     n.breaks = 6)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pwr <- plotdata_women_run %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_women_run %>% filter(year > 1999), aes(y = mu/60, x = year), colour = '#E69F00', size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(30,45),
                     n.breaks = 5)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pwtt <- plotdata_women_tt %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_women_tt %>% filter(year > 1999), aes(y = mu/60, x = year), colour = '#E69F00', size = 2)+
  labs(x = 'Year',
       y = 'Time (seconds)')+
  theme_classic()+
  # scale_y_continuous(limits = c(6800,8400),
  #                    n.breaks = 5)+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")




pms <- plotdata_men_swim %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_men_swim %>% filter(year > 1999), aes(y = mu/60, x = year), colour = "#0072B2", size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(18,23),
                     n.breaks = 5)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pmb <- plotdata_men_bike %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_men_bike %>% filter(year > 1999), aes(y = mu/60, x = year), colour = "#0072B2", size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(58,72),
                     n.breaks = 6)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pmr <- plotdata_men_run %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_men_run %>% filter(year > 1999), aes(y = mu/60, x = year), colour = "#0072B2", size = 2)+
  labs(x = 'Year',
       y = 'Minutes')+
  theme_classic()+
  scale_y_continuous(limits = c(30,45),
                      n.breaks = 5)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

pmtt <- plotdata_men_tt %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60))+
  geom_point(data = plot_sum_men_tt %>% filter(year > 1999), aes(y = mu/60, x = year), colour = "#0072B2")+
  labs(x = 'Year',
       y = 'Time (seconds)')+
  theme_classic()+
  # scale_y_continuous(limits = c(6800,8400),
  #                    n.breaks = 5)+
  theme(axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))



plot_grid(pms, pws, pmb, pwb, pmr, pwr,
          ncol = 2,
          align = c("hv"),
          scale = 1,
          labels = c('A','B','C','D','E', 'F'))
  

ggsave(filename = 'modeled_times_complete.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 8)

  # Slope for year
year_slope <- gam_men_bike %>%
  group_by(athlete_id) %>%
  data_grid(year) %>%
  add_fitted_draws(mod_bayes_men_bike) %>%
  as.data.frame() %>%
  filter(year %in% c(2000,2022)) %>%
  select(year,.value,.draw)

year_slope %>%
  group_by(year,.draw) %>%
  summarise(.value = mean(.value)) %>%
  pivot_wider(names_from = year, values_from = .value) %>%
  mutate(year_diff = `2000`-`2022`) %>%
  mean_qi(yd = year_diff)


dbfit <- lm(mean ~ year,
            data = gam_sum1 %>% filter(sex == 'women' & seg == 'swim' & year>5))
hist(residuals(dbfit))
car::qqPlot(residuals(dbfit))
summary(dbfit)
confint(dbfit)


conditional_effects(mod_bayes)

