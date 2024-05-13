### Analysis For Segment contributions ###

library(tidyverse)
library(modelr)
library(zoib)
library(tidybayes)
library(rjags)
library(R2jags)
library(cowplot)


beta_men <- complete_men %>% 
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & total_time > 0) %>% 
  group_by(athlete_id) %>% 
  mutate(s = sum(swim),
         b = sum(bike),
         r = sum(run),
         t = sum(t1) + sum(t2),
         total = (s + b + r + t)) %>%
  select(athlete_id, year, s, b, r, t, total) %>%
  mutate(sp = (s/total),
         bp = (b/total),
         rp = (r/total),
         tp = (t/total),
         tt = sp + bp + rp + tp) %>% 
  ungroup() %>% 
  select(athlete_id, sp, bp, rp, tp, tt, year)

beta_women <- complete_women %>% 
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0 & total_time > 0) %>% 
  group_by(athlete_id) %>% 
  mutate(s = sum(swim),
         b = sum(bike),
         r = sum(run),
         t = sum(t1) + sum(t2),
         total = (s + b + r + t)) %>%
  select(athlete_id, year, s, b, r, t, total) %>%
  mutate(sp = (s/total),
         bp = (b/total),
         rp = (r/total),
         tp = (t/total),
         tt = sp + bp + rp + tp) %>% 
  ungroup() %>% 
  select(athlete_id, sp, bp, rp, tp, tt, year)



beta_men_test <- beta_men %>% sample_n(size = 1000)
beta_men_mean <- beta_men %>% group_by(year) %>% summarise(mean_swim_men = mean(sp),
                                                           mean_bike_men = mean(bp),
                                                           mean_run_men = mean(rp))

beta_women_test <- beta_women %>% sample_n(size = 1000)
beta_women_mean <- beta_women %>% group_by(year) %>% summarise(mean_swim_women = mean(sp),
                                                               mean_bike_women = mean(bp),
                                                               mean_run_women = mean(rp))

test_fit_men <- zoib(sp ~ year|1|1|1|1,
                 data = beta_men_test,
                 zero.inflation = FALSE,
                 one.inflation = FALSE,
                 joint = FALSE,
                 n.iter = 2000,
                 n.thin = 2,
                 n.burn = 50,
                 n.chain = 2)

test_fit_women <- zoib(sp ~ year|1|1|1|1,
                 data = beta_women_test,
                 zero.inflation = FALSE,
                 one.inflation = FALSE,
                 joint = FALSE,
                 n.iter = 2000,
                 n.thin = 2,
                 n.burn = 50,
                 n.chain = 2)





# Model summary
#summary(test_fit$coeff)

# Model fit statistic (if you want to compare a couple of different models)
#dic.samples(test_fit$MCMC.model, n.iter = 100, thin = 2)


# Plot fitted values for each group
#summary(test_fit_men$ypred)

newdat_men <- data_grid(beta_men_test, year)
newdat_women <- data_grid(beta_women_test, year)

tidy_fitted_men <- test_fit_men$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_men_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_women <- test_fit_women$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_women_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_men = as.data.frame(tidy_fitted_men)
tidy_fitted_women = as.data.frame(tidy_fitted_women)

bayes_swim <- tidy_fitted_men %>%
  group_by(year) %>%
  mean_qi(.value) %>%
  ggplot(aes(x = year, y = .value)) +
  geom_lineribbon(aes(ymin = .lower, max = .upper, fill = 'Men'))+ # geom_ribbon()
  #stat_halfeye(colour = '#0072B2')+
  geom_lineribbon(data = tidy_fitted_women %>% group_by(year) %>% mean_qi(.value), 
               aes(x = year, y = .value, ymin = .lower, max = .upper, fill = 'Women'))+
  theme_classic()+
  scale_y_continuous(breaks = c(0.156, 0.159, 0.162, 0.165),
                     labels = c("15.6", "15.9", "16.2", "16.5"))+
  labs(title = 'Swim',
       x = 'Year',
       y = '%')+
  scale_fill_manual(values = c("#0072B2AA", "#E69F00AA"), name="")+
  scale_colour_manual(values = c('black', 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.position = "none")


#### END


#### Bike ####

test_fit_men_bike <- zoib(bp ~ year|1|1|1|1,
                      data = beta_men_test,
                      zero.inflation = FALSE,
                      one.inflation = FALSE,
                      joint = FALSE,
                      n.iter = 2000,
                      n.thin = 2,
                      n.burn = 50,
                      n.chain = 2)

test_fit_women_bike <- zoib(bp ~ year|1|1|1|1,
                       data = beta_women_test,
                       zero.inflation = FALSE,
                       one.inflation = FALSE,
                       joint = FALSE,
                       n.iter = 2000,
                       n.thin = 2,
                       n.burn = 50,
                       n.chain = 2)

# Model summary
summary(test_fit$coeff)

# Model fit statistic (if you want to compare a couple of different models)
dic.samples(test_fit$MCMC.model, n.iter = 100, thin = 2)


# Plot fitted values for each group
summary(test_fit_men$ypred)

newdat_men_bike <- data_grid(beta_men_test, year)
newdat_women_bike <- data_grid(beta_women_test, year)

tidy_fitted_men_bike <- test_fit_men_bike$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_men_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_women_bike <- test_fit_women_bike$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_women_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_men_bike = as.data.frame(tidy_fitted_men_bike)
tidy_fitted_women_bike = as.data.frame(tidy_fitted_women_bike)

bayes_bike <- tidy_fitted_men_bike %>%
  group_by(year) %>%
  mean_qi(.value) %>%
  ggplot(aes(x = year, y = .value)) +
  geom_lineribbon(aes(ymin = .lower, max = .upper, fill = 'Men'))+ 
  geom_lineribbon(data = tidy_fitted_women_bike %>% group_by(year) %>% mean_qi(.value), 
                  aes(x = year, y = .value, ymin = .lower, max = .upper, fill = 'Women'))+
  theme_classic()+
  scale_y_continuous(breaks = c(0.515, 0.520, 0.525, 0.530),
                     labels = c("51.5", "52.0", "52.5", "53.0"))+
  labs(title = 'Bike',
       x = 'Year',
       y = '%')+
  scale_fill_manual(values = c("#0072B2AA", "#E69F00AA"), name="")+
  scale_colour_manual(values = c('black', 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.position = c(0.8, 0.8))


#### END





#### RUN ####

test_fit_men_run <- zoib(rp ~ year|1|1|1|1,
                          data = beta_men_test,
                          zero.inflation = FALSE,
                          one.inflation = FALSE,
                          joint = FALSE,
                          n.iter = 2000,
                          n.thin = 2,
                          n.burn = 50,
                          n.chain = 2)

test_fit_women_run <- zoib(rp ~ year|1|1|1|1,
                            data = beta_women_test,
                            zero.inflation = FALSE,
                            one.inflation = FALSE,
                            joint = FALSE,
                            n.iter = 2000,
                            n.thin = 2,
                            n.burn = 50,
                            n.chain = 2)

# Model summary
summary(test_fit$coeff)

# Model fit statistic (if you want to compare a couple of different models)
dic.samples(test_fit$MCMC.model, n.iter = 100, thin = 2)


# Plot fitted values for each group
summary(test_fit_men$ypred)

newdat_men_run <- data_grid(beta_men_test, year)
newdat_women_run <- data_grid(beta_women_test, year)

tidy_fitted_men_run <- test_fit_men_run$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_men_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_women_run <- test_fit_women_run$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_women_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_men_run = as.data.frame(tidy_fitted_men_run)
tidy_fitted_women_run = as.data.frame(tidy_fitted_women_run)

bayes_run <- tidy_fitted_men_run %>%
  group_by(year) %>%
  mean_qi(.value) %>%
  ggplot(aes(x = year, y = .value)) +
  geom_lineribbon(aes(ymin = .lower, max = .upper, fill = 'Men'))+ # geom_ribbon()
  #stat_halfeye(colour = '#0072B2')+
  geom_lineribbon(data = tidy_fitted_women_run %>% group_by(year) %>% mean_qi(.value), 
                  aes(x = year, y = .value, ymin = .lower, max = .upper, fill = 'Women'))+
  theme_classic()+
  scale_y_continuous(breaks = c(0.285, 0.290, 0.295, 0.300, 0.305, 0.310),
                     labels = c("28.5", "29.0", "29.5", "30.0", "30.5", "31.0"))+
  labs(title = 'Run',
       x = 'Year',
       y = '%')+
  scale_fill_manual(values = c("#0072B2AA", "#E69F00AA"), name="")+
  scale_colour_manual(values = c('black', 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.position = "none")


#### END


#### Transition ####

test_fit_men_t <- zoib(tp ~ year|1|1|1|1,
                         data = beta_men_test,
                         zero.inflation = FALSE,
                         one.inflation = FALSE,
                         joint = FALSE,
                         n.iter = 2000,
                         n.thin = 2,
                         n.burn = 50,
                         n.chain = 2)

test_fit_women_t <- zoib(tp ~ year|1|1|1|1,
                           data = beta_women_test,
                           zero.inflation = FALSE,
                           one.inflation = FALSE,
                           joint = FALSE,
                           n.iter = 2000,
                           n.thin = 2,
                           n.burn = 50,
                           n.chain = 2)

# Model summary
summary(test_fit$coeff)

# Model fit statistic (if you want to compare a couple of different models)
dic.samples(test_fit$MCMC.model, n.iter = 100, thin = 2)


# Plot fitted values for each group
summary(test_fit_men$ypred)

newdat_men_t <- data_grid(beta_men_test, year)
newdat_women_t <- data_grid(beta_women_test, year)

tidy_fitted_men_t <- test_fit_men_t$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_men_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_women_t <- test_fit_women_t$ypred %>% gather_draws(ypred[.row]) %>%
  left_join(
    select(beta_women_test, year) %>% mutate(.row = 1:n())
  )

tidy_fitted_men_t = as.data.frame(tidy_fitted_men_t)
tidy_fitted_women_t = as.data.frame(tidy_fitted_women_t)

bayes_t <- tidy_fitted_men_t %>%
  group_by(year) %>%
  mean_qi(.value) %>%
  ggplot(aes(x = year, y = .value)) +
  geom_lineribbon(aes(ymin = .lower, max = .upper, fill = 'Men'))+
  geom_lineribbon(data = tidy_fitted_women_t %>% group_by(year) %>% mean_qi(.value), 
                  aes(x = year, y = .value, ymin = .lower, max = .upper, fill = 'Women'))+
  theme_classic()+
  scale_y_continuous(breaks = c(0.012, 0.014, 0.016, 0.018),
                     labels = c("1.2", "1.4", "1.6", "1.8"))+
  labs(title = 'Transition (T1 + T2)',
       x = 'Year',
       y = '%')+
  scale_fill_manual(values = c("#0072B2AA", "#E69F00AA"), name="")+
  scale_colour_manual(values = c('black', 'black'))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.position = "none")


#### END



## Legend ###
legend_contribution <-
  get_legend(
    bayes_swim + 
      guides(colour = guide_legend(nrow = 1))+
      theme(legend.position = 'bottom')
  )


plot_grid(bayes_swim, bayes_bike, bayes_run, bayes_t,
          ncol = 2)


ggsave(filename = 'bayes_contribution.png',
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete")




### Getting Quantified points from the models ###

tidy_fitted_men %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  view()

tidy_fitted_women %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  filter(year == 1990 | year == 2022) %>% 
  view()

### Bike

tidy_fitted_men_bike %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  filter(year == 1990 | year == 2022) %>% 
  view()

tidy_fitted_women_bike %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  filter(year == 1990 | year == 2022) %>% 
  view()

### Run

tidy_fitted_men_run %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  filter(year == 1990 | year == 2022) %>% 
  view()

tidy_fitted_women_run %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  filter(year == 1990 | year == 2022) %>% 
  view()

### Transition

tidy_fitted_men_t %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  view()

tidy_fitted_women_t %>% 
  group_by(year) %>% 
  mean_qi(.value) %>% 
  view()





beta_men %>% 
  group_by(year) %>%
  mutate(mean_run = mean(rp),
         mean_bike = mean(bp),
         mean_swim = mean(sp)) %>% 
  filter(year > 1999) %>% 
  ggplot(aes(x = year, y = mean_swim))+
  geom_point()+
  #ylim(0.29, 0.31)+
  geom_smooth()


## Segement time
complete_women %>%
  group_by(year) %>%
  summarise(mu = mean(swim, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(y = mu, x = year))


# Model
library(lme4)
mod <- lmer(swim ~ year + (1|athlete_id))

#plot(mod)

hist(residuals(mod))
car::qqPlot(residuals(mod))



