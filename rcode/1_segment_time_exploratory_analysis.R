

# Model segment times
# DN Borg
# March, 2023

# Default for plots
theme_set(theme_minimal())


# Load data
d <- readRDS("C:/Users/borgdn/Dropbox/Research projects/Project - Triathlon performance/triathlon-performance/Data/complete_women.RDS")

# Clean
dsub <-
  d %>% filter(status == '' &
                 time_error == FALSE &
                 swim >0 &
                 year >=1990 &
                 swim <10000) %>%
  mutate(year_s = year-min(year))


# Summary of swim times
dsum <-
  dsub %>%
  group_by(year) %>%
  summarise(mu = mean(swim, na.rm = T))

dsum


#### Exploratory plots
dsub %>%
  ggplot() + geom_histogram(aes(x = swim-min(swim)), colour = 'white', bins = 100)

dsub %>%
  ggplot(aes(x = year, y = swim))+
  geom_hex()

dsum %>%
  ggplot(aes(x = year, y = mu))+
  geom_point()

dsub %>%
  ggplot(aes(x = year, y = swim))+
  geom_point(alpha = 0.05, size = 3)


#### Modelling
# Model 1: Gaussian response
fit <- lmer(swim ~ year_s + (1|program_id) + (1|athlete_id),
            data = dsub)

res <- residuals(fit) %>%
  as.data.frame()

res %>% ggplot() + geom_histogram(aes(.), colour = 'white', bins = 100)

qqPlot(res$.) # Residuals are skewed, much taller than a 'normal' response




# Model 2: Negative binomial - response distribution for over dispersion
fit_nb <- glmmTMB(swim ~ ns(year_s,2) + (1|athlete_id), # + (1|program_id)
                  data = dsub,
                  ziformula = ~0,
                  family = nbinom2)

summary(fit_nb)

# Plot fitted values
(refgrid <- list(year_s = seq(min(dsub$year_s),
                              max(dsub$year_s),
                              by = 1)))

mar_ef <- emmip(fit_nb,
                ~ year_s,
                at = refgrid,
                CIs = T,
                plotit = F,
                type = 'response')


mar_ef %>%
  ggplot(aes(x = year_s+min(dsub$year), y = yvar)) +
  geom_ribbon(aes(y = yvar, ymin = LCL, ymax = UCL), alpha = 0.5)+
  geom_line()+
  geom_point(data = dsum, aes(x = year, y = mu), colour = 'red')+
  scale_x_continuous(n.breaks = 8)




# Model 3: General additive model (GAM)
fit_gam <- gam(swim ~ s(year_s),
               family = nb(),
               data = dsub)

plot(fit_gam, pages = 1)

summary(fit_gam)

# Plot fitted values
(refgrid <-
    list(year_s = seq(min(dsub$year_s),
                      max(dsub$year_s),
                      by = 1)))

mar_ef <- emmip(fit_gam,
                ~ year_s,
                at = refgrid,
                CIs = T,
                plotit = F,
                type = 'response')

mar_ef %>%
  ggplot(aes(x = year_s+min(dsub$year), y = yvar)) +
  geom_ribbon(aes(y = yvar, ymin = LCL, ymax = UCL), alpha = 0.25)+
  geom_line()+
  #geom_point()+
  geom_point(data = dsum, aes(x = year, y = mu), colour = 'red')+
  ylim(1000,1400) -> p1
p1


# Add subject level random effects to GAM
fit_subj_rand <- gam(swim ~ s(year_s) + s(athlete_id, bs = 're'), # by = year
                     family = nb(),
                     data = dsub)

plot(fit_subj_rand, pages = 1)

summary(fit_subj_rand)

# Plot fitted values
(refgrid <-
    list(year_s = seq(min(dsub$year_s),
                      max(dsub$year_s),
                      by = 1)))

mar_ef_rand <-
  emmip(fit_subj_rand,
        ~ year_s,
        at = refgrid,
        CIs = T,
        plotit = F,
        type = 'response')

# Add to plot
p1 +
  geom_ribbon(data = mar_ef_rand,
              aes(y = yvar, ymin = LCL, ymax = UCL),
              alpha = 0.15,
              fill = 'red')+
  geom_line(data = mar_ef_rand,
            aes(y = yvar, x = year_s+min(dsub$year)),
            linetype = 5)


# Compare most useful models
BIC(fit_gam) # GAM no random effects
BIC(fit_subj_rand) # GAM with subject level random effects
BIC(fit_nb) # NB, linear setting with subject level random effects





#### Look into subject responses
dsubj <- dsub %>%
  group_by(athlete_id) %>%
  mutate(n=n()) %>%
  filter(n>60) %>% # select athletes with more than 60 races
  ungroup() %>%
  group_by(athlete_id,
           year) %>%
  summarise(mu_subj = mean(swim, na.rm = T))
  
dsubj %>%
  ggplot(aes(x = year, y = mu_subj, group = athlete_id))+
  geom_point()+
  facet_wrap(~athlete_id, scales = 'free_y')+
  stat_smooth(se = 0, method = 'lm', size = 0.5) +
  stat_smooth(se = 0, colour = 'red', size = 0.5)+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#ggsave(file = 'subj.pdf', dpi = 900)


dsubj %>%
  ggplot(aes(x = year, y = mu_subj, group = athlete_id))+
  geom_point()+
  stat_smooth(se = 0, method = 'lm')


