

# Packages
library(brms)
library(tidyverse)
library(modelr)
library(viridis)
library(tidybayes)
library(lme4)
library(splines)
library(emmeans)


# Load data
load("/Users/david/Dropbox/Student projects/2023_Alex Gibson/Ordinal model/men_pos_model2.RData")
load("C:/Users/borgdn/Dropbox/Student projects/2023_Alex Gibson/Ordinal model/men_pos_model2.RData") # Windows

men_pos_model2 = men_pos_model2 %>%
  mutate(program_id = as.integer(program_id),
         age = year - yob)

women_pos_model2 = women_pos_model2 %>%
  mutate(program_id = as.integer(program_id),
         age = year - yob)

# Look at finish pos
names(men_pos_model2)
table(men_pos_model2$position)


#### Binomial regression
# Wrangle
men_pos_model_podium <-
  men_pos_model2 %>%
  mutate(podium = cut(position,
                      breaks = c(0,3,Inf),
                      labels = c('TRUE','FALSE')),
         podium = relevel(podium, ref = 'FALSE'),
         program_id = as.factor(program_id)) %>% 
  select(program_id, age, pos_swim, pos_bike, podium, athlete_id, category, year, position, swim, t1, bike)

women_pos_model_podium <-
  women_pos_model2 %>%
  mutate(podium = cut(position,
                      breaks = c(0,3,Inf),
                      labels = c('TRUE','FALSE')),
         podium = relevel(podium, ref = 'FALSE'),
         program_id = as.factor(program_id)) %>% 
  select(program_id, age, pos_swim, pos_bike, podium, athlete_id, category, year, position, swim, t1, bike)

table(men_pos_model_podium$podium)
table(women_pos_model_podium$podium)

# Filtering so that races included had atleast 10 people in the race


saveRDS(men_pos_model_podium, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/men_pos_model2.RDS")

# Intercept only model
men_podium_fit <- glmer(podium ~ 1 + (1|program_id) + (1|age) + (1|athlete_id),
             family = binomial(link = 'logit'),
             data = men_pos_model_podium)

# # Summary
# summary(men_podium_fit)
# ranef(men_podium_fit)
# confint(men_podium_fit)
# plot(men_podium_fit)
# 
# # Setting a prior for the brms logistic regression model for performance
# # define some priors          
# bprior <- set_prior("gamma(4)",
#                       class = 'sd',
#                       coef = 'program_id')
# 
# # verify that the priors indeed found their way into Stan's model code
# make_stancode(podium ~ pos_swim*pos_bike + (1|program_id),
#               data = men_pos_model_podium, 
#               family = cumulative(),
#               prior = bprior)
# 
# # Testing as bayes
# men_podium_fit <- brm(
#   podium ~ pos_swim*pos_bike + (1|age) + (1|athlete_id),
#   data = men_pos_model_podium,
#   family = bernoulli(link = "logit"),
#   chains = 4,
#   cores = 4,
#   iter = 8000,
#   seed = 123,
#   control = list(adapt_delta = 0.9))
# 
# # Get the Priors for the current model
# prior_summary(men_podium_fit)
# 
# # Posterior predictive checks
# pp_check(men_podium_fit, re_formula = NULL)
# 
# # Summary
# summary(men_podium_fit)
# 
# # Random effects
# ranef(men_podium_fit)
# 
# # Marginal effects
# conditional_effects(men_podium_fit)



# Combining men and womens data sets to be modeled together
pos_model_podium <- rbind(men_pos_model_podium, women_pos_model_podium) %>%
  filter(pos_swim > 0 & pos_bike > 0 & position > 0)

saveRDS(pos_model_podium, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/pos_model_podium.RDS")


# Full fit without random effects
men_podium_fit <- glm(formula = podium ~ pos_swim*pos_bike*category + bs(age, df = 3)*category,
                      family = binomial(link = 'logit'),
                      data = pos_model_podium)

save(men_podium_fit, file = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/men_podium_fit_2023.RData")


summary(men_podium_fit)


# # Full model
# men_podium_fit <- glmer(podium ~ pos_swim_s*pos_bike_s + age_s,
#              family = binomial(link = 'logit'),
#              data = men_pos_model_podium,
#              control = glmerControl(optimizer = "bobyqa",
#                                     optCtrl = list(maxfun = 100000)),
#              verbose = 1)

# Summary
summary(men_podium_fit)
confint(men_podium_fit)

#confint(fit)
plot(men_podium_fit)

 men_podium_fit_plot <- cbind(pos_model_podium, predict(men_podium_fit, type = 'response'))
# 
# men_podium_fit_plot %>%
#   # group_by(pos_swim,pos_bike) %>%
#   # summarise(med = median(pred, na.rm = T)) %>%
#   ggplot()+
#   geom_tile(aes(x = as.integer(pos_swim), y = as.integer(pos_bike), fill = predict(men_podium_fit, type = "response")))+ # Tile aligns centrally
#   theme_minimal(base_size = 12)+
#   theme(panel.grid = element_blank(),
#         strip.text = element_text(size = 12),
#         panel.background=element_rect(fill="gray70", colour="gray70"))+
#   scale_y_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+ # expand = c(0,0)
#   scale_x_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+
#   scale_fill_viridis_c(option = 'A')+
#   labs(x = '\nEnd Swim Position',
#        y = 'End Bike Position\n')+
#   coord_cartesian(xlim = c(1,20),
#                   ylim = c(1,20))


# Back transforing the scaling and centering pos swim and pos bike to plot
men_pos_model_podium <- men_pos_model_podium %>%
  mutate(pos_swim_s = men_pos_model_podium$pos_swim_s * attr(men_pos_model_podium$pos_swim_s, 'scaled:scale') + attr(men_pos_model_podium$pos_swim_s, 'scaled:center'),
         pos_bike_s = men_pos_model_podium$pos_bike_s * attr(men_pos_model_podium$pos_bike_s, 'scaled:scale') + attr(men_pos_model_podium$pos_bike_s, 'scaled:center'),
         age_s = men_pos_model_podium$age_s * attr(men_pos_model_podium$age_s, 'scaled:scale') + attr(men_pos_model_podium$age_s, 'scaled:center'))


# Add fitted values way #2
podium_fit_plot = pos_model_podium %>%
  group_by(athlete_id, category) %>% 
  data_grid(pos_swim, pos_bike, program_id, age)

podium_fit_plot <- cbind(podium_fit_plot, predict(men_podium_fit, podium_fit_plot, type = "response", allow.new.levels = TRUE)) %>%
  as.data.frame() %>% 
  rename(pred = ...6)

summ_podium_fit_plot <- podium_fit_plot %>% 
  group_by(pos_swim, pos_bike, category, age) %>% 
  summarise(pred = mean(pred))

summ_podium_fit_plot %>%
  ggplot()+
  geom_tile(aes(x = as.integer(pos_swim), y = as.integer(pos_bike), fill = pred))+
  theme_minimal(base_size = 12)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  scale_y_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+ # expand = c(0,0)
  scale_x_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+
  scale_fill_viridis_c(option = 'A', direction = 1)+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n',
       fill = 'Probability')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))+
  facet_wrap(~category)


ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/Probpodium_prob_age.png',
       dpi = 300,
       width = 10,
       height = 5)


# Probability of podium against age
summ_podium_fit_plot %>%
  group_by(pos_swim, pos_bike, category, age) %>% 
  summarise(mu = mean(pred, na.rm = FALSE)) %>% 
  filter(age > 17 & age < 41) %>% 
  group_by(age) %>% 
  ggplot(aes(x = age, y = mu))+
  geom_point()+
  geom_line()+
  facet_wrap(~category)

# Probabilty of podium against age again using emmeans

emm_options(rg.limit = Inf)

(age_refgrid <- list(pos_swim = seq(1,20, by = 1),
                 pos_bike = seq(1,20, by = 1),
                 category = c('Elite Men','Elite Women'),
                 age = seq(18,40, by =1))) # take the min and max values for age in the dataset

age_mar_ef <- emmip(men_podium_fit, ~ age|category, at = age_refgrid, CIs = T, plotit = F, type = "response")

age_mar_ef %>%
  ggplot()+
  geom_line(aes(y = yvar,
                x = age))+
  geom_ribbon(aes(ymin = LCL, ymax = UCL,
                  x = age),
              alpha = 0.25)+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank())+
  scale_y_continuous(limits = c(0, 0.125), 
                     breaks = c(0,0.025,0.050,0.075,0.100, 0.125),
                     labels = c('0.000','0.025','0.050','0.075','0.100','0.125')) +
  scale_x_continuous(limits = c(18, 40), 
                     breaks = c(20,25,30,35,40))+
  labs(x = 'Age',
       y = 'Probability')+
  facet_wrap(~category)

# Save the plot
ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/age_prob.png',
       dpi = 300,
       width = 8,
       height = 4)


# Visualising the drop in probability in a different way to show the change in probability for swim and bike positions

# The probabilty distribution of cycling when swim position is fixed at first place
# summ_podium_fit_plot %>% 
#   filter(pos_swim == 1 & pos_bike < 21) %>% 
  ggplot()+
  geom_point(aes(x = pos_bike, y = yvar),
             shape = 15,
             data = prob_mar_ef %>% 
                    filter(pos_swim == 1 & pos_bike < 21))+
  geom_line(aes(x = pos_bike, y = yvar), 
            data = prob_mar_ef %>% 
              filter(pos_swim == 1 & pos_bike < 21))+
    geom_point(aes(x = pos_bike, y = yvar), 
               shape = 16,
               data = prob_mar_ef %>% 
                 filter(pos_swim == 5 & pos_bike < 21))+
    geom_line(aes(x = pos_bike, y = yvar), 
              data = prob_mar_ef %>% 
                filter(pos_swim == 5 & pos_bike < 21))+
    geom_point(aes(x = pos_bike, y = yvar), 
               shape = 17,
               data = prob_mar_ef %>% 
                 filter(pos_swim == 10 & pos_bike < 21))+
    geom_line(aes(x = pos_bike, y = yvar), 
              data = prob_mar_ef %>% 
                filter(pos_swim == 10 & pos_bike < 21))+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank()) + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c('0','0.2','0.4','0.6','0.8','1.0')) +
  scale_x_continuous(limits = c(1, 20), 
                     breaks = c(1,5,10,15,20))+
  labs(x = 'End Bike Positions',
       y = 'Probability')+
  facet_wrap(~category)

# Save the plot
ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/bike_prob.png',
       dpi = 300,
       width = 6,
       height = 3)

# The probabilty distribution of swimming when bike position is fixed at first place
  ggplot()+
  geom_point(aes(x = pos_swim, y = yvar), 
             shape = 15,
             data = prob_mar_ef %>% 
               group_by(pos_swim, pos_bike) %>% 
               filter(pos_bike == 1 & pos_swim < 21))+
  geom_line(aes(x = pos_swim, y = yvar), 
            data = prob_mar_ef %>% 
              group_by(pos_swim, pos_bike) %>% 
              filter(pos_bike == 1 & pos_swim < 21))+
  geom_point(aes(x = pos_swim, y = yvar), 
             shape = 16,
             data = prob_mar_ef %>% 
               group_by(pos_swim, pos_bike) %>% 
               filter(pos_bike == 5 & pos_swim < 21))+
  geom_line(aes(x = pos_swim, y = yvar),
            data = prob_mar_ef %>% 
              group_by(pos_swim, pos_bike) %>% 
              filter(pos_bike == 5 & pos_swim < 21))+
  geom_point(aes(x = pos_swim, y = yvar), 
             shape = 17,
             data = prob_mar_ef %>% 
               group_by(pos_swim, pos_bike) %>% 
               filter(pos_bike == 10 & pos_swim < 21))+
  geom_line(aes(x = pos_swim, y = yvar),
            data = prob_mar_ef %>% 
              group_by(pos_swim, pos_bike) %>% 
              filter(pos_bike == 10 & pos_swim < 21))+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank()) + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c('0','0.2','0.4','0.6','0.8','1.0')) +
  scale_x_continuous(limits = c(1, 20), 
                     breaks = c(1,5,10,15,20))+
  labs(x = 'End Swim Positions',
       y = 'Probability')+
  facet_wrap(~category)

# Saving the plot
ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/swim_prob_age.png',
       dpi = 300,
       width = 6,
       height = 3)


# Age plot for age of men and women across the five years

pos_model_podium %>% 
  filter(age >17 & age < 41) %>% 
  ggplot()+
  geom_boxplot(aes(x = year, y = age, group = year))+
  facet_wrap(~category)


# Probabiltiy reporting for swimming = 1
summ_podium_fit_plot %>% 
  filter(pos_swim == 1 & pos_bike < 21 & category == 'Elite Men') %>%
  head(10)

summ_podium_fit_plot %>% 
  filter(pos_swim == 10 & pos_bike < 21 & category == 'Elite Women') %>%
  head(10)

# Probabiltiy reporting for bike = 1
summ_podium_fit_plot %>% 
  filter(pos_bike == 1 & pos_swim < 21 & category == 'Elite Men') %>%
  head(10)

summ_podium_fit_plot %>% 
  filter(pos_bike == 10 & pos_swim < 21 & category == 'Elite Women') %>%
  head(10)


# Summary statistics
# Total Races for men and women
pos_model_podium %>% 
  group_by(category) %>% 
  distinct(program_id) %>% 
  summarise(n = n())

# Total athletes for men and women
pos_model_podium %>% 
  group_by(category) %>% 
  distinct(athlete_id) %>% 
  summarise(n = n())

# Total athletes per race for men and women
pos_model_podium %>% 
  group_by(category, program_id) %>% 
  distinct(athlete_id) %>% 
  summarise(n = n()) %>%
  reframe(mu = median(n),
            IQR = quantile(n))

# Average age of people
pos_model_podium %>% 
  select(age, athlete_id, year, category) %>% 
  group_by(category, year) %>%
  na.omit() %>% 
  distinct(athlete_id, .keep_all = TRUE) %>%
  reframe(mu = median(age),
          iqr25 = quantile(age, probs = c(0.25)),
          iqr75 = quantile(age, probs = c(0.75))) %>% 
  ggplot()+
  geom_point(aes(x = year, y = mu), size = 2.5)+
  geom_errorbar(aes(ymin = iqr25, ymax = iqr75, x = year), width = 0.6)+
  theme_minimal(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.grid = element_blank())+
  scale_y_continuous(limits = c(20, 34), 
                     breaks = c(20,24,28,32))+
  labs(x = 'Years',
       y = 'Age')+
  facet_wrap(~category)

# Saving the plot
ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/age_graph.png',
       dpi = 300,
       width = 6,
       height = 3)

# Age for all years
pos_model_podium %>% 
  select(age, athlete_id, year, category) %>% 
  group_by(category) %>%
  na.omit() %>% 
  distinct(athlete_id, .keep_all = TRUE) %>%
  reframe(mu = median(age),
          iqr25 = quantile(age, probs = c(0.25)),
          iqr75 = quantile(age, probs = c(0.75))) 


# Determining how many races were in each year for men and women
pos_model_podium %>% 
  group_by(category, year) %>%
  reframe(n = n_distinct(program_id)) %>% 
  ggplot()+
  geom_col(aes(x = year, y = n, fill = category), position = 'dodge')+
  scale_fill_manual(values = c('black', 'grey60'))+
  theme_classic(base_size = 12)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.6,0.8))+
  labs(x = 'Year',
       y = 'Race Count')

ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/Race_count_perf.png',
       dpi = 300,
       width = 5,
       height = 3)
  

# Examining missing age data
pos_model_podium %>% 
  select(program_id, age, athlete_id) %>% 
  mutate(isna = is.na(age)) %>% 
  filter(isna == TRUE) %>% 
  reframe(n= n_distinct(program_id))

# Determining the median and IQR for the amount of times an athlete raced
pos_model_podium %>% 
  group_by(category) %>%
  count(athlete_id) %>% 
  reframe(mu = median(n),
         IQR = quantile(n))

# Number of races for each year for each sex
pos_model_podium %>% 
  group_by(category, year) %>%
  reframe(n = n_distinct(program_id))


### Parameter Estimtes 
summary(men_podium_fit)
confint(men_podium_fit)

# Probabilty heatmap matrix for top 20 positions when age is at the median
(prob_refgrid <- list(pos_swim = seq(1,20, by = 1),
                     pos_bike = seq(1,20, by = 1),
                     category = c('Elite Men','Elite Women'),
                     age = 26)) # take the min and max values for age in the dataset

prob_mar_ef <- emmip(men_podium_fit, ~ pos_swim|pos_bike|category, at = age_refgrid, CIs = T, plotit = F, type = "response", rg.limit = 18500)


prob_mar_ef %>%
  ggplot()+
  geom_tile(aes(x = as.integer(pos_swim), y = as.integer(pos_bike), fill = yvar))+
  theme_minimal(base_size = 12)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 12))+
  scale_y_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+ # expand = c(0,0)
  scale_x_continuous(limits = c(0,21), breaks = seq(1,20, by = 1))+
  scale_fill_viridis_c(option = 'A', direction = 1)+
  labs(x = '\nEnd Swim Position',
       y = 'End Bike Position\n',
       fill = 'Probability')+
  coord_cartesian(xlim = c(1,20),
                  ylim = c(1,20))+
  facet_wrap(~category)

ggsave(file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob/Probpodium_age_26_new_races.png',
       dpi = 300,
       width = 10,
       height = 5)



# Total number of position combinations
pos_model_podium %>% 
  group_by(category, pos_swim, pos_bike) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>%
  filter(category == "Elite Women") %>% view()



# Checking female age to see if there are one athlete that wins most
pos_model_podium %>% 
  filter(category == "Elite Women" & podium == TRUE) %>%
  group_by(athlete_id) %>% 
  count() %>% 
  view()


#### End