
# Modelling segment times across 2000 to 2022
# 17 July 2023
# Alexander D Gibson

library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(ggplot2)
library(cowplot)
library(lme4)
library(emmeans)
library(marginaleffects)
library(forcats)
library(MASS)



# Modelling all segment times for men and women together
# Only athletes which start the race, completed the segment time and did not have any.

# Joining men and women data together to model segment and total times

model_times_df <- rbind(complete_men_yobll, complete_women_yobll) %>% 
  dplyr::select(swim, bike, run, total_time, year, category, status, time_error, program_id, athlete_id, position) %>% 
  filter(year > 1999 & time_error == FALSE & status != "DNS") %>% 
  dplyr::select(swim, bike, run, total_time, year, category, position, program_id, athlete_id) %>% 
  mutate(year_s = year - min(year)) %>% 
  group_by(program_id) %>% 
  mutate(race_size = n_distinct(athlete_id)) %>% 
  ungroup()
  

model_times_df %>% 
  group_by(program_id) %>% 
  mutate(race_size = n_distinct(athlete_id)) %>% 
  filter(swim < 5000 & swim > 600) %>%
  filter(position < 21) %>% 
  ggplot()+
  geom_histogram(aes(x = swim), binwidth = 5)+
  facet_wrap(~category)

model_times_df %>% 
  filter(bike < 6000 & bike > 2500) %>% 
  ggplot()+
  geom_histogram(aes(x = bike), binwidth = 20)

model_times_df %>% 
  filter(run < 4000 & run > 1400 & position < 21) %>%
  ggplot()+
  geom_histogram(aes(x = run), binwidth = 60)+
  facet_grid(~category)

model_times_df %>% 
  filter(total_time < 10800 & total_time > 2500) %>% 
  ggplot()+
  geom_histogram(aes(x = total_time), binwidth = 20)


# Modelling in Bayes
# Swim time models for women and men
swim_model_finalw <- brm(
  formula = swim ~ year,
  family = negbinomial(link = "log"),
  chains = 8,
  cores = 8,
  iter = 4000,
  control = list(adapt_delta = 0.95),
  thin = 1,
  seed = 123,
  data = model_times_df %>% filter(category == "Elite Women" & swim > 500 & swim < 4000))

swim_model_final <- brm(
  formula = swim ~ year,
  family = negbinomial(link = "log"),
  chains = 8,
  cores = 8,
  iter = 4000,
  control = list(adapt_delta = 0.95),
  thin = 1,
  seed = 123,
  data = model_times_df %>% filter(category == "Elite Men" & swim > 500 & swim < 4000))

# Bike time models for women and men
bike_model_finalw <- brm(
  formula = bike ~ year,
  family = negbinomial(link = "log"),
  chains = 8,
  cores = 8,
  iter = 4000,
  control = list(adapt_delta = 0.95),
  thin = 1,
  seed = 123,
  data = model_times_df %>% filter(category == "Elite Women" & bike > 1000 & bike < 10000))

 bike_model_final <- brm(
  formula = bike ~ year,
  family = negbinomial(link = "log"),
  chains = 8,
  cores = 8,
  iter = 4000,
  control = list(adapt_delta = 0.95),
  thin = 1,
  seed = 123,
  data = model_times_df %>% filter(category == "Elite Men" & bike > 1000 & bike < 10000))

# Run time models for women and men
 run_model_finalw <- brm(
   formula = run ~ year,
   family = negbinomial(link = "log"),
   chains = 8,
   cores = 8,
   iter = 4000,
   control = list(adapt_delta = 0.95),
   thin = 1,
   seed = 123,
   data = model_times_df %>% filter(category == "Elite Women" & run > 500 & run < 5000))
 
 run_model_final <- brm(
   formula = run ~ year,
   family = negbinomial(link = "log"),
   chains = 8,
   cores = 8,
   iter = 4000,
   control = list(adapt_delta = 0.95),
   thin = 1,
   seed = 123,
   data = model_times_df %>% filter(category == "Elite Men" & run > 500 & run < 5000))
 
# Total time models for women and men
 total_model_finalw <- brm(
   formula = total_time ~ year,
   family = negbinomial(link = "log"),
   chains = 8,
   cores = 8,
   iter = 4000,
   control = list(adapt_delta = 0.95),
   thin = 1,
   seed = 123,
   data = model_times_df %>% filter(category == "Elite Women" & total_time > 2500 & total_time < 15000))
 
 total_model_final <- brm(
   formula = total_time ~ year,
   family = negbinomial(link = "log"),
   chains = 8,
   cores = 8,
   iter = 4000,
   control = list(adapt_delta = 0.95),
   thin = 1,
   seed = 123,
   data = model_times_df %>% filter(category == "Elite Men" & total_time > 2500 & total_time < 15000))
 
 
#save(fit, file = "model.RData")

# Posterior predictive check
pp_check(total_model_final, re_formula = NULL)

# Summary
summary(bike_model_final)

# Prior summary
prior_summary(bike_model_final)

# Marginal effect
conditional_effects(bike_model_finalw)

# Slope
# Effect of Condition

swim_model_final_plot <-
  conditional_effects(swim_model_final, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

swim_model_finalw_plot <-
  conditional_effects(swim_model_finalw, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

bike_model_final_plot <-
  conditional_effects(bike_model_final, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

bike_model_finalw_plot <-
  conditional_effects(bike_model_finalw, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

run_model_final_plot <-
  conditional_effects(run_model_final, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

run_model_finalw_plot <-
  conditional_effects(run_model_finalw, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

total_model_final_plot <-
  conditional_effects(total_model_final, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")

total_model_finalw_plot <-
  conditional_effects(total_model_finalw, effects = "year", plot = FALSE, prob = .95)[[1]] %>%
  mutate(ci = "95%")
  
  

# Men plots for Swim, Bike, Run and Total Time

# Swim 
swim_model_final_plot %>% 
  ggplot()+
  geom_lineribbon(data = swim_model_finalw_plot, aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'women'))+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'men'))+
  # geom_point(data = model_times_df %>% filter(category == 'Elite Men' & bike > 1000 & run < 10000) %>% group_by(year) %>% mutate(mu = mean(bike)),
  #            aes(x = year, y = mu/60))+  
  theme_classic()+
  scale_fill_manual(values = c('women' = '#E69F00', 'men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Time (minutes)')+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

# Bike 
bike_model_final_plot %>% 
  ggplot()+
  geom_lineribbon(data = bike_model_finalw_plot, aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'women'))+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'men'))+
  # geom_point(data = model_times_df %>% filter(category == 'Elite Men' & bike > 1000 & run < 10000) %>% group_by(year) %>% mutate(mu = mean(bike)),
  #            aes(x = year, y = mu/60))+  
  theme_classic()+
  scale_fill_manual(values = c('women' = '#E69F00', 'men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Time (minutes)')+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

# Running
run_model_final_plot %>% 
  ggplot()+
  geom_lineribbon(data = run_model_finalw_plot, aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'women'))+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'men'))+
  # geom_point(data = model_times_df %>% filter(category == 'Elite Men' & run > 500 & run < 5000) %>% group_by(year) %>% mutate(mu = mean(run)),
  #            aes(x = year, y = mu/60))+
  theme_classic()+
  scale_fill_manual(values = c('women' = '#E69F00', 'men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Time (minutes)')+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


total_model_final_plot %>% 
   ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'men'))+
  geom_lineribbon(data = total_model_finalw_plot, aes(x = year, y = estimate__/60, ymin = lower__/60, ymax = upper__/60, fill = 'women'))+
  # geom_point(data = model_times_df %>% filter(category == 'Elite Men' & total_time > 2500 & total_time < 15000) %>% group_by(year) %>% mutate(mu = mean(total_time)),
  #            aes(x = year, y = mu/60))+
  scale_fill_manual(values = c('women' = '#E69F00', 'men' = '#0072B2'))+
  theme_classic()+
   labs(x = 'Year',
       y = 'Time (minutes)')+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))





  
  
  
  
  
  
  
  


# Participation Rates #

# Fitting it as a frequentist logistic regression
part_rates <- lm(sex ~ year_s, data = year_participation_bayes %>% filter(year > 2002) %>%
                   mutate(year_s = year - min(year)))

# Model Logistic Reg
mod_bayes_part_rates_logistic <- brm(
  formula = sex ~ year_s,
  # family = Beta(link = "logit"),
  family = bernoulli(),
  #prior = c(set_prior("normal(0,1)", class = "b")),
  chains = 8,
  cores = 8,
  iter = 20000,
  control = list(adapt_delta = 0.95),
  thin = 1,
  seed = 123,
  data = year_participation_bayes %>% filter(year > 2002) %>% mutate(year_s = year - min(year)))


# Posterior predictive check
pp_check(mod_bayes_part_rates_logistic, re_formula = NULL)

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


model_pp <- 
plotdata_part_rates_logistic %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__*100, ymin = lower__*100, ymax = upper__*100))+
  geom_point(data = year_participation_3 %>%
               filter(year > 2002), aes(x = year, y = perc_w*100))+
  scale_x_continuous(limits = c(2003, 2022), breaks = c(2005, 2009, 2013, 2017, 2021))+
  theme_classic()+
  labs(x = 'Year',
       y = 'Proportion (%)')+
  scale_y_continuous(limits = c(25,50),
                     n.breaks = 5)+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

summary(lm(estimate__ ~ year_s, data = plotdata_part_rates_logistic %>% mutate(year_s = year - min(year))))

row_1 <- plot_grid(men_pp, women_pp,
                   nrow = 1,
                   align = c("hv"),
                   labels = c('A','B'))

row_2 <- plot_grid(participation_plot,
                   nrow = 1,
                   align = c('hv'),
                   labels = c('C'))

plot_grid(row_1, row_2,
          nrow = 2,
          rel_heights = c(0.5,0.5),
          align = c('v'))

ggsave(filename = 'participation_final_v1.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 12,
       height = 6)

#Fitting in a frequentist framework
#swim_model_final <- lm(swim ~ year_s*category, data = model_times_df %>% filter(swim > 500 & swim < 4000) %>% filter(position < 21))
swim_model_final <- glm.nb(swim ~ year_s + category + year_s*category + (1|program_id) + (1|athlete_id), data = model_times_df %>% filter(swim > 600 & swim < 5000 & position < 21))

#bike_model_final <- lm(bike ~ year_s*category, data = model_times_df %>% filter(bike > 2500 & bike < 10000)%>% filter(position < 21))
bike_model_final <- glm.nb(bike ~ year_s + category + year_s*category + (1|program_id) + (1|athlete_id), data = model_times_df %>% filter(bike > 500 & bike < 6000))

#run_model_final <- lm(run ~ year_s*category, data = model_times_df %>% filter(run > 1000 & run < 5000)%>% filter(position < 21))
run_model_final <- glm.nb(run ~ year_s + category + year_s*category + (1|program_id) + (1|athlete_id), data = model_times_df %>% filter(run > 500 & run < 4000) %>% filter(position < 21))

#total_model_final <- lm(total_time ~ year_s*category, data = model_times_df %>% filter(total_time > 2500 & total_time < 15000)%>% filter(position < 21))
total_model_final <- glm.nb(total_time ~ year_s + category + year_s*category + (1|program_id) + (1|athlete_id), data = model_times_df %>% filter(total_time > 2500 & total_time < 10800)%>% filter(position < 21))

# Taking the yearly predictions to plot through ggplot
swim_model_final_plot <- predictions(
  swim_model_final,
  newdata = datagrid(category = unique, year_s = unique))

bike_model_final_plot <- predictions(
  bike_model_final,
  newdata = datagrid(category = unique, year_s = unique))

run_model_final_plot <- predictions(
  run_model_final,
  newdata = datagrid(category = unique, year_s = unique))

total_model_final_plot <- predictions(
  total_model_final,
  newdata = datagrid(category = unique, year_s = unique))

# Plotting the yearly predictions
sp <- swim_model_final_plot %>%
  ggplot()+
  geom_ribbon(aes(x = year_s+2000, y = estimate/60, ymin = conf.low/60, ymax = conf.high/60, group = category, fill = category))+
  geom_line(aes(x = year_s+2000, y = estimate/60, group = category))+
  geom_point(data = model_times_df %>% filter(swim > 600 & swim < 5000 & position < 21) %>% 
               group_by(category, year) %>% 
               mutate(mu = mean(swim)),
             aes(x = year, y = mu/60, shape = category))+
  theme_classic()+
  scale_fill_manual(values = c('Elite Women' = '#E69F00', 'Elite Men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Swim Time (minutes)')+
  #scale_y_continuous(limits = c(19,23), breaks = c(19,20,21,22,23))+
  theme(text = element_text(size = 14),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = 'none')

bp <- bike_model_final_plot %>% 
  ggplot()+
  geom_ribbon(aes(x = year_s+2000, y = estimate/60, ymin = conf.low/60, ymax = conf.high/60, group = category, fill = category))+
  geom_line(aes(x = year_s+2000, y = estimate/60, group = category))+
  geom_point(data = model_times_df %>% filter(bike > 500 & bike < 6000 & position < 21) %>% 
               group_by(category, year) %>% 
               mutate(mu = mean(bike)),
             aes(x = year, y = mu/60))+
  geom_errorbar(data = model_times_df %>% filter(bike > 500 & bike < 6000 & position < 21) %>% 
                  group_by(category, year) %>% 
                  mutate(mu = mean(bike)/60,
                         sd = sd(bike)/60),
                aes(ymin = mu-sd, ymax = mu + sd, x = year))+
  theme_classic()+
  scale_fill_manual(values = c('Elite Women' = '#E69F00', 'Elite Men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Bike Time (minutes)')+
  #scale_y_continuous(limits = c(60,70), breaks = c(60,62,64,66,68,70))+
  theme(text = element_text(size = 14),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = 'none')

rp <- run_model_final_plot %>% 
  ggplot()+
  geom_ribbon(aes(x = year_s+2000, y = estimate/60, ymin = conf.low/60, ymax = conf.high/60, group = category, fill = category))+
  geom_line(aes(x = year_s+2000, y = estimate/60, group = category))+
  geom_point(data = model_times_df %>% filter(run > 500 & run < 4000 & position < 21) %>% 
               group_by(category, year) %>% 
               mutate(mu = mean(run)),
             aes(x = year, y = mu/60))+
  geom_errorbar(data = model_times_df %>% filter(run > 500 & run < 4000 & position < 21) %>% 
                  group_by(category, year) %>% 
                  mutate(mu = mean(run)/60,
                         sd = sd(run)/60),
                aes(ymin = mu-sd, ymax = mu + sd, x = year))+
  theme_classic()+
  scale_fill_manual(values = c('Elite Women' = '#E69F00', 'Elite Men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Run Time (minutes)')+
  #scale_y_continuous(limits = c(34,42), breaks = c(34,36,38,40,42))+
  theme(text = element_text(size = 14),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = 'none')

tp <- total_model_final_plot %>% 
  ggplot()+
  geom_ribbon(aes(x = year_s+2000, y = estimate/60, ymin = conf.low/60, ymax = conf.high/60, group = category, fill = category))+
  geom_line(aes(x = year_s+2000, y = estimate/60, group = category))+
  geom_point(data = model_times_df %>% filter(total_time > 2500 & run < 15000 & position < 21) %>% 
               group_by(category, year) %>% 
               mutate(mu = mean(total_time)),
             aes(x = year, y = mu/60))+
  theme_classic()+
  #scale_y_continuous(limits = c(115,135), breaks = c(115,120,125,130,135))+
  scale_fill_manual(values = c('Elite Women' = '#E69F00', 'Elite Men' = '#0072B2'))+
  labs(x = 'Year',
       y = 'Race Time (minutes)')+
  theme(text = element_text(size = 14),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = 'none')

plot_grid(sp, bp, rp, tp,
          align = c("hv"),
          scale = 1,
          labels = c('A','B','C','D'))

ggsave(filename = 'modeled_times_complete.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 6)
# Summary statistics for each

summary(swim_model_final)
summary(bike_model_final)
summary(run_model_final)
summary(total_model_final)

confint(swim_model_final)
confint(bike_model_final)
confint(run_model_final)
confint(total_model_final)
