### For finished code that results in a desired outcome

library(tidyverse)
library(ggplot2)
library(cowplot)
library(GGally)
library(DescTools)
library(viridis)


#### Selecting the Bad Races for Elite Men ####
bad_races <- elite_men %>% 
  filter(bike < 3000 & total_time < 5000 & bike > 0 & total_time > 0) %>% 
  .$program_id %>% 
  unique

bad_races2 <- elite_men %>% 
  filter(swim < 821 & total_time > 0 & swim > 0) %>% 
  .$program_id %>% 
  unique()

bad_races3 <- elite_men %>% 
  filter(total_time < 5400 & total_time > 0) %>% 
  .$program_id %>% 
  unique()

bad_races4 <- elite_men %>% 
  filter(total_time > 0 & bike > 0 & bike < 1000) %>% 
  .$program_id %>% 
  unique()

bad_races5 <- elite_men %>% 
  filter(total_time > 0, swim > 6000) %>% 
  .$program_id %>% 
  unique()

bad_races6 <- elite_men %>% 
  filter(total_time > 0, run < 1100, run > 0) %>% 
  .$program_id %>% 
  unique()

# joining the bad races together
bad <- c(bad_races, bad_races2, bad_races3, bad_races4, bad_races5, bad_races6)

# Setting NA to 0
elite_men_NA <- elite_men

elite_men_NA[rowSums(is.na(elite_men_NA)) > 0,] %>% 
  view()

elite_men_NA <- replace_na(elite_men_NA, list(swim = 0, t1 = 0, bike = 0, t2 = 0, run = 0, total_time = 0))

# removing the bad races

final_subset_df <- subset(elite_men_NA, !program_id %in% bad)

#### Selecting the bad races for Elite women ####

bad_races_f <- elite_women %>% 
  filter(bike > 0, bike < 3000, total_time > 0, total_time < 6000) %>% 
  .$program_id %>% 
  unique()

bad_races_f2 <- elite_women %>% 
  filter(run > 0, run <1500 , total_time > 0, total_time <7500) %>% 
  .$program_id %>% 
  unique()


bad_races_f3 <- elite_women %>% 
  filter(swim > 0, swim < 820, total_time > 0, total_time < 10000) %>% 
  .$program_id %>% 
  unique()

bad_races_f4 <- elite_women %>% 
  filter(total_time > 0, total_time < 5500) %>% 
  .$program_id %>% 
  unique()

bad_races_f5 <- elite_women %>% 
  filter(total_time > 0, run > 0, run < 1200) %>% 
  .$program_id %>% 
  unique()

bad_races_f6 <- elite_women %>% 
  filter(total_time > 0 & bike > 0 & bike < 1000) %>% 
  .$program_id %>% 
  unique()

# joing the bad races together
bad_f <- c(bad_races_f, bad_races_f2, bad_races_f3, bad_races_f4, bad_races_f5, bad_races_f6)

# removing the bad races from the data set
elite_women_NA <- elite_women

elite_women_NA[rowSums(is.na(elite_women_NA)) > 0,] %>% 
  view()

elite_women_NA <- replace_na(elite_women_NA, list(swim = 0, t1 = 0, bike = 0, t2 = 0, run = 0, total_time = 0))

# Removing the bad races

final_subset_df_f <- subset(elite_women_NA, !program_id %in% bad_f)


#### Selecting Erroneous Timing for Elite Men ####

final_subset_df %>% 
  filter(status == "") %>% 
  group_by(program_id) %>%
  mutate(r_median_total_time = median(total_time),
         r_median_swim = median(swim, na.rm = T),
         r_median_bike = median(bike, na.rm = T),
         r_median_run = median(run, na.rm = T),
         r_median_t1 = median(t1, na.rm = T),
         r_median_t2 = median(t2, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(athlete_id) %>% 
  mutate(a_median_total_time = median(total_time),
         a_median_swim = median(swim, na.rm = T),
         a_median_bike = median(bike, na.rm = T),
         a_median_run = median(run, na.rm = T),
         a_median_t1 = median(t1, na.rm = T),
         a_median_t2 = median(t2, na.rm = T)) %>% 
  mutate(r_perc_total_time = total_time/r_median_total_time,
         r_perc_swim = swim/r_median_swim,
         r_perc_bike = bike/r_median_bike,
         r_perc_run = run/r_median_run,
         r_perc_t1 = t1/r_median_t1,
         r_perc_t2 = t2/r_median_t2,
         a_perc_total_time = total_time/a_median_total_time,
         a_perc_swim = swim/a_median_swim,
         a_perc_bike = bike/a_median_bike,
         a_perc_run = run/a_median_run,
         a_perc_t1 = t1/a_median_t1,
         a_perc_t2 = t2/a_median_t2) -> p

error_timing_men <- p %>% 
  select(program_id, athlete_id, status, swim, t1, bike, t2, run, total_time,
         a_median_swim, r_median_swim, a_perc_swim, r_perc_swim,
         a_median_bike, r_median_bike, a_perc_bike, r_perc_bike,
         a_median_run, r_median_run, a_perc_run, r_perc_run,
         a_median_t1, r_median_t1, a_perc_t1, r_perc_t1,
         a_median_t2, r_median_t2, a_perc_t2, r_perc_t2) %>% 
  filter(a_perc_swim < 0.9 & r_perc_swim < 0.6 |
           a_perc_swim > 1.1 & r_perc_swim > 1.4 |
           a_perc_bike < 0.9 & r_perc_bike < 0.6 |
           a_perc_bike > 1.1 & r_perc_bike > 1.4 |
           a_perc_run < 0.9 & r_perc_run < 0.6 |
           a_perc_run > 1.1 & r_perc_run > 1.4 |
           a_perc_t1 > 1.2 & r_perc_t1 > 3 |
           a_perc_t2 > 1.2 & r_perc_t2 > 3 |
           swim == total_time | t1 == total_time | bike == total_time | t2 == total_time | run == total_time & total_time > 0 |
           a_perc_swim == 1 & r_perc_swim < 0.5 |
           a_perc_swim == 1 & r_perc_swim > 1.5 |
           a_perc_bike == 1 & r_perc_bike < 0.5 |
           a_perc_bike == 1 & r_perc_bike > 1.5 |
           a_perc_run == 1 & r_perc_run < 0.5 |
           a_perc_run == 1 & r_perc_run > 1.5 |
           a_perc_t1 == 1 & r_perc_t1 > 5 |
           a_perc_t2 == 1 & r_perc_t2 > 5) %>%
  mutate(time_error = TRUE) %>% 
  select(program_id, athlete_id, time_error)


complete_men <- left_join(final_subset_df, error_timing_men) %>%
  mutate(time_error = if_else(time_error == TRUE, TRUE, FALSE, missing = FALSE)) %>% 
  view()
  
complete_men %>% 
  filter(swim == 0 & t1 == 0 & bike == 0 & t2 == 0 & run == 0) %>% 
  view()



#### Selecting Erroneous Timing for Elite Women ####

final_subset_df_f %>% 
  filter(status == "") %>% 
  group_by(program_id) %>%
  mutate(r_median_total_time = median(total_time),
         r_median_swim = median(swim, na.rm = T),
         r_median_bike = median(bike, na.rm = T),
         r_median_run = median(run, na.rm = T),
         r_median_t1 = median(t1, na.rm = T),
         r_median_t2 = median(t2, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(athlete_id) %>% 
  mutate(a_median_total_time = median(total_time),
         a_median_swim = median(swim, na.rm = T),
         a_median_bike = median(bike, na.rm = T),
         a_median_run = median(run, na.rm = T),
         a_median_t1 = median(t1, na.rm = T),
         a_median_t2 = median(t2, na.rm = T)) %>% 
  mutate(r_perc_total_time = total_time/r_median_total_time,
         r_perc_swim = swim/r_median_swim,
         r_perc_bike = bike/r_median_bike,
         r_perc_run = run/r_median_run,
         r_perc_t1 = t1/r_median_t1,
         r_perc_t2 = t2/r_median_t2,
         a_perc_total_time = total_time/a_median_total_time,
         a_perc_swim = swim/a_median_swim,
         a_perc_bike = bike/a_median_bike,
         a_perc_run = run/a_median_run,
         a_perc_t1 = t1/a_median_t1,
         a_perc_t2 = t2/a_median_t2) -> p_women

error_timing_women <- p_women %>% 
  select(program_id, athlete_id, status, swim, t1, bike, t2, run, total_time,
         a_median_swim, r_median_swim, a_perc_swim, r_perc_swim,
         a_median_bike, r_median_bike, a_perc_bike, r_perc_bike,
         a_median_run, r_median_run, a_perc_run, r_perc_run,
         a_median_t1, r_median_t1, a_perc_t1, r_perc_t1,
         a_median_t2, r_median_t2, a_perc_t2, r_perc_t2) %>% 
  filter(a_perc_swim < 0.9 & r_perc_swim < 0.6 |
           a_perc_swim > 1.1 & r_perc_swim > 1.4 |
           a_perc_bike < 0.9 & r_perc_bike < 0.6 |
           a_perc_bike > 1.1 & r_perc_bike > 1.4 |
           a_perc_run < 0.9 & r_perc_run < 0.6 |
           a_perc_run > 1.1 & r_perc_run > 1.4 |
           a_perc_t1 > 1.2 & r_perc_t1 > 3 |
           a_perc_t2 > 1.2 & r_perc_t2 > 3 |
           swim == total_time | t1 == total_time | bike == total_time | t2 == total_time | run == total_time & total_time > 0 |
           a_perc_swim == 1 & r_perc_swim < 0.5 |
           a_perc_swim == 1 & r_perc_swim > 1.5 |
           a_perc_bike == 1 & r_perc_bike < 0.5 |
           a_perc_bike == 1 & r_perc_bike > 1.5 |
           a_perc_run == 1 & r_perc_run < 0.5 |
           a_perc_run == 1 & r_perc_run > 1.5 |
           a_perc_t1 == 1 & r_perc_t1 > 5 |
           a_perc_t2 == 1 & r_perc_t2 > 5) %>%
  mutate(time_error = TRUE) %>% 
  select(program_id, athlete_id, time_error)


complete_women <- left_join(final_subset_df_f, error_timing_women) %>%
  mutate(time_error = if_else(time_error == TRUE, TRUE, FALSE, missing = FALSE))
  view()


#### Creating Graphs



#### Checking correlation matrix ####

complete_men %>% 
  filter(swim > 0, t1 > 0, bike > 0, t2 > 0, run > 0, total_time > 0) %>% 
  select(swim, t1, bike, t2, run, total_time) %>% 
  ggpairs(.)

complete_women %>% 
  filter(swim > 0, t1 > 0, bike > 0, t2 > 0, run > 0, total_time > 0) %>% 
  select(swim, t1, bike, t2, run, total_time) %>% 
  ggpairs(.)



#### Participation ####

year_participation <- bind_rows(
  complete_men %>%
    filter(status != "DNS") %>% 
    group_by(year) %>% 
    distinct(athlete_id) %>% 
    summarise(n = n(), sex = "Men"),
  complete_women %>% 
    filter(status != "DNS") %>% 
    group_by(year) %>% 
    distinct(athlete_id) %>% 
    summarise(n = n(), sex = "Women"))

year_participation_2 <- year_participation %>%
  pivot_wider(names_from = sex, values_from = n)

year_participation_3 <- year_participation_2 %>% 
  mutate(perc_w = Women/(Men+Women),
         perc_m = Men/(Men+Women))

participation_plot <- year_participation_3 %>%
  ggplot()+
  geom_lineribbon(aes(x = year, y = estimate__*100, ymin = lower__*100, ymax = upper__*100), data = plotdata_part_rates_logistic)+
  geom_line(aes(y = perc_w*100, x = year), colour = 'black', data = year_participation_3 %>% filter(year > 1988))+
  theme_classic()+
  labs(
       y = "Proportion (%)",
       x = "Year")+
  scale_y_continuous(limits = c(25,50))+
  scale_x_continuous(limits = c(1989, 2023),
                     breaks = c(1990,1995,2000,2005,2010,2015,2020))+
  theme_classic()+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        axis.title.y = element_text(vjust = 2),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggsave('female_participation.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete')

mw_participation_plot <- year_participation %>%
  #filter(year > 1988) %>% 
  ggplot(aes(x=year,y=n,fill=sex)) +
  geom_col(color = "white", linewidth = 0.1, show.legend = FALSE)+
  scale_fill_manual(values = c('black', 'black')) +
  labs(x='Year',
       y = 'Athletes',
       fill = "Gender",
       title = 'Athlete Participation',
       subtitle = "Unique athletes")+
  theme_classic()+
  facet_wrap(~sex)+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times New Roman", face = 'bold'),
        axis.title = element_text(size = 16, family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman" ),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.x = element_line(colour = 'white'),
        strip.text = element_text(size = 12, face = 'bold', family = 'Times New Roman'),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

men_pp <- year_participation %>% 
  filter(sex == 'Men') %>% 
  ggplot(aes(x = year, y = n))+
  geom_col(colour = 'white', fill = '#0072B2', linewidth = 0.1, show.legend = FALSE)+
  labs(x = 'Year',
       y = 'Number of Athletes')+  
  theme_classic()+
  scale_y_continuous(limits = c(0,1500),
                     n.breaks = 7)+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

women_pp <- year_participation %>% 
  filter(sex == 'Women') %>% 
  ggplot(aes(x = year, y = n))+
  geom_col(colour = 'white', fill = '#E69F00', linewidth = 0.1, show.legend = FALSE)+
  labs(x = 'Year',
       y = 'Number of Athletes')+  
  theme_classic()+
  scale_y_continuous(limits = c(0,1500),
                     n.breaks = 7)+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")
  

plot_grid(mw_participation_plot, participation_plot,
          nrow = 1,
          rel_heights = c(1,1),
          rel_widths = c(1.5,1))

ggsave('participation.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete')


complete_women %>% 
  filter(status != 'DNS') %>% 
  count()

complete_men %>% filter(status != 'DNS') %>% count(.$athlete_id) %>% view()

unique(complete_men$program_id)



row_1 <- plot_grid(men_pp, women_pp,
                   nrow = 1,
                   align = c("hv"),
                   labels = c('A','B'))

row_2 <- plot_grid(participation_plot,
                   nrow = 1,
                   labels = c('C'))

plot_grid(row_1, row_2,
          nrow = 2,
          rel_heights = c(0.5,0.5))

ggsave(filename = 'participation_final_v2.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 6.5)

#### Careers ####

# Summary stats
q90 = reframe(complete_men %>%
                filter(status != 'DNS') %>% 
                group_by(athlete_id) %>% 
                distinct(year) %>% 
                summarise(n = n()) , quantiles = quantile(n, c(0.00,0.90))) %>%
  mutate(
    q = 90,
    limit = c('lower','upper'))

q50 = reframe(complete_men %>%
                filter(status != 'DNS') %>% 
                group_by(athlete_id) %>% 
                distinct(year) %>% 
                summarise(n = n()) , quantiles = quantile(n, c(0.00,0.50))) %>%
  mutate(
    q = 50,
    limit = c('lower','upper'))

q90w = reframe(complete_women %>%
                filter(status != 'DNS') %>% 
                group_by(athlete_id) %>% 
                distinct(year) %>% 
                summarise(n = n()) , quantiles = quantile(n, c(0.00,0.90))) %>%
  mutate(
    q = 90,
    limit = c('lower','upper'))

q50w = reframe(complete_women %>%
                filter(status != 'DNS') %>% 
                group_by(athlete_id) %>% 
                distinct(year) %>% 
                summarise(n = n()) , quantiles = quantile(n, c(0.00,0.50))) %>%
  mutate(
    q = 50,
    limit = c('lower','upper'))

# Data for arrows (without pointers); go below the x-axis
arr = bind_rows(q50, q90) %>%
  tidyr::spread(limit, quantiles) %>%
  mutate(y = case_when(
    q==90 ~ -200,
    q==50 ~ -400
  ),
  yend = y
  )

arrw = bind_rows(q90w) %>%
  tidyr::spread(limit, quantiles) %>%
  mutate(y = case_when(
    q==90 ~ -200
  ),
  yend = y
  )
# Text labels for arrows (high on y-axis)
arr.text = mutate(arr, x=20, label=paste(c('90%'),'of athletes')) # at far right
arr.textw = mutate(arrw, x=20, label=paste(c('90%'),'of athletes')) # at far right



men_career <- 
complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = n))+
  geom_bar(fill = "#0072B2", color = "#FFFFFF")+
  geom_text(data = arr.text, aes(x=x, y=y, label=label), family = 'Times New Roman', size = 5, hjust = 1, col='black')+
  geom_segment(data = arr, linewidth = 1.5, aes(x = lower, y = y, xend = upper, yend = yend), position = 'identity')+
  labs(x = 'Career Length (years)',
       y = 'Number of Athletes')+
  scale_y_continuous(limits = c(-200,6000), breaks = c(0,1000,2000,3000,4000,5000,6000))+
  theme_classic()+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

women_career <- 
  complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x=n))+
  geom_bar(fill = "#E69F00", color = "#FFFFFF")+
  geom_text(data = arr.textw, aes(x=x, y=y, label=label), family = 'Times New Roman', size = 5, hjust=1, col='black')+
  geom_segment(data = arrw, linewidth = 1.5, aes(x = lower, y = y, xend = upper, yend = yend), position = 'identity')+
  labs(x = 'Career Length (years)',
       y = 'Number of Athletes')+
  scale_y_continuous(limits = c(-200,6000), breaks = c(0,1000,2000,3000,4000,5000,6000))+
  theme_classic()+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

plot_grid(men_career, women_career,
          align = c("hv"),
          scale = 1,
          labels = c('A','B'))

ggsave(filename = 'carrer_complete_v2.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 4)

zoom_men_career <- complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  filter(n > 10) %>% 
  ggplot(aes(x=n))+
  geom_histogram(binwidth = 1, fill = "#E69F00", color = "#FFFFFF") +
  labs(x = 'Years',
       y = 'Athletes',
       title = 'Years Elite Men Compete') +
  theme_classic() +
  scale_x_continuous(breaks = c(12,14,16,18,20))+
  theme(panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

ggplot_build(men_career)$data[[1]] %>%
  view()


ggplot_build(women_career)$data[[1]] %>%
  view()

men_year_compete <- ggdraw(men_career)+
  draw_plot(zoom_men_career, .45, .35, .50, .50)


ggsave('men_compete.jpg',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete')

women_career <- complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x=n))+
  geom_histogram(binwidth = 1, fill = "black", color = "#FFFFFF") +
  labs(x = 'Years',
       y = 'Athletes',
       title = 'Years Elite Women Compete') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


zoom_women_career <- complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  filter(n > 10) %>% 
  ggplot(aes(x=n))+
  geom_histogram(binwidth = 1, fill = "black", color = "#FFFFFF") +
  labs(x = 'Years',
       y = 'Athletes',
       title = 'Years Elite Women Compete') +
  theme_classic() +
  scale_x_continuous(breaks = c(11,13,15,17,19,21))+
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


women_year_compete <- ggdraw(women_career)+
  draw_plot(zoom_women_career, .45, .35, .50, .50)

plot_grid(men_year_compete, women_year_compete)

ggsave('year_compete.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 12,
       height = 6)


complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>%
  reframe(n = n()) %>%
  reframe(median = median(n),
         quant = quantile(n))

complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>%
  filter(n >= 10) %>% 
  count()

# Calculating mens careers
complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  count(n) %>% mutate(total = sum(nn),
                      percentage = (nn/total)*100) %>% 
  dplyr::select(n, nn, percentage) %>% 
  mutate(total = cumsum(percentage))
  
# Calculating womens careers
complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  count(n) %>% mutate(total = sum(nn),
                      percentage = (nn/total)*100) %>% 
  dplyr::select(n, nn, percentage) %>% 
  filter(n >9) %>% 
  mutate(total = sum(nn),
         total_perc = sum(percentage))

# Calculating change in mens career lengths over time 
complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  count(n) %>% mutate(total = sum(nn),
                      percentage = (nn/total)*100) %>% 
  dplyr::select(n, nn, percentage)
  
# Calculating change in womens career lengths over time 
complete_women %>%
  filter(status != 'DNS' & year > 2012) %>% 
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  count(n) %>% mutate(total = sum(nn),
                      percentage = (nn/total)*100) %>% 
  dplyr::select(n, nn, percentage)



#### Number of Races Athletes Start ####

number_races_men <- complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 4) %>% 
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "black",
                 color = "#FFFFFF")+
  labs(title = 'Races Men Start',
       x = 'Races',
       y = 'Athletes')+
  theme_classic() +
  scale_x_continuous(breaks = c(5,25,50,75,100,125,150))+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


zoom_number_races_men <- complete_men %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 49) %>% 
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "black",
                 color = "#FFFFFF")+
  labs(title = 'Races Men Start',
       x = 'Races',
       y = 'Athletes')+
  scale_x_continuous(n.breaks = 6)+
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


men_number_races <- ggdraw(number_races_men)+
  draw_plot(zoom_number_races_men, .45, .4, .5, .5)



number_races_women <- complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 4) %>% 
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "black",
                 color = "#FFFFFF")+
  labs(title = 'Races Women Start',
       x = 'Races',
       y = 'Athletes')+
  theme_classic() +
  scale_x_continuous(breaks = c(5,25,50,75,100,125,150))+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


zoom_number_races_women <- complete_women %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 49) %>% 
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "black",
                 color = "#FFFFFF")+
  labs(title = 'Races Women Start',
       x = 'Races',
       y = 'Athletes')+
  scale_x_continuous(n.breaks = 6)+
  theme_classic() +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        axis.text.x = element_text(hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


ggplot_build(number_races_men)$data[[1]] %>% 
  view()


ggplot_build(number_races_women)$data[[1]] %>%
  view()


women_number_races <- ggdraw(number_races_women)+
  draw_plot(zoom_number_races_women, .45, .4, .5, .5)


plot_grid(men_number_races, women_number_races)


ggsave('race_starts.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       height = 5,
       width = 12)

# Number of absolute race starts for men
complete_men_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  mutate(races = sum(n))

# Number of absolute race starts for women
complete_women_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  mutate(races = sum(n))

# Total amount of race starts for men
complete_men_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  count(n) %>% 
  mutate(perc = nn/sum(nn)*100)

# Total amount of race starts for women
complete_women_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  count(n) %>% 
  mutate(perc = nn/sum(nn)*100)

# Total amount of race starts for men in the last decade
complete_men_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  count(n) %>% 
  mutate(perc = nn/sum(nn)*100) %>% 
  view()

# Total amount of race starts for women in the last decade
complete_women_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  count(n) %>% 
  mutate(perc = nn/sum(nn)*100)

# Calculating number of male starts >= 10 since 2000

complete_men_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 9)
  
# Calculating number of female starts >= 10 since 2000

complete_women_yobll %>% 
  filter(status != 'DNS' & year > 1999) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  filter(n > 9)
  



complete_women %>% 
  filter(status != 'DNS') %>% 
  filter(year >= 1990 & year <= 2000) %>% 
  group_by(athlete_id) %>%
  summarise(n = length(program_id)) %>% 
  #filter(n == 1) %>% 
  count()
  

complete_women %>% 
  filter(status != 'DNS') %>% 
  filter(year >= 2010 & year <= 2020) %>% 
  group_by(athlete_id) %>%
  summarise(n = length(program_id)) %>% 
  #filter(n == 1) %>% 
  count()

#### Average Time Yearly ####

#men_yearly <- 
ggplot(data = complete_men %>%
         filter(position == 1 & time_error == FALSE & total_time > 0) %>%
         group_by(year) %>%
         mutate(mean_tt_1 = median(total_time)), aes(x = year)) +
  geom_point(aes(y = mean_tt_1), colour = '#0072B2', shape = 16, size = 2.5, show.legend = FALSE) +
  geom_line(aes(y = mean_tt_1), colour = '#0072B2', show.legend = TRUE, linewidth = 1) +
  geom_point(data = complete_men %>%
               filter(total_time > 0 & time_error == FALSE & status == "") %>% 
               group_by(year) %>%
               mutate(mean_tt = median(total_time)), 
             aes(y = mean_tt), shape = 18, size = 3, show.legend = FALSE) +
  geom_line(data = complete_men %>%
              filter(total_time > 0 & time_error == FALSE & status == "") %>% 
              group_by(year) %>%
              mutate(mean_tt = median(total_time)), 
            aes(y = mean_tt), show.legend = TRUE, linewidth = 1) +
  #scale_color_manual(name = "Legend", values = c("First Position" = "#E69F00", "Average" = "black")) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1986, 2022, 6))+
  labs(title = 'Yearly Median Finish Time Men',
       x = 'Year',
       y = ' Time (seconds)') +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 14, family = 'Times New Roman'),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



#women_yearly <- 
ggplot(data = complete_women %>%
               filter(position == 1 & time_error == FALSE & total_time > 0) %>%
               group_by(year) %>%
               mutate(mean_tt_1 = median(total_time)), aes(x = year)) +
  geom_point(aes(y = mean_tt_1), colour = '#E69F00', size = 2.5) +
  geom_line(aes(y = mean_tt_1), colour = '#E69F00', linewidth = 1) +
  geom_point(data = complete_women %>%
               filter(total_time > 0 & time_error == FALSE & status == "") %>%
               group_by(year) %>% 
               mutate(mean_tt = median(total_time)), 
             aes(y = mean_tt), shape = 18, size = 3) +
  geom_line(data = complete_women %>%
              filter(total_time > 0 & time_error == FALSE & status == "") %>% 
              group_by(year) %>% 
              mutate(mean_tt = median(total_time)), 
            aes(y = mean_tt), linewidth = 1) +
  #scale_color_manual(name = "", values = c("First Position" = "#E69F00", "Average" = "black")) +
  scale_x_continuous(breaks = seq(1986, 2022, 6))+
  theme_classic() +
  labs(title = 'Yearly Median Finish Time Women',
       x = 'Year',
       y = ' Time (seconds)') +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 14, family = 'Times New Roman'),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



#### Average Segment Yearly ####


complete_men %>% 
  filter(time_error == FALSE & swim > 0) %>%
  group_by(program_id) %>% 
  group_by(year) %>% 
  mutate(mean_swim = mean(swim),
         sd = sd(swim)) %>% 
  ggplot(aes(x = year))+
  geom_point(aes(y = mean_swim))+
  geom_line(aes(y = mean_swim))+
  geom_point(data = complete_women %>% 
               filter(time_error == FALSE & swim > 0) %>% 
               group_by(year) %>% 
               mutate(mean_swim = mean(swim)),
             aes(y = mean_swim))+
  geom_line(data = complete_women %>% 
              filter(time_error == FALSE & swim > 0) %>% 
              group_by(year) %>% 
              mutate(mean_swim = mean(swim)),
            aes(y = mean_swim))+
  theme_classic() +
  scale_x_continuous(breaks = seq(1986, 2022, 6))+
  labs(title = '',
       x = 'Year',
       y = ' Time (seconds)') +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 14, family = 'Times New Roman'),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

complete_men %>% 
  filter(time_error == FALSE & bike > 0 & position > 0 & position < 11) %>% 
  group_by(year) %>% 
  mutate(mean_bike = mean(bike)) %>% 
  ggplot(aes(x = year))+
  geom_point(aes(y = mean_bike))+
  geom_line(aes(y = mean_bike))+
  geom_point(data = complete_women %>% 
               filter(time_error == FALSE & bike > 0 & position > 0 & position < 11) %>% 
               group_by(year) %>% 
               mutate(mean_bike = mean(bike)),
             aes(y = mean_bike))+
  geom_line(data = complete_women %>% 
              filter(time_error == FALSE & bike > 0 & position > 0 & position < 11) %>% 
              group_by(year) %>% 
              mutate(mean_bike = mean(bike)),
            aes(y = mean_bike))

complete_men %>% 
  filter(time_error == FALSE & run > 0 & position > 0 & position < 11) %>% 
  group_by(year) %>% 
  mutate(mean_run = mean(run)) %>% 
  ggplot(aes(x = year))+
  geom_point(aes(y = mean_run))+
  geom_line(aes(y = mean_run))+
  geom_point(data = complete_women %>% 
               filter(time_error == FALSE & run > 0 & position > 0 & position < 11) %>% 
               group_by(year) %>% 
               mutate(mean_run = mean(run)),
             aes(y = mean_run))+
  geom_line(data = complete_women %>% 
              filter(time_error == FALSE & run > 0 & position > 0 & position < 11) %>% 
              group_by(year) %>% 
              mutate(mean_run = mean(run)),
            aes(y = mean_run))

#### Segment Time Contribution ####

complete_men %>%
  filter(status == "" & time_error == FALSE & swim > 0 & bike > 0 & run > 0 & t1 < 600 & t2 < 600 & t2 >0 & t1 > 0 ) %>%
  group_by(year) %>%
  mutate(cont_swim = sum(swim),
         cont_bike = sum(bike),
         cont_run = sum(run),
         cont_t = sum(t1) + sum(t2)) %>%
  summarise(s = median(cont_swim),
            b = median(cont_bike),
            r = median(cont_run),
            t = median(cont_t),
            total = s + b + r + t) %>%
  select(year, s, b, r, t, total) %>%
  mutate(sp = (s/total)*100,
         bp = (b/total)*100,
         rp = (r/total)*100,
         tp = (t/total)*100,
         tt = sp + bp + rp + tp) %>% 
  select(year, sp, bp, rp, tp, tt) %>% 
  ggplot(aes(x = year, y = sp + bp + rp + tp))+
  geom_col(fill = 'black')+
  geom_col(aes(y = bp+rp+sp), fill = 'grey70')+
  geom_col(aes(y = bp+rp), fill = 'grey45')+
  geom_col(aes(y = rp), fill = 'grey20')+
  theme_cowplot()+
  labs(title = "Segment Contribution Over Time for Elite Men",
       x = "Year",
       y = '%')+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = 'Times New Roman'),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = 'bold', family = 'Times New Roman'),
        axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'))

ggsave('men_contribution.jpg',
       height = 4, width = 6,
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       dpi = 400)

complete_women %>% 
  filter(status == "" & time_error == FALSE & swim > 0 & bike > 0 & run > 0 & t1 > 0 & t2> 0 & t1 < 600 & t2 < 600) %>%
  group_by(year) %>%
  mutate(cont_swim = sum(swim),
         cont_bike = sum(bike),
         cont_run = sum(run),
         cont_t = sum(t1) + sum(t2)) %>%
  summarise(s = mean(cont_swim),
            b = mean(cont_bike),
            r = mean(cont_run),
            t = mean(cont_t),
            total = s + b + r + t) %>%
  select(year, s, b, r, t, total) %>%
  mutate(sp = (s/total)*100,
         bp = (b/total)*100,
         rp = (r/total)*100,
         tp = (t/total)*100,
         tt = sp + bp + rp + tp) %>% 
  select(year, sp, bp, rp, tp, tt) %>% 
  ggplot(aes(x = year, y = sp + bp + rp + tp))+
  geom_col(fill = 'black')+
  geom_col(aes(y = bp+rp+sp), fill = 'grey70')+
  geom_col(aes(y = bp+rp), fill = 'grey45')+
  geom_col(aes(y = rp), fill = 'grey20')+
  theme_cowplot()+
  labs(title = "Segment Contribution Over Time for Elite Women",
       x = "Year",
       y = '%')+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = 'Times New Roman'),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = 'bold', family = 'Times New Roman'),
        axis.title = element_text(size = 14, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'))

ggsave('women_contribution.jpg',
       height = 4, width = 6,
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       dpi = 400)



#### First Place Country ####

top_10_men = complete_men %>% 
  filter(position == 1 & year < 2023) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>%
  arrange(-(n)) %>%
  slice(1:10) %>%
  select(-n)




men_country_wins <- complete_men %>% 
  filter(position == 1) %>%
  filter(country %in% top_10_men$country) %>% 
  group_by(country, year) %>%
  summarise(n = n()) %>%
  mutate(cumulative_n = cumsum(n)) %>%
  ggplot(aes(x = year, y = cumulative_n))+
  geom_line(linewidth = 1.8, aes(colour = as.factor(country)), alpha = 0.9)+
  scale_color_viridis(option = "viridis", discrete=TRUE, end = .9)+
  geom_text(data = complete_men %>% 
              filter(position == 1) %>%
              filter(country %in% top_10_men$country) %>% 
              group_by(country, year) %>% 
              summarise(n = n()) %>%
              mutate(cumulative_n = cumsum(n)) %>%
              filter(year == 2022), aes(label = country), hjust = 0, 
            nudge_x = 1, check_overlap = FALSE,
            family = 'Times New Roman', fontface = 'bold', size = 4)+
  theme_classic()+
  labs(title = 'Country Male First Place',
       y = 'Count',
       x = 'Year')+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 12, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.position = "none")



top_10_women = complete_women %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>%
  arrange(-(n)) %>%
  slice(1:10) %>%
  select(-n)

#women_country_wins <- 
complete_women %>% 
  filter(position == 1) %>%
  filter(country %in% top_10_women$country) %>% 
  group_by(country, year) %>% 
  summarise(n = n()) %>%
  mutate(cumulative_n = cumsum(n)) %>% view()
  ggplot(aes(x = year, y = cumulative_n, group = country))+
  geom_line(aes(colour = country))+
  #geom_point(aes(shape = country))+
  scale_shape_manual(values = c(16,17,15,13,6,4,5,10,7,18))+
  labs(x = 'Year',
       y = 'Count',
      shape = 'Country',
      colour = 'Country')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = 'Times New Roman'),
        axis.title = element_text(family = 'Times New Roman'),
        axis.text = element_text(family = 'Times New Roman'),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = c(.25, .7),
        legend.title = element_text(family = 'Times New Roman'),
        legend.text = element_text(family = "Times New Roman"))

top_10_men$country %in% top_10_women$country

plot_grid(men_country_wins, women_country_wins)

ggsave('men_women_country_wins.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 16, height = 8)



saveRDS(complete_women, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/complete_women.RDS')
saveRDS(complete_men, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/complete_men.RDS')




#### First Place Per 100 Athletes Per Country #####

country_wins_men_first <- complete_men %>% 
  filter(position == 1 & year < 2023) %>% 
  group_by(country) %>% 
  summarise(wins = n()) %>%
  arrange(-wins) %>% 
  slice(1:10) %>% 
  arrange(country)

country_wins_men_first %>% arrange(-wins)
country_wins_women_first %>% arrange(-wins)

country_athlete_men <- complete_men %>%
  filter(country %in% country_wins_men_first$country) %>% 
  arrange(country) %>% 
  group_by(country) %>%
  summarise(num_ath = n()) %>%
  slice(1:10) %>% 
  select(num_ath)


per100_men <- cbind(country_athlete_men, country_wins_men_first) %>% 
  mutate(ratio = (wins/num_ath)) %>% 
  slice(1:10) %>% 
  arrange(-wins)

cbind(per100_men,
      per100_women)

wins_men_plot <- per100_men %>% 
  ggplot(aes(x = factor(country, levels = country), y = ratio)) + 
  geom_segment(aes(x = factor(country, levels = country), xend = factor(country, levels = country), y = 0, yend = ratio),
              linewidth = 1, colour = 'grey65') +
  geom_point(size = per100_men$n, colour = "#0072B2")+
  theme_classic()+
  labs(y = '%',
       x = 'Country')+
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

per100_men %>% 
  mutate(suc = ratio*100) %>% 
  arrange(-suc)

country_wins_women_first <- complete_women %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(wins = n()) %>%
  arrange(-wins) %>% 
  slice(1:10) %>% 
  arrange(country)


country_athlete_women <- complete_women %>%
  filter(country %in% country_wins_women_first$country) %>% 
  arrange(country) %>% 
  group_by(country) %>%
  summarise(num_ath = n()) %>%
  slice(1:10) %>% 
  select(num_ath)


per100_women <- cbind(country_athlete_women, country_wins_women_first) %>% 
  mutate(ratio = (wins/num_ath))%>% 
  slice(1:10) %>% 
  arrange(-wins)


wins_women_plot <- per100_women %>% 
  ggplot(aes(x = factor(country, levels = country), y = ratio)) + 
  geom_segment(aes(x = factor(country, levels = country), xend = factor(country, levels = country), y = 0, yend = ratio),
               linewidth = 1, colour = 'grey65') +
  geom_point(size = per100_women$n, colour = "#E69F00")+
  theme_classic()+
  labs(title = 'Wins Per Female Athlete Starts',
       y = '%',
       x = 'Country')+
  theme(axis.title = element_text(size = 16, family = 'Times New Roman'),
        axis.text = element_text(size = 16, family = 'Times New Roman'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")


plot_grid(wins_men_plot, wins_women_plot,
          align = c("hv"),
          scale = 1,
          labels = c('A','B'))

ggsave(filename = 'country_wins_per_ath.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 12,
       height = 5)



wins_men_plot <-
per100_men %>% 
  ggplot(aes(x = factor(country, levels = country), y = wins))+ 
  geom_segment(aes(x = factor(country, levels = country), xend = factor(country, levels = country), y = 0, yend = wins), colour  = 'black', linetype = 3) +
  geom_point(size = per100_men$ratio*150, colour = "#0072B2")+
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200),
                     limits = c(0,200))+
  theme_classic()+
  labs(y = 'Total Wins',
       x = 'Country')+
  theme(text = element_text(size = 17, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))
  
wins_women_plot <-
per100_women %>% 
  ggplot(aes(x = factor(country, levels = country), y = wins))+ 
  geom_segment(aes(x = factor(country, levels = country), xend = factor(country, levels = country), y = 0, yend = wins), colour = 'black', linetype = 3)+
  geom_point(size = per100_women$ratio*150, colour = "#E69F00")+
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200),
                     limits = c(0,200))+
  theme_classic()+
  labs(y = 'Total Wins',
       x = 'Country')+
  theme(text = element_text(size = 17, family = 'Times New Roman'),
        panel.grid.major.y = element_line(colour = 'white'),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")

plot_grid(wins_men_plot, wins_women_plot,
          align = c("hv"),
          scale = 1,
          labels = c('A','B'))

ggsave(filename = 'country_wins_complete_v2.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete',
       width = 10,
       height = 4)


#### Athlete Age ####

complete_women_yobll %>% group_by(program_id) %>% filter(latitude != 'NA' & longitude != 'NA') %>% summarise()
  
age_women <- 
complete_women_yobll %>%
  drop_na() %>%
  group_by(year) %>%
  distinct(athlete_id, .keep_all = TRUE) %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age))%>%
  ggplot(aes(x = year, y = median_age))+
  geom_point()+
  geom_line()+
  geom_point(data = complete_women_yobll %>%
               drop_na() %>%
               group_by(year) %>%
               filter(position == 1) %>% 
               distinct(athlete_id, .keep_all = TRUE) %>% 
               mutate(age = year - yob) %>% 
               summarise(median_age_pod = median(age)), aes(x = year, y = median_age_pod), colour = '#E69F00')+
  geom_line(data = complete_women_yobll %>%
              drop_na() %>%
              group_by(year) %>%
              filter(position == 1) %>% 
              distinct(athlete_id, .keep_all = TRUE) %>% 
              mutate(age = year - yob) %>% 
              summarise(median_age_pod = median(age)), aes(x = year, y = median_age_pod), colour = '#E69F00')+
  theme_classic()+
  scale_x_continuous(breaks = seq(1986, 2022, 6))+
  scale_y_continuous(limits = c(23,31),
                     breaks = c(23, 25, 27, 29, 31))+
  labs(title = 'Average Female Age',
       y = 'Age',
       x = 'Year')+
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


age_men <- 
complete_men_yobll %>%
  drop_na() %>%
  group_by(year) %>%
  distinct(athlete_id, .keep_all = TRUE) %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age)) %>%
  ggplot(aes(x = year, y = median_age))+
  geom_point()+
  geom_line()+
  geom_point(data = complete_men_yobll %>%
               drop_na() %>%
               group_by(year) %>%
               filter(position == 1) %>% 
               distinct(athlete_id, .keep_all = TRUE) %>% 
               mutate(age = year - yob) %>% 
               summarise(median_age_pod = median(age)), aes(x = year, y = median_age_pod), colour = '#0072B2')+
  geom_line(data = complete_men_yobll %>%
              drop_na() %>%
              group_by(year) %>%
              filter(position == 1) %>% 
              distinct(athlete_id, .keep_all = TRUE) %>% 
              mutate(age = year - yob) %>% 
              summarise(median_age_pod = median(age)), aes(x = year, y = median_age_pod), colour = '#0072B2')+
  theme_classic()+
  scale_x_continuous(breaks = seq(1986, 2022, 6))+
  scale_y_continuous(limits = c(23,31),
                     breaks = c(23, 25, 27, 29, 31))+
  labs(title = "Median Men Age",
        y = 'Age',
        x = 'Year')+
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
        legend.position = c(0.8, 0.8))


plot_grid(age_men, age_women)

ggsave(filename = "men_women_age_podium.png",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete",
       width = 12,
       height = 6)



complete_men %>%
  group_by(year) %>%
  distinct(athlete_id, .keep_all = TRUE) %>% 
  mutate(age = year - yob) %>%
  ggplot()+
  geom_histogram(aes(x = age), binwidth = 1)+
  facet_wrap(~year, ncol = 6)

complete_women %>%
  group_by(year) %>%
  distinct(athlete_id, .keep_all = TRUE) %>% 
  mutate(age = year - yob) %>%
  ggplot()+
  geom_histogram(aes(x = age), binwidth = 1)

#Finding the median age of a first race
complete_men %>%
  select(athlete_id,program_id,year,position,yob) %>% 
  drop_na() %>% 
  group_by(year) %>%
  distinct(athlete_id, .keep_all = TRUE) %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age)) 

#Calculating the median age of Men and Women Podium Finishers for 2022
complete_men_yobll %>% 
  drop_na() %>% 
  filter(year == 2022 & position > 0 & position < 4 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

complete_women_yobll %>% 
  drop_na() %>% 
  filter(year == 2022 & position > 0 & position < 4 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

#Calculating the median age of Men and Women Finishers for 2022
complete_men_yobll %>% 
  drop_na() %>% 
  filter(year == 2022 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

complete_women_yobll %>% 
  drop_na() %>% 
  filter(year == 2022 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

# Calculating the median age of Men and Women for podium finsihers from 2012-2022
complete_men_yobll %>% 
  drop_na() %>% 
  filter(year > 2011 & position > 0 & position < 4 & status == '') %>%
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

complete_women_yobll %>% 
  drop_na() %>% 
  filter(year > 2011 & position > 0 & position < 4 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

# Calculating the median age of men and women finishers from 2012-2022

complete_men_yobll %>% 
  drop_na() %>% 
  filter(year > 2011 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

complete_women_yobll %>% 
  drop_na() %>% 
  filter(year > 2011 & status == '') %>% 
  mutate(age = year - yob) %>% 
  summarise(median_age = median(age),
            sd = sd(age)) 

# Calculating the median age of men and women finishers from all years

complete_men %>% 
  select(year,yob,program_id,athlete_id,position,status) %>% 
  filter(status != 'DNS' & position == 1 | position == 2 | position == 3) %>%
  mutate(age = year - yob) %>% 
  dplyr::select(athlete_id, age, year) %>% 
  drop_na() %>% 
  distinct(athlete_id, year, .keep_all = TRUE) %>% #summarise(n = n())
  reframe(median_age = median(age),
            IQR = quantile(age)) 

complete_women %>% 
  select(year,yob,program_id,athlete_id,position,status) %>% 
  filter(status != 'DNS' & position == 1 | position == 2 | position == 3) %>% 
  mutate(age = year - yob) %>% 
  dplyr::select(athlete_id, age, year) %>% 
  drop_na() %>%
  distinct(athlete_id, year, .keep_all = TRUE) %>% #summarise(n = n())
  reframe(median_age = median(age),
            IQR = quantile(age)) 


# Moods median test to see if podium is older than finish

median_test

#### World Map ####

#get the world map
world <- map_data("world")

#create plot


complete_men_yobll %>%
  filter(year > 1999) %>% 
  mutate_if(is.numeric, ~ case_when(. > 5 ~ round(., 2), TRUE ~ ceiling(.))) %>% 
  group_by(longitude, latitude) %>%
  summarize(race_size = n_distinct(program_id)) %>% view()

rbind(complete_men_yobll, complete_women_yobll) %>%
  mutate_if(is.numeric, ~ case_when(. > 5 ~ round(., 0), TRUE ~ ceiling(.))) %>% 
  group_by(longitude, latitude) %>%
  summarize(race_size = n_distinct(program_id)) %>% 
ggplot()+
  geom_polygon(data = world, aes(x=long, y = lat, group = group),  alpha = 0.2) +
  geom_point(aes(x=longitude, y=latitude, size = race_size), alpha = 1, shape = 21, fill ='#E69F00', colour = 'black') +
  scale_size_area(breaks = c(1,8,16,32),
                  max_size = 8,
                  name="Number of Races")+
  theme_void()+
  guides(colour = guide_legend()) +
  theme(plot.title = element_text(face = "bold",size = 11,hjust = 0.13,vjust = 0,lineheight = 1,margin = margin(0, 0, 0, 0)),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size=10),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = NA, color = NA), 
        legend.background = element_rect(fill = NA, color = NA))




ggsave(filename = "world_map.jpg",
       path = "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Complete")


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

plot_grid(men_pp, women_pp, participation_plot, model_pp,
          align = c("hv"),
          scale = 1)

rbind(complete_men_yobll, complete_women_yobll) %>% 
  distinct(program_id, .keep_all = TRUE) %>%
  filter(year > 1999 & year < 2011) %>%
  filter(latitude > 30 | latitude < -30) %>% 
  summarise(n = n())



complete_men %>% 
  filter(year == 2010) %>% 
  group_by(program_id) %>% 
  summarise(n = n_distinct(athlete_id)) %>% 
  mutate(mean = median(n))


complete_women %>% 
  filter(year == 2010) %>% 
  group_by(program_id) %>% 
  summarise(n = n_distinct(athlete_id)) %>% 
  mutate(mean = median(n))

