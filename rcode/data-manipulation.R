### Manipulation of Data ###

# Loading Libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(cowplot)
library(naniar)
library(visdat)
library(reshape2)
library(GGally)
library(janitor)
library(viridis)
library(lubridate)

install.packages('GGally')


# Load raw male and female data

elite_men <- read.csv('/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/elite_men.csv')
elite_women <- read.csv('/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/elite_women.csv')
yoblatlong <- read.csv('/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/Results export v2.csv')


# Taknig YOB and Lat/Long out 
yobll_men <- yoblatlong %>% 
  clean_names() %>% 
  select(program_id, athlete_id, yob, latitude, longitude, category) %>% 
  filter(category == 'Elite Men')

complete_men_yobll <- left_join(complete_men, yobll_men, multiple = "all")

yobll_women <- yoblatlong %>% 
  clean_names() %>% 
  select(program_id, athlete_id, yob, latitude, longitude, category) %>% 
  filter(category == 'Elite Women')

complete_women_yobll <- left_join(complete_women, yobll_women)

# Remove all program_id's (races) with finish time shorter than Olympic distance
# Three clustering, two total time clusters below ~5000 seconds will be < olympic

elite_men %>%
  filter(total_time > 5000, swim > 0 & swim < 3000) %>% 
  ggplot(aes(x=swim, y=total_time))+
  geom_jitter()

## Investigations
elite_men %>% filter(swim > 0 & swim <1000) -> test
nrow(test)

test %>% 
  ggplot(aes(x=swim, y=total_time, colour = as.factor(program_id)))+
  geom_jitter()+
  guides(colour = 'none')

test %>% filter(total_time > 5000) -> p

p %>% 
  ggplot(aes(x=swim, y=total_time, colour = as.factor(program_id)))+
  geom_jitter()+
  guides(colour = 'none')

table(p$program_id)


table(p$year)


men_finish %>% 
  filter()
  filter(total_time >0, swim > 0, swim < 3000) %>% 
  ggplot(aes(x=swim, y=total_time))+
  geom_jitter()

elite_women %>%
  filter(total_time >0, swim >0, swim < 5000) %>% 
  ggplot(aes(x=swim, y=total_time))+
  geom_jitter()


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

elite_women %>% 
  filter(run > 1400, run < 1500, total_time > 0) %>% 
  ggplot(aes(x = run, y = total_time, colour = as.factor(program_id)))+
  geom_jitter()

final_subset_df_f %>%
  filter(run > 0, run < 1800, total_time > 0) %>% 
  ggplot(aes(x = run, y = total_time, colour = as.factor(program_id)))+
  geom_jitter()
  

elite_women %>% 
  filter(program_id == 272018)


bad_f <- c(bad_races_f, bad_races_f2, bad_races_f3, bad_races_f4, bad_races_f5)
  
vis_expect(t, ~.x >= 1)
vis_expect(elite_women, ~.x >= 1)
vis_miss(elite_men, ~t1 >= 1)


cormat <- elite_men %>% filter(swim > 0, t1 > 0, bike > 0, t2 > 0, run > 0, total_time > 0) %>% 
  select(swim, t1, bike, t2, run, total_time) %>%
  drop_na()
cormat <- round(cor(cormat),2)
head(cormat)

melted_cormat <- melt(cormat)

melted_cormat %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value))+
  geom_tile()

ggpairs(cormat2)

cormat2 <- final_subset_df %>% filter(swim > 0, t1 > 0, bike > 0, t2 > 0, run > 0, total_time > 0) %>% 
  select(swim, t1, bike, t2, run, total_time) %>%
  drop_na()



elite_men %>% 
  filter(total_time > 0) %>% 
  ggplot(aes(x = swim, y = total_time, colour = as.factor(program_id)))+
  geom_jitter()+
  guides(colour = 'none')

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

elite_men %>% filter(run > 1000, total_time > 0, run < 1500) %>% 
  ggplot(aes(x = run, y = total_time, colour = as.factor(program_id)))+
  geom_jitter()


final_subset_df %>% 
  filter(program_id == 556957)

bad <- c(bad_races, bad_races2, bad_races3, bad_races4, bad_races5, bad_races6)

final_subset_df %>%
  filter(run > 0, total_time > 0, swim < 20000) %>% 
  ggplot(aes(x = run, y = total_time))+
  geom_jitter()

final_subset_df_f %>% 
  filter(run > 0 & total_time > 0 & swim < 10000) %>% 
  ggplot(aes(x = run, y = total_time))+
  geom_jitter()

final_subset_df_f %>% 
  filter(program_id == '1963')


## Checking to flag all the races which have a timing error

#med_race_time_male <-

final_subset_df %>% 
  filter(status == "") %>% 
  group_by(program_id) %>%
  mutate(median_total_time = median(total_time),
         median_swim = median(swim),
         median_bike = median(bike),
         median_run = median(run)) %>% 
  ungroup() %>% 
  mutate(perc_total_time = median_total_time/total_time,
         perc_swim = swim/median_swim,
         perc_bike = bike/median_bike,
         perc_run = run/median_run) %>%
  filter(status == "" & swim > 0) %>%
  ggplot(aes(x = total_time, y = perc_swim, colour = as.factor(program_id)))+
  geom_point()+
  guides(colour = 'none')



final_subset_df %>% 
  filter(status == "") %>% 
  group_by(athlete_id) %>%
  mutate(median_total_time = median(total_time),
         median_swim = median(swim),
         median_bike = median(bike),
         median_run = median(run)) %>% 
  ungroup() %>% 
  mutate(perc_total_time = median_total_time/total_time,
         perc_swim = swim/median_swim,
         perc_bike = bike/median_bike,
         perc_run = run/median_run) %>%
  filter(status == "" & swim > 0, bike >0) %>%
  ggplot(aes(x = reorder(as.numeric(athlete_id), -perc_bike), y = perc_bike, colour = as.factor(program_id)))+
  geom_point()+
  guides(colour = 'none')


# Calc median
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

p %>%
  ggplot(aes(x = reorder(as.numeric(athlete_id), -perc_run), y = perc_run, colour = as.factor(program_id)))+
  geom_point()+
  guides(colour = 'none')

p %>% 
  filter(a_perc_t1 > 2 & r_perc_t1 > 4) %>% 
  view()

error_timing <- p %>% 
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
  mutate(flag = 'timing') %>% 
  select(program_id, athlete_id, flag)


complete_men <- left_join(final_subset_df, error_timing) %>% 
  view()



p %>%
  filter(swim == total_time | t1 == total_time | bike == total_time | t2 == total_time | run == total_time & total_time > 0) %>% 
  view()
  
final_subset_df %>% 
  filter(status == "" & position == 0)

p %>% 
  filter(program_id == 3993) %>% 
  view()


quantile(p$perc_bike,
         na.rm = T,
         prob = c(0.05,0.95))

summary(p$perc_bike)
nrow(p)

nrow(p %>% filter(perc_bike>1.1))

final_subset_df %>% 
  filter(program_id == 151599) 
  


t <- elite_men %>% 
  filter(status == '')

elite_men %>% 
  filter(swim > 0 & t1 > 0 & bike > 0 & t2 > 0 & run > 0) %>% 
  view()

# selecting for the program id's below ~5000
# All races with a time below 5000 and not 0


subset_e <- subset(elite_men, total_time < 5900 & total_time > 0)
program_ids <- subset_e$program_id
final_subset_df <- subset(elite_men, !program_id %in% program_ids)

subset_e <- subset(elite_men, total_time < 5900 & total_time > 0)
program_ids <- subset_e$program_id


final_subset_df <- subset(elite_men, !program_id %in% bad)
final_subset_df_f <- subset(elite_women, !program_id %in% bad_f)

subset_e_f <- subset(elite_women, total_time < 5000 & total_time > 0)
program_ids_f <- subset_e_f$program_id
final_subset_df_f <- subset(elite_women, !program_id %in% program_ids_f)

subset_e_f <- subset(elite_women, total_time < 6600 & total_time >0)
program_ids_f <- subset_e_f$program_id
final_subset_df_f <- subset(elite_women, !program_id %in% program_ids_f)

final_subset_df %>%
  filter(total_time >0, bike >0) %>% 
  ggplot(aes(x=bike, y=total_time))+
  geom_jitter()


final_subset_df_f %>%
  filter(total_time >0, bike >0, bike < 10000) %>% 
  ggplot(aes(x=bike, y=total_time))+
  geom_jitter()

final_subset_df_f %>%
  filter(total_time >0, run >0, run < 20000) %>% 
  ggplot(aes(x=run, y=total_time))+
  geom_jitter()

final_subset_df_f %>% 
  filter(total_time > 0, swim > 0, swim < 20000) %>% 
  ggplot(aes(x=swim, y = total_time))+
  geom_jitter()


# Taking results with all 6 times and creating position for men

men_df1 <- complete_men %>%
  filter(status == '') %>%
  mutate(c_swim = swim,
         c_t1 = swim + t1,
         c_bike = swim + t1 + bike,
         c_t2 = swim + t1 + bike + t2,
         c_run = swim + t1 + bike + t2 + run)

men_df2 <- complete_men %>% 
  filter(status != '') %>% 
  mutate(c_swim = ifelse(swim == 0, 0, swim),
         c_t1 = ifelse(t1 == 0, 0, swim + t1),
         c_bike = ifelse(bike == 0, 0, swim + t1 + bike),
         c_t2 = ifelse(t2 == 0, 0, swim + t1 + bike + t2),
         c_run = ifelse(run == 0, 0, total_time))

men_df2 <- complete_men %>%
  mutate(
    c_swim = if_else(status != "" & swim == 0, 0, swim),
    c_t1 = if_else(status != "" & t1 == 0, 0, swim + t1),
    c_bike = if_else(status != "" & bike == 0, 0, swim + t1 + bike),
    c_t2 = if_else(status != "" & t2 == 0, 0, swim + t1 + bike + t2),
    c_run = if_else(status != "" & run == 0, 0, total_time))


men_cumul <- full_join(men_df1, men_df2)



men_finish <- men_df2 %>% 
  group_by(program_id) %>% 
  mutate(pos_swim = ifelse(c_swim > 0, rank(c_swim[c_swim > 0], ties.method='min'), 0),
         pos_t1 = ifelse(c_t1 > 0, rank(c_t1[c_t1 > 0], ties.method='min'), 0),
         pos_bike = ifelse(c_bike > 0, rank(c_bike[c_bike > 0], ties.method='min'), 0),
         pos_t2 = ifelse(c_t2 > 0, rank(c_t2[c_t2 > 0], ties.method='min'), 0),
         pos_run = ifelse(c_run > 0, rank(c_run[c_run > 0], ties.method='min'), 0))

saveRDS(men_finish, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/elite_men_positions.RDS')



  # Taking results with all 6 times and creating position for women
  
  women_df1 <- final_subset_df_f %>%
    filter(status == '') %>%
    mutate(c_swim = swim,
           c_t1 = swim + t1,
           c_bike = swim + t1 + bike,
           c_t2 = swim + t1 + bike + t2,
           c_run = total_time)
  
  women_df2 <- final_subset_df_f %>% 
    filter(status != '') %>% 
    mutate(c_swim = ifelse(swim == 0, 0, swim),
           c_t1 = ifelse(t1 == 0, 0, swim + t1),
           c_bike = ifelse(bike == 0, 0, swim + t1 + bike),
           c_t2 = ifelse(t2 == 0, 0, swim + t1 + bike + t2),
           c_run = ifelse(run == 0, 0, swim + t1 + bike + t2 + run))
  
  women_cumul <- full_join(women_df1, women_df2)
  
  women_finish <- women_cumul %>% 
    group_by(program_id) %>% 
    mutate(pos_swim = ifelse(c_swim == 0, 0,rank(c_swim, ties.method='first')),
           pos_t1 = ifelse(c_t1 == 0, 0,rank(c_t1, ties.method='first')),
           pos_bike = ifelse(c_bike == 0, 0,rank(c_bike, ties.method='first')),
           pos_t2 = ifelse(c_t2 == 0, 0,rank(c_t2, ties.method='first')),
           pos_run = ifelse(c_run == 0, 0,rank(c_run, ties.method='first')))

  
  women_df2 <- complete_women %>%
    mutate(
      c_swim = if_else(status != "" & swim == 0, 0, swim),
      c_t1 = if_else(status != "" & t1 == 0, 0, swim + t1),
      c_bike = if_else(status != "" & bike == 0, 0, swim + t1 + bike),
      c_t2 = if_else(status != "" & t2 == 0, 0, swim + t1 + bike + t2),
      c_run = if_else(status != "" & run == 0, 0, total_time))
  
  
  
  women_finish <- women_df2 %>% 
    group_by(program_id) %>% 
    mutate(pos_swim = ifelse(c_swim > 0, rank(c_swim[c_swim > 0], ties.method='min'), 0),
           pos_t1 = ifelse(c_t1 > 0, rank(c_t1[c_t1 > 0], ties.method='min'), 0),
           pos_bike = ifelse(c_bike > 0, rank(c_bike[c_bike > 0], ties.method='min'), 0),
           pos_t2 = ifelse(c_t2 > 0, rank(c_t2[c_t2 > 0], ties.method='min'), 0),
           pos_run = ifelse(c_run > 0, rank(c_run[c_run > 0], ties.method='min'), 0))
  
  saveRDS(women_finish, file = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/elite_women_positions.RDS')
  
  
  ###############
  
  ### Creating Descriptive Plots 
  # Yearly athletes
  
  athlete_years <- bind_rows(
    men_finish %>% 
      group_by(year) %>% 
      distinct(athlete_id) %>% 
      summarise(n = n(), gender = "Men"),
    women_finish %>% 
      group_by(year) %>% 
      distinct(athlete_id) %>% 
      summarise(n = n(), gender = "Women"))
  
  
athlete_years_w <- athlete_years_w %>% 
  mutate(year = year...1, men = n...2, women = n...5) %>% 
  select(year, men, women)

athlete_years_w <- athlete_years_w %>% 
  mutate(perc_w = women/(men+women),
         perc_m = men/(men+women))

participation <- athlete_years_w %>%
  filter(year > 1988) %>% 
  ggplot(aes(y = perc, x = year))+
  geom_line(size = 1.4, colour = '#8C3F4D')+
  theme_cowplot()+
  labs(title = "Proportion of Females",
       x = "Year")+
  scale_y_continuous(breaks = c(0.30, 0.35, 0.40, 0.45),
                    labels = c("30%", "35%", "40%", "45%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(colour = 'gray75'),
        panel.grid.minor.y = element_line(linetype = "dotted", colour = 'gray85'),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title.y = element_blank())

  unique_year <- athlete_years %>%
    filter(year > 1988) %>% 
    ggplot(aes(x=year,y=n,fill=gender)) +
    geom_col(color = "white", size = 0.3, show.legend = FALSE)+
    scale_fill_manual(values = c('#3E606F', '#8C3F4D')) +
    labs(x='Year',
         y = 'Number of Athletes',
         fill = "Gender",
         title = 'Unique Athletes Competing Per Year',
         subtitle = "Any number of races") +
    theme_cowplot() +
    facet_wrap(~gender)+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.line.y = element_line(color = "gray20"),
          axis.line.x = element_line(color = "gray20"),
          panel.grid.major.y = element_line(color = "gray80"),
          panel.grid.minor.y = element_line(color = "gray95"),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 16),
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  
  
  athlete_years %>%
    ggplot(aes(x=year, colour=gender)) +
    geom_linerange(data = athlete_years[athlete_years$gender=="Men",], 
                   aes(ymin = 0, ymax = 0-n), size = 3.5, alpha = 0.8)+
    geom_linerange(data = athlete_years[athlete_years$gender=="Women",], 
                   aes(ymin = 0, ymax = 0+n), size = 3.5, alpha = 0.8)+
    geom_label(aes(x = year, y = 0, label = year),
               inherit.aes = F,
               size = 3.5, label.padding = unit(0.0, "lines"), label.size = 0,
               label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.9, color = "#5D646F")+
    theme_classic()+
    coord_flip()+
    labs(title = "Athletes Per Year",
         subtitle = "Standard Distance")+
    scale_color_manual(name = "Gender", values = c(Men = "#3E606F", Women = "#8C3F4D"),
                       labels = c("Men", "Women"))+
    scale_y_continuous(breaks = c(c(-2000, -1500, -1000, -500, 0) - 30, c(0, 250, 500, 1000, 1250) + 30),
                       labels = c("2000", "1500", "1000", "500", "0", "0", "250", "500", "1000", "1250"))+
    theme_minimal()+
    theme(text = element_text(color = "#3A3F4A"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 24, margin = margin(b = 10), hjust = 0.030),
          plot.subtitle = element_text(size = 14, margin = margin(b = 20), hjust = 0.030),
          plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
          axis.text.x = element_text(size = 12, color = "#5D646F"),
          axis.text.y = element_blank(),
          strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
          plot.background = element_rect(fill = "#EFF2F4"),
          plot.margin = unit(c(2, 2, 2, 2), "cm"),
          legend.position = "top",
          legend.spacing = unit(0, "lines"),
          legend.text  = element_text(size = 14),
          legend.text.align = 0)
  




ggsave('athletes_per_year.tiff',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Descriptive',
       width = 16, height = 9)

  
# Survivorship Plots
  
ymc <- final_subset_df %>%
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=n))+
  geom_histogram(binwidth = 1, fill = "#3E606F", color = "#FFFFFF") +
  labs(x = 'Years',
       y = 'Number of Athletes',
       title = 'Years Men Spend Competing') +
  theme_classic() +
  scale_y_log10()+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line.y = element_line(color = "gray20"),
        axis.line.x = element_line(color = "gray20"),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray95"))
  
ywc <- final_subset_df_f %>%
  group_by(athlete_id) %>% 
  distinct(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=n))+
  geom_histogram(binwidth = 1, fill = "#8C3F4D", color = "#FFFFFF") +
  labs(x = 'Years',
       y = 'Number of Athletes',
       title = 'Years Women Spend Competing') +
  theme_classic() +
  scale_y_log10()+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line.y = element_line(color = "gray20"),
        axis.line.x = element_line(color = "gray20"),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray95"))

# Podiums Per Country

final_subset_df %>%
  filter(position == 1) %>%
  group_by(year, country) %>%
  summarise(count = n()) %>%
  top_n(1, count) %>%
  ggplot(aes(x = year, y = count, fill = country)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Year",
       y = "Number of First Place Finishes",
       title = "Total First Place Finishes by Year and Country") +
    theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

men_finish %>%
  filter(position == 1) %>%
  group_by(year, country) %>%
  summarise(count = n()) %>%
  top_n(1, count) %>% 
  ungroup(country, year) %>%
  gt() %>% 
  cols_label(year = 'Year',
             country = 'Country',
             count = 'Count') %>% 
  tab_header(title = "Country with the most wins per year")

# Tricky to do this as a good graph ^^

# Number of Races Athletes Compete in

rms <- final_subset_df %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "#3E606F",
                 color = "#FFFFFF")+
  theme_classic()+
  scale_y_log10()+
  labs(title = 'Total Races Male Triathletes Start',
       subtitle = 'Binwidth = 5',
       x = 'Number of Races',
       y = 'Number of Athletes')+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line.y = element_line(color = "gray20"),
        axis.line.x = element_line(color = "gray20"),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray95"))
  
  
rws <- final_subset_df_f %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>%
  ggplot(aes(x = n))+
  geom_histogram(binwidth = 5,
                 fill = "#8C3F4D",
                 color = "#FFFFFF")+
  theme_classic()+
  labs(title = 'Total Races Women Triathletes Start',
       subtitle = 'Binwidth = 5',
       x = 'Number of Races',
       y = 'Number of Athletes')+
  scale_y_log10()+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line.y = element_line(color = "gray20"),
        axis.line.x = element_line(color = "gray20"),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray95"))

# Total wins per country for top 10 countries

wins_country_men <- men_finish %>% 
    filter(position == 1) %>% 
    group_by(country) %>% 
    summarise(wins = n()) %>% 
    arrange(desc(wins)) %>%
    select(country, wins)

wins_country_women <- women_finish %>% 
    filter(position == 1) %>% 
    group_by(country) %>% 
    summarise(wins = n()) %>% 
    arrange(desc(wins)) %>%
    select(country, wins)

wins_country_men %>%
  head(10) %>%
  mutate(country = factor(country, levels = country[order(wins, decreasing = TRUE)])) %>% 
  ggplot(aes(x = country, y = wins))+
  geom_col(color = "black", size = 0.2, fill = "#3E606F")+
  theme_minimal()+
  labs(title = 'Male Wins by Country',
       x = 'Country',
       y = 'Wins')+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'italic'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

wins_country_women %>%
  head(10) %>%
  mutate(country = factor(country, levels = country[order(wins, decreasing = TRUE)])) %>% 
  ggplot(aes(x = country, y = wins))+
  geom_col(color = "black", size = 0.2, fill = "#8C3F4D")+
  theme_minimal()+
  labs(title = 'Women Wins by Country',
       x = 'Country',
       y = 'Wins')+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'italic'),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# Creating plots of segment times over the years for male and female

mt <- ggplot(data = final_subset_df %>%
         filter(position == 1, total_time > 0) %>%
         group_by(year) %>% 
         drop_na() %>%
         mutate(mean_tt_1 = mean(total_time), color_var = "First Position"), aes(x = year)) +
  geom_point(aes(y = mean_tt_1, color = color_var), size = 2.5, show.legend = FALSE) +
  geom_line(aes(y = mean_tt_1, color = color_var), show.legend = FALSE, size = 1.5) +
  geom_point(data = final_subset_df %>%
               group_by(year) %>% 
               filter(total_time > 0, status == "") %>% 
               drop_na() %>%
               mutate(mean_tt = mean(total_time), color_var = "Average"), 
             aes(y = mean_tt, color = color_var), size = 2.5, show.legend = FALSE) +
  geom_line(data = final_subset_df %>%
              group_by(year) %>% 
              filter(total_time > 0, status == "") %>% 
              drop_na() %>%
              mutate(mean_tt = mean(total_time), color_var = "Average"), 
            aes(y = mean_tt, color = color_var), show.legend = FALSE, size = 1.5) +
  scale_color_manual(name = "Legend", values = c("First Position" = "#E69F00", "Average" = "black")) +
  theme_minimal() +
  labs(title = 'Yearly Mean Finish Time Men',
       x = 'Year',
       y = ' Time (sec)') +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



wt <- ggplot(data = final_subset_df_f %>%
         filter(position == 1, total_time > 0) %>%
         group_by(year) %>%
         drop_na() %>%
         mutate(mean_tt_1 = mean(total_time), color_var = "First Position"), aes(x = year)) +
  geom_point(aes(y = mean_tt_1, color = color_var), size = 2.5) +
  geom_line(aes(y = mean_tt_1, color = color_var), size = 1.5) +
  geom_point(data = final_subset_df_f %>%
               group_by(year) %>% 
               filter(total_time > 0 & status == "") %>%
               drop_na() %>% 
               mutate(mean_tt = mean(total_time), color_var = "Average"), 
             aes(y = mean_tt, color = color_var), size = 2.5) +
  geom_line(data = final_subset_df_f %>%
              group_by(year) %>% 
              filter(total_time > 0 & status == "") %>% 
              drop_na() %>%
              mutate(mean_tt = mean(total_time), color_var = "Average"), 
            aes(y = mean_tt, color = color_var), size = 1.5) +
  scale_color_manual(name = "", values = c("First Position" = "#E69F00", "Average" = "black")) +
  theme_minimal() +
  labs(title = 'Yearly Mean Finish Time Women',
       x = 'Year',
       y = ' Time (sec)') +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

final_subset_df_f %>% 
  filter(year == 2018) %>% 
  filter(position == 1) %>%
  view()

final_subset_df_f %>% 
  filter(year == 2018) %>% 
  view()
  mutate(m = mean(total_time))

#Code below was used to check the longer distance races
# women_finish %>%
#   filter(position == 1) %>%
#   ggplot(aes(x=total_time))+
#   geom_boxplot()
# 
# women_finish %>%
#   filter(position == 1, total_time > 0) %>%
#   view()

# Swim Bike Run averages

sbr_m <- men_finish %>% 
  filter(position == 1 & total_time < 13000 & swim > 0 & bike > 0 & run > 0 & swim < 5000 & run < 3500) %>% 
  group_by(year) %>% 
  mutate(med_swim = median(swim),
         med_bike = median(bike),
         med_run = median(run),
         sd_swim = sd(swim),
         sd_bike = sd(bike),
         sd_run = sd(run)) %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = med_swim, colour = "Swim"), size = 1.3)+
  geom_line(aes(y = med_bike, colour = 'Bike'), size = 1.3)+
  geom_line(aes(y = med_run, colour = 'Run'), size = 1.3)+
  geom_point(aes(y = med_swim, colour = 'Swim'), size = 1.3)+
  geom_point(aes(y = med_bike, colour = 'Bike'), size = 1.3)+
  geom_point(aes(y = med_run, colour = 'Run'), size = 1.3)+
  geom_errorbar(aes(ymin = med_swim - sd_swim, ymax = med_swim + sd_swim), width = 0.2, colour = '#000080') +
  geom_errorbar(aes(ymin = med_bike - sd_bike, ymax = med_bike + sd_bike), width = 0.2, colour = '#395930') +
  geom_errorbar(aes(ymin = med_run - sd_run, ymax = med_run + sd_run), width = 0.2, colour = '#d51609') +
  theme_cowplot()+
  labs(title = "Median Segment Times for Men",
       x = "Year",
       y = "Time (sec)")+
  scale_colour_manual(name = "", values = c("Swim" = "#000080", "Bike" = "#395930", "Run" = "#d51609"))+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "gray85"),
        panel.grid.minor.y = element_line(color = "gray95"))

sbr_w <- women_finish %>% 
  filter(position == 1 & total_time < 13000 & swim > 0 & bike > 0 & run > 0 & swim < 5000 & run < 3500) %>% 
  group_by(year) %>% 
  mutate(med_swim = median(swim),
         med_bike = median(bike),
         med_run = median(run),
         sd_swim = sd(swim),
         sd_bike = sd(bike),
         sd_run = sd(run)) %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = med_swim, colour = "Swim"), size = 1.3)+
  geom_line(aes(y = med_bike, colour = 'Bike'), size = 1.3)+
  geom_line(aes(y = med_run, colour = 'Run'), size = 1.3)+
  geom_point(aes(y = med_swim, colour = 'Swim'), size = 1.3)+
  geom_point(aes(y = med_bike, colour = 'Bike'), size = 1.3)+
  geom_point(aes(y = med_run, colour = 'Run'), size = 1.3)+
  geom_errorbar(aes(ymin = med_swim - sd_swim, ymax = med_swim + sd_swim), width = 0.2, colour = '#000080') +
  geom_errorbar(aes(ymin = med_bike - sd_bike, ymax = med_bike + sd_bike), width = 0.2, colour = '#395930') +
  geom_errorbar(aes(ymin = med_run - sd_run, ymax = med_run + sd_run), width = 0.2, colour = '#d51609') +
  theme_cowplot()+
  labs(title = "Median Segment Times for Women",
       x = "Year",
       y = "Time (sec)")+
  scale_colour_manual(name = "", values = c("Swim" = "#000080", "Bike" = "#395930", "Run" = "#d51609"))+
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_line(color = "gray85"),
        panel.grid.minor.y = element_line(color = "gray95"))


men_finish %>% 
  filter(status == "" & swim > 780 & t1 > 0 & bike > 1500 & t2 > 0 & run > 1000 &
           swim < 2000 & t1 < 200 & bike < 5500 & t2 < 200 & run < 3500)

## Add all segment times up ##
final_subset_df %>% 
  filter(status == "", swim >0, bike > 0, run > 0, t1 > 0, t2 >0, t2 <2000, t1 < 600) %>% 
  drop_na() %>% 
  group_by(year) %>%
  mutate(cont_swim = sum(swim),
         cont_bike = sum(bike),
         cont_run = sum(run),
         cont_t1 = sum(t1),
         cont_t2 = sum(t2)) %>%
  summarise(s = mean(cont_swim),
            b = mean(cont_bike),
            r = mean(cont_run),
            t1 = mean(cont_t1),
            t2 = mean(cont_t2),
            total = s + b + r + t1 + t2) %>%
  select(year, s, b, r, t1, t2, total) %>%
  mutate(sp = (s/total)*100,
         bp = (b/total)*100,
         rp = (r/total)*100,
         t1p = (t1/total)*100,
         t2p = (t2/total)*100,
         tt = sp + bp + rp + t1p + t2p) %>% 
  select(year, sp, bp, rp, t1p, t2p, tt) %>% 
  ggplot(aes(x = year, y = sp + bp + rp + t1p + t2p))+
  geom_col(fill = '#56B4E9')+
  geom_col(aes(y = t1p+bp+t2p+rp), fill = 'black')+
  geom_col(aes(y = bp+t2p+rp), fill = '#009E73')+
  geom_col(aes(y = t2p+rp), fill = 'black')+
  geom_col(aes(y = rp), fill = '#E69F00')+
  theme_cowplot()+
  labs(title = "Segment Contribution Over Time",
       x = "Year",
       y = '%')

contribution_diff <- contribution %>%
  mutate(across(everything(), ~ ifelse(is.na(.), ., . - lag(.)))) %>% 
  rename_with(~ paste0("diff_", .))

contrib <- cbind(contribution, contribution_diff)

contrib %>% 
  group_by(year) %>% 
  ggplot(aes(x = year))+
  geom_col(aes(y = diff_sp, fill = 'swim'))+
  geom_col(aes(y = diff_bp, fill = 'bike'))+
  geom_col(aes(y = diff_rp, fill = 'run'))



### Creating some plots
plot_grid(ymc, ywc, rms, rws,
          nrow = 2)
ggsave('multi_men_women.tiff', 
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Descriptive',
       width = 16, height = 9)

plot_grid(mt, wt,
          nrow = 2,
          rel_heights = c(1,1),
          rel_widths = c(1,1))
ggsave('time_men_women.tiff',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Descriptive',
       width = 12, height = 10,
       bg = 'white')

plot_grid(unique_year, participation,
          nrow = 2,
          rel_heights = c(1,1),
          rel_widths = c(1.5,1))
ggsave('plot_1.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Descriptive',
       width = 16, height = 8,
       bg = 'white')

plot_grid(sbr_m, sbr_w)
ggsave('sbr.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Descriptive',
       width = 15, height = 7,
       bg = 'white')


### Moving from plots to numbered general stats

# Number of Athletes
unique(final_subset_df$athlete_id)
unique(final_subset_df_f$athlete_id)

# Number of Races
unique(final_subset_df$program_id)
unique(final_subset_df_f$program_id)

# Average Races per Athlete

final_subset_df %>%
  filter(status =="") %>% 
  group_by(athlete_id) %>% 
  summarise(program_id) %>%
  count(athlete_id) %>% 
  colSums()
49936/10266

women_finish %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(program_id) %>%
  count(athlete_id) %>% 
  colSums()
34832/5208

# Races per year
unique(final_subset_df$year)
1662/37

unique(final_subset_df_f$year)
1673/37
  
# Average Total time
final_subset_df %>%
  filter(total_time > 0) %>% 
  .$total_time %>% 
  median()

final_subset_df_f %>%
  filter(total_time >0) %>% 
  .$total_time %>% 
  median()

# Average Swim Bike Run
final_subset_df %>%
  filter(swim > 0) %>% 
  filter(status == "") %>% 
  .$swim %>% 
  median()

final_subset_df %>%
  filter(status == "") %>% 
  filter(bike > 0) %>% 
  .$bike %>% 
  median()

final_subset_df %>%
  filter(status == "") %>% 
  filter(run > 0) %>% 
  .$run %>% 
  median()

final_subset_df_f %>%
  filter(swim > 0) %>% 
  filter(status == "") %>% 
  .$swim %>% 
  median()

final_subset_df_f %>%
  filter(status == "") %>% 
  filter(bike > 0) %>% 
  .$bike %>% 
  median()

final_subset_df_f %>%
  filter(status == "") %>% 
  filter(run > 0) %>% 
  .$run %>% 
  median()

# Fastest Total Time
final_subset_df %>%
  filter(status == "") %>% 
  filter(total_time > 0) %>%
  .$total_time %>% 
  min()

final_subset_df_f %>%
  filter(status == "") %>% 
  filter(total_time > 5600) %>%
  .$total_time %>% 
  min()


# Fastest Swim Bike Run
final_subset_df %>%
  filter(status =='') %>% 
  filter(swim > 0) %>% 
  .$swim %>% 
  min()


final_subset_df_f %>%
  filter(status =='') %>% 
  filter(swim > 0) %>% 
  .$swim %>% 
  min()


final_subset_df %>%
  filter(status =='') %>% 
  filter(bike > 2300) %>% 
  .$bike %>% 
  min()


final_subset_df_f %>%
  filter(status =='') %>% 
  filter(bike > 2977) %>% 
  .$bike %>% 
  min()

final_subset_df %>%
  filter(status =='') %>% 
  filter(run > 1200) %>% 
  .$run %>% 
  min()

final_subset_df %>%
  filter(run < 1200, run > 0, total_time > 0) %>% 
  ggplot(aes(x = run, y = total_time, colour = as.factor(program_id)))+
  geom_jitter()


final_subset_df_f %>%
  filter(status =='') %>% 
  filter(run > 1216) %>% 
  .$run %>% 
  min()

men_finish %>% 
  ggplot()

men_finish %>% 
  filter(swim < 4000 & swim > 0 & total_time > 0) %>% 
  filter(status == "") %>%
  select(swim) %>% 
  ggplot(aes(x = swim, y = 1))+
  geom_point()



# Most races started
final_subset_df %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()
  
final_subset_df_f %>%
  filter(status != 'DNS') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()

#Most races Finished
final_subset_df %>%
  filter(status == '') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()

final_subset_df_f %>%
  filter(status == '') %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()


# Most wins by an athlete
final_subset_df %>%
  filter(position == 1) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()

final_subset_df_f %>%
  filter(position == 1) %>% 
  group_by(athlete_id) %>% 
  summarise(n = length(program_id)) %>% 
  .$n %>% 
  max()

# Most wins by a country

final_subset_df %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(n = length(country)) %>% 
  .$n %>% 
  max()

final_subset_df_f %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(n = length(country)) %>% 
  .$n %>% 
  max()

#Country with most wins

final_subset_df %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(wins = n()) %>%
  arrange(desc(wins)) %>% 
  select(country, wins)
  
final_subset_df_f %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(wins = n()) %>%
  arrange(desc(wins)) %>% 
  select(country, wins)

final_subset_df %>% 
  filter(program_id == 1863)


complete_men %>% 
  filter(status == "DNF") %>% 
  group_by(year) %>% 
  summarise(n =n()) %>% 
  ggplot(aes(x = year, y = n))+
  geom_col()


wins_country_men <- complete_men %>% 
  filter(position == 1) %>% 
  group_by(country, year) %>% 
  summarise(n = n()) %>%
  mutate(cumulative_n = cumsum(n)) %>%
  arrange(desc(cumulative_n)) %>% 
  filter(year == 2022)

top_10_men = complete_men %>% 
  filter(position == 1) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>%
  arrange(-(n)) %>%
  slice(1:10) %>%
  select(-n)
  
complete_men %>% 
  filter(position == 1) %>%
  filter(country %in% top_10_men$country) %>% 
  group_by(country, year) %>% 
  summarise(n = n()) %>%
  mutate(cumulative_n = cumsum(n)) %>%
  ggplot(aes(x = year, y = cumulative_n))+
  geom_line(linewidth = 2, aes(colour = as.factor(country)))+
  scale_color_viridis(discrete=TRUE)+
  geom_text(data = complete_men %>% 
              filter(position == 1) %>%
              filter(country %in% top_10$country) %>% 
              group_by(country, year) %>% 
              summarise(n = n()) %>%
              mutate(cumulative_n = cumsum(n)) %>%
              filter(year == 2022), aes(label = country), hjust = 0)+
  theme_classic()+
  labs(title = 'First Place Finishes',
       y = 'Count',
       x = 'Year')+
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





subset(yoblatlong, Program.ID %in% para_races)

pt_races <- yoblatlong %>% 
  filter(str_detect(Category, pattern = "PT")) %>%
  .$Program.ID %>% 
  unique()

awad_races <- yoblatlong %>% 
  filter(str_detect(Category, pattern = "AWAD")) %>%
  .$Program.ID %>% 
  unique()

para_races <- yoblatlong %>% 
  filter(str_detect(Category, pattern = "Para")) %>%
  .$Program.ID %>% 
  unique()

para <- c(pt_races, awad_races, para_races)

para_dataset <- subset(yoblatlong, Program.ID %in% para)




saveRDS(para_dataset, "/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/para_dataset.RDS")





### Updating data with 2023 data ###
### load in data ###
update_2023_data <- read.csv('/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Data/2023_update_data.csv')

### select only 2023 data ###
update_2023_data <- 
  clean_names(update_2023_data) %>% 
  filter(year == 2023, category == "Elite Men" | category == "Elite Women")

### turn time into seconds ###

update_2023_data$total_time <- strptime(update_2023_data$total_time, format = "%H:%M:%OS")
update_2023_data$total_time <- hour(update_2023_data$total_time) * 3600 + minute(update_2023_data$total_time) * 60 + second(update_2023_data$total_time)

update_2023_data$swim <- strptime(update_2023_data$swim, format = "%H:%M:%OS")
update_2023_data$swim <- hour(update_2023_data$swim) * 3600 + minute(update_2023_data$swim) * 60 + second(update_2023_data$swim)

update_2023_data$bike <- strptime(update_2023_data$bike, format = "%H:%M:%OS")
update_2023_data$bike <- hour(update_2023_data$bike) * 3600 + minute(update_2023_data$bike) * 60 + second(update_2023_data$bike)

update_2023_data$run <- strptime(update_2023_data$run, format = "%H:%M:%OS")
update_2023_data$run <- hour(update_2023_data$run) * 3600 + minute(update_2023_data$run) * 60 + second(update_2023_data$run)

update_2023_data$t1 <- strptime(update_2023_data$t1, format = "%H:%M:%OS")
update_2023_data$t1 <- hour(update_2023_data$t1) * 3600 + minute(update_2023_data$t1) * 60 + second(update_2023_data$t1)

update_2023_data$t2 <- strptime(update_2023_data$t2, format = "%H:%M:%OS")
update_2023_data$t2 <- hour(update_2023_data$t2) * 3600 + minute(update_2023_data$t2) * 60 + second(update_2023_data$t2)

view(update_2023_data)

### clean the data for erroneous races/times ###

bad_races_2023 <- update_2023_data %>% 
  filter(bike < 3000 & total_time < 5000 & bike > 0 & total_time > 0) %>% 
  .$program_id %>% 
  unique

bad_races_2023_2 <- update_2023_data %>% 
  filter(swim < 821 & total_time > 0 & swim > 0) %>% 
  .$program_id %>% 
  unique()

bad_races_2023_3 <- update_2023_data %>% 
  filter(total_time < 5400 & total_time > 0) %>% 
  .$program_id %>% 
  unique()

bad_races_2023_4 <- update_2023_data %>% 
  filter(total_time > 0 & bike > 0 & bike < 1000) %>% 
  .$program_id %>% 
  unique()

bad_races_2023_5 <- update_2023_data %>% 
  filter(total_time > 0, swim > 6000) %>% 
  .$program_id %>% 
  unique()

bad_races_2023_6 <- update_2023_data %>% 
  filter(total_time > 0, run < 1100, run > 0) %>% 
  .$program_id %>% 
  unique()

# joining the bad races together
bad_2023 <- c(bad_races_2023, bad_races_2023_2, bad_races_2023_3, bad_races_2023_4, bad_races_2023_5, bad_races_2023_6)

# Setting NA to 0
elite_men_NA_2023 <- update_2023_data

elite_men_NA_2023[rowSums(is.na(elite_men_NA_2023)) > 0,] %>% 
  view()

elite_men_NA_2023 <- replace_na(elite_men_NA_2023, list(swim = 0, t1 = 0, bike = 0, t2 = 0, run = 0, total_time = 0))

# removing the bad races

final_2023_update <- subset(elite_men_NA_2023, !program_id %in% bad_2023)
view(final_2023_update)


### append 2023 to the main data frame ###
### Append 2023 men to the complete_men data frame ###

complete_men <-
  final_2023_update %>% 
  filter(category == "Elite Men") %>% 
  bind_rows(complete_men_yobll) %>% 
  view()

complete_women <-
  final_2023_update %>% 
  filter(category == "Elite Women") %>% 
  bind_rows(complete_women_yobll) %>% 
  view()
