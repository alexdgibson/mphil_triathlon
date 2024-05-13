
# Alexander D Gibson
# Model testing

# Packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(splines)
library(pROC)
library(probably)
library(plotROC)


# Calibration stuff
# AUCs
roc_df <- roc(val_loop$y_var, val_loop$pred)
plot(roc_df)
auc(roc_df)

ggroc(roc)

# Plot ROCs
#@roc_plot <- ggroc(list(`Male (AUC: 0.94)`=roc_m, `Female (AUC: 0.89)`=roc_w), size = 1)

roc_data <- data.frame(
  FPR = roc_df$specificities,
  TPR = roc_df$sensitivities
)

roc_plot <- ggplot(roc_data, aes(x = 1-FPR, y = TPR)) +
  geom_line(color = "black", size = 1)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color="gray50", linetype="longdash",
               size=0.35)+
  annotate("text", x = 0.8, y = 0.2, label = paste("AUC: 0.78"), color = "black", size = 4) +
  labs(x = 'Specificity',
       y = 'Sensitivity')+
  scale_x_continuous(breaks = c(0.00,0.25,0.50,0.75,1.00),
                     labels = c("1.00","0.75","0.5","0.25","0.00"))+
  theme_bw()+
  theme(axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid = element_blank())


# Window plots (i think you want logistic regression plots) - https://www.tidyverse.org/blog/2022/11/model-calibration/
cal_plot_windowed(.data = val_loop,
                  truth = y_var,
                  estimate = pred,
                  smooth = FALSE,
                  include_rug = FALSE,
                  step_size = 0.1,
                  window_size = 0.6)+
  theme_classic()
  # geom_point(data = val_loop %>% filter(y_var == '1'),
  #            aes(y = 1,
  #                x = pred),
  #            shape = '|')+
  # geom_point(data = val_loop %>% filter(y_var == '0'),
  #            aes(y = 0,
  #                x = pred),
  #            shape = '|')+
  # labs(y = 'Predicted event rate') -> p3; p3 


cal_plot_logistic(.data = val_loop,
                  truth = y_var,
                  estimate = pred,
                  smooth = FALSE)

cal_plot <- ggplot(data = val_loop, aes(x = pred, y = y_var)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  geom_point(data = val_loop %>% filter(y_var == '1'),
             aes(y = 1,
                 x = pred),
             shape = '|')+
  geom_point(data = val_loop %>% filter(y_var == '0'),
             aes(y = 0,
                 x = pred),
             shape = '|')+
  labs(x = "Predicted Probabilities", y = "Observed Probabilities") +
  theme_bw()+
  theme(axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid = element_blank())


cowplot::plot_grid(roc_plot, cal_plot)

ggsave(filename = 'roc_cal_plot.png',
       path = '/Users/alexander/Documents/GitHub/triathlon-performance/triathlon-performance/Images/Performance_Prob',
       width = 8,
       height = 4)

# Brier scores

brier_score_m = (mean((val_loop$y_var-val_loop$pred)^2))/425
round(brier_score_m,6)


brier_score <- (val_loop$y_var - val_loop$pred)^2 / 425
mean(brier_score %>% na.omit())
# Brier scores
male_athletes <- dmod %>%
  mutate(athlete_unique_id = paste0(athlete_first,athlete_last)) %>%
  group_by(athlete_unique_id) %>%
  count() %>%
  nrow(.)
brier_score_m = (mean((df_men$y_var-df_men$pred)^2))/male_athletes
round(brier_score_m,6)
