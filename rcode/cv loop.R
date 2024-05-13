
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

# Leave one (athlete) out cross validation for men
results <- data.frame()

for (k in unique(pos_model_podium$program_id)) {
  
  # Create training and validation datasets
  train_dat <- dplyr::filter(pos_model_podium, program_id != k)
  val_dat <- dplyr::filter(pos_model_podium, program_id == k)
  
  # Fit the logistic regression model using glm
  fit_loop <- glm(formula = podium ~ pos_swim*pos_bike*category + age*category,
                        family = binomial(link = 'logit'),
                        data = pos_model_podium)
  
  # Predict on validation set
  pred_prob <- predict(fit_loop, newdata = val_dat, type = "response")
  
  # Store the results
  val_dat$predicted_prob = pred_prob
  results <- rbind(results, val_dat)
}

#write.csv(results, file = 'results_cv_men.csv', row.names = F)

# From model
pred = results$predicted_prob
y_var <- as.integer(as.logical(pos_model_podium$podium))
  

val_loop = cbind(y_var,pred) %>%
  as.data.frame()
