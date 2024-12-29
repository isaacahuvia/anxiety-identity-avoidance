# ---------------------------------------------------------------------------- #
# Explore Marginal Effects Plots
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# For info on (a) ordinal models using "brms" and (b) types of posteriors, see
# https://mvuorre.github.io/brms-workshop/posts/ordinal/slides.html#1
# https://www.andrewheiss.com/blog/2022/09/26/guide-visualizing-types-posteriors/

# ---------------------------------------------------------------------------- #
# Check correct R version and load packages ----
# ---------------------------------------------------------------------------- #

# R version 4.1.2

library(tidyverse) # 2.0.0
library(here)      # 1.0.1
library(brms)      # 2.20.4
library(psych)     # 2.3.9

# ---------------------------------------------------------------------------- #
# Import models controlling for anxiety symptom severity ----
# ---------------------------------------------------------------------------- #

model_ba_control <- readRDS("./models/model_ba_control.rds")
model_ea_control <- readRDS("./models/model_ea_control.rds")

# ---------------------------------------------------------------------------- #
# Plot thresholds and attempt to compute marginal effects from simplexes ----
# ---------------------------------------------------------------------------- #

# Extract and plot thresholds for situational avoidance

summary(model_ba_control)
bayes_R2(model_ba_control)
ba_control_thresholds <- fixef(model_ba_control)[paste0("Intercept[", 1:4, "]"), "Estimate"]

x <- seq(-4, 4, length=100)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "SD from M", ylab = "Density",
     main = "Thresholds of Latent Situational Avoidance")
abline(v = ba_control_thresholds, lty = 2)

# Extract anxiety identity effect and simplexes for situational avoidance and
# attempt to compute marginal effects of anxiety identity from simplexes

ba_control_moanxiety_identity_Estimate <- fixef(model_ba_control)["moanxiety_identity", "Estimate"]
round(ba_control_moanxiety_identity_Estimate, 4) == 0.3375

ba_control_moanxiety_identity_tot_chg <- ba_control_moanxiety_identity_Estimate*4
round(ba_control_moanxiety_identity_tot_chg, 4) == 1.3499

ba_control_df <- as.data.frame(model_ba_control)
ba_control_simo1 <- mean(ba_control_df$`simo_moanxiety_identity1[1]`)
ba_control_simo2 <- mean(ba_control_df$`simo_moanxiety_identity1[2]`)
ba_control_simo3 <- mean(ba_control_df$`simo_moanxiety_identity1[3]`)
ba_control_simo4 <- mean(ba_control_df$`simo_moanxiety_identity1[4]`)

ba_control_simo1*ba_control_moanxiety_identity_tot_chg
sum(ba_control_simo1, ba_control_simo2)*ba_control_moanxiety_identity_tot_chg
sum(ba_control_simo1, ba_control_simo2, ba_control_simo3)*ba_control_moanxiety_identity_tot_chg
sum(ba_control_simo1, ba_control_simo2, ba_control_simo3, ba_control_simo4)*ba_control_moanxiety_identity_tot_chg ==
  ba_control_moanxiety_identity_tot_chg

# Extract and plot thresholds for experiential avoidance

summary(model_ea_control)
bayes_R2(model_ea_control)
ea_control_thresholds <- fixef(model_ea_control)[paste0("Intercept[", 1:6, "]"), "Estimate"]

x <- seq(-4, 4, length=100)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "SD from M", ylab = "Density",
     main = "Thresholds of Latent Experiential Avoidance")
abline(v = ea_control_thresholds, lty = 2)

# Extract anxiety identity effect and simplexes for experiential avoidance and
# attempt to compute marginal effects of anxiety identity from simplexes

ea_control_moanxiety_identity_Estimate <- fixef(model_ea_control)["moanxiety_identity", "Estimate"]
round(ea_control_moanxiety_identity_Estimate, 4) == 0.1329

ea_control_moanxiety_identity_tot_chg <- ea_control_moanxiety_identity_Estimate*4
round(ea_control_moanxiety_identity_tot_chg, 4) == 0.5317

ea_control_df <- as.data.frame(model_ea_control)
ea_control_simo1 <- mean(ea_control_df$`simo_moanxiety_identity1[1]`)
ea_control_simo2 <- mean(ea_control_df$`simo_moanxiety_identity1[2]`)
ea_control_simo3 <- mean(ea_control_df$`simo_moanxiety_identity1[3]`)
ea_control_simo4 <- mean(ea_control_df$`simo_moanxiety_identity1[4]`)

ea_control_simo1*ea_control_moanxiety_identity_tot_chg
sum(ea_control_simo1, ea_control_simo2)*ea_control_moanxiety_identity_tot_chg
sum(ea_control_simo1, ea_control_simo2, ea_control_simo3)*ea_control_moanxiety_identity_tot_chg
sum(ea_control_simo1, ea_control_simo2, ea_control_simo3, ea_control_simo4)*ea_control_moanxiety_identity_tot_chg ==
  ea_control_moanxiety_identity_tot_chg

# ---------------------------------------------------------------------------- #
# Plot marginal effects of anxiety identity on avoidance at mean symptom severity ----
# ---------------------------------------------------------------------------- #

# Define conditions argument value to plot marginal effects at symptom severity of exactly
# 0 (which yields plots on latent avoidance scale that start exactly at 0, given our 
# fixed intercept, and that show total change corresponding exactly with our results)

cond <- data.frame(symptom_severity = 0)

# Plot predicted values on latent situational avoidance scale

  # Note: It starts at 0 (with a CI of width 0) because no intercept is estimated in 
  # ordinal models (as an intercept and thresholds are not identified at the same time; 
  # Burkner & Vuorre, 2019). In other words, the intercept is fixed at 0 (thus, there 
  # is no uncertainty around it), and because when modeling a predictor as monotonic 
  # the intercept corresponds to the first predictor category (Burkner & Charpentier, 
  # 2020), 0 on the latent avoidance scale represents the model-estimated mean avoidance
  # when anxiety identity is 1 and mean-centered anxiety symptom severity is at its 
  # mean (i.e., 0), not total absence of avoidance.

(linpred <- conditional_effects(model_ba_control, effects = "anxiety_identity", method = "posterior_linpred", 
                    robust = F, conditions = cond))

df_linpred <- linpred$anxiety_identity

# Plot predicted probabilities of each situational avoidance response category

(epred_cat <- conditional_effects(model_ba_control, effects = "anxiety_identity", method = "posterior_epred",   
                    robust = F, conditions = cond, categorical = T))

df_epred_cat <- epred_cat$anxiety_identity

# Plot expected values of situational avoidance ratings (i.e., sum of the probabilities 
# of each response category multiplied by the category numbers)

(epred <- conditional_effects(model_ba_control, effects = "anxiety_identity", method = "posterior_epred",   
                    robust = F, conditions = cond))

df_epred <- epred$anxiety_identity

  # To see that this is the case (e.g., using weighted prediction for anxiety identity of 1):

df_epred_first_estimate <- df_epred[df_epred$effect1__ == 1, "estimate__"]

df_test <- df_epred_cat[df_epred_cat$effect1__ == 1, ]
df_test$weighted_pred <- as.integer(df_test$effect2__)*df_test$estimate
round(sum(df_test$weighted_pred), 6) == round(df_epred_first_estimate, 6)

# Plot predicted values of situational avoidance ratings incorporating residual error
# (which is excluded by "posterior_epred")

  # Note: See below for resources on differences from "posterior_epred" and typical use 
  # of "posterior_epred" instead for marginal effects)
  # https://rdrr.io/cran/brms/man/posterior_epred.brmsfit.html
  # https://rdrr.io/cran/brms/man/posterior_predict.brmsfit.html
  # https://discourse.mc-stan.org/t/confusion-on-difference-between-posterior-epred-and-posterior-predict-in-a-mixed-effects-modelling-context/28813/2
  # https://discourse.mc-stan.org/t/expected-value-of-posterior-vs-posterior-of-expected-value-with-epred/28502
  # https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#which-average-is-best

(predict <- conditional_effects(model_ba_control, effects = "anxiety_identity", method = "posterior_predict", 
                    robust = F, conditions = cond))

df_predict <- predict$anxiety_identity

# Plot same for experiential avoidance

conditional_effects(model_ea_control, effects = "anxiety_identity", method = "posterior_linpred", 
                    robust = F, conditions = cond)
conditional_effects(model_ea_control, effects = "anxiety_identity", method = "posterior_epred",   
                    robust = F, conditions = cond, categorical = T)
conditional_effects(model_ea_control, effects = "anxiety_identity", method = "posterior_epred",   
                    robust = F, conditions = cond)
conditional_effects(model_ea_control, effects = "anxiety_identity", method = "posterior_predict", 
                    robust = F, conditions = cond)