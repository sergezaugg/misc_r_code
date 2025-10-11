############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    https://mc-stan.org/rstanarm/articles/rstanarm.html
############################################################

#----------------------
# initialize session
source("R/init_session/load_packages.R")
library(ggplot2)
library(bayesplot)
library(rstanarm)
library(cowplot)

theme_set(bayesplot::theme_default())


logistic <- function(x) {1/(1+exp(-x))}

logistic(-2)
logistic(0)
logistic(2)


# ------------
# load and prepare data 
data("womensrole", package = "HSAUR3")
dat <- tibble(womensrole)

dat <- dat %>% 
  mutate(total = agree + disagree) %>% 
  mutate(educ_stand = (education - mean(education))/sd(education)) %>% 
  mutate(educ_cat = as.factor(if_else(education<=10, "low", "high"))) %>% 
  mutate(prop_agree = agree / total) %>% 
  na.omit() 


# ------------
# plot data descriptively 
ggplot(data = dat) +
  geom_point(aes(x = education , y = prop_agree, color = gender )) +
  facet_grid('gender')










# ------------
# ML GLM

glm_1 <- glm(
  cbind(agree, disagree) ~ educ_stand + gender,
  data = dat, family = binomial(link = "logit"))
summary(glm_1)

# separate slope 
glm_2 <- glm(
  cbind(agree, disagree) ~ 0 + gender/educ_cat,
  data = dat, family = binomial(link = "logit"))
summary(glm_2)

# separate intercept in each groups 
glm_3 <- glm(
  cbind(agree, disagree) ~ 0 + gender:educ_cat,
  data = dat, family = binomial(link = "logit"))
summary(glm_3)

# model based proportions
data.frame(props = logistic(glm_3$coefficients))

# crude proportions
dat %>% 
  group_by(gender , educ_cat ) %>% 
  summarise(agree_sum = sum(agree), total_sum = sum(total )) %>% 
  mutate(prop = agree_sum / total_sum)

dat$educ_cat



















