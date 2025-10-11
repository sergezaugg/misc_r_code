############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    Crate synthetic data
############################################################

#----------------------
# initialize session
source("R/load_packages.R")


#----------------------
# create data 
n1 = 400
n2 = 600
# group A and B
g1 = data.frame("group" = rep("A", times=n1), "response" = rnorm(n=n1, 2, 0.6))
g2 = data.frame("group" = rep("B", times=n2), "response" = rnorm(n=n2, 1, 0.6))
# row-combine groups
data01 <- rbind(g1,g2) %>% as_tibble() 
# add covariates 
mat <- data.frame(matrix(runif(2*(n1+n2)), nrow = n1+n2, ncol = 2))
names(mat) <- c("var01","var02")
data01 <- bind_cols(data01, mat)
# check 
head(data01)


#----------------------
# statistical analysis 

# standard additive variable encoding
mod01 <- lm(formula = response ~ var01 + var02 + group, data = data01)
summary(mod01)

# add interaction between var01 and group
mod02 <- lm(formula = response ~ var01 + var02 + group + var01*group, data = data01)
summary(mod02)

# (same as mod02) separate slope of var01 inside group 
mod04 <- lm(formula = response ~ var02 + group/var01, data = data01)
summary(mod04)

# (same as mod02 and mod04 )
mod05 <- lm(formula = response ~ 0 + var02 + group/var01, data = data01)
summary(mod05)

# residuals are exactly the same 
tibble(
  res_mod02 = mod02$residuals,
  res_mod04 = mod04$residuals,
  res_mod05 = mod05$residuals
  ) %>% slice_sample(n=10)

# we can check that AIC is exactly equal 
AIC(mod02, mod04, mod05)


# interaction between var01 var02 and group
mod03 <- lm(formula = response ~ (var01 + var02) * group, data = data01)
summary(mod03)

# (same as mod03) separate slope and intercept for both var01 and var02
mod06 <- lm(formula = response ~ 0 + group/var01 + group/var02 , data = data01)
summary(mod06)

# we can check that AIC is exactly equal 
AIC(mod03, mod06)


# compare models 
AIC(mod01, mod02, mod05, mod03)
anova(mod01, mod02, test = "LRT")
anova(mod02, mod03, test = "LRT")
anova(mod01, mod03, test = "LRT")



#----------------------
# added-variable, also called partial-regression, plots
library(car)
# ???? Partial regression (added-variable) plot
avPlots(mod05, terms = ~ 0 + group/var01,)















