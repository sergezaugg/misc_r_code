#-----------------------------------------
# Project:   play around
# Author:    Serge Zaugg
# purpose    https://mc-stan.org/rstanarm/articles/continuous.html
#-----------------------------------------

library(ggplot2)
library(bayesplot)
library(rstanarm)

theme_set(bayesplot::theme_default())




data(kidiq)

dim(kidiq)
head(kidiq)


post1 <- stan_glm(kid_score ~ mom_hs, 
                  data = kidiq,
                  family = gaussian(link = "identity"),
                  seed = 12345)


post2 <- update(post1, formula = . ~ mom_iq)
post3 <- update(post1, formula = . ~ mom_hs + mom_iq)
post4 <- update(post1, formula = . ~ mom_hs * mom_iq)



summary(post1)


posterior_interval(post1)
posterior_interval(post2)
posterior_interval(post3)
posterior_interval(post4)


#-------------------------
# post1

# scatterplot of data 
base <- ggplot(kidiq, aes(x = mom_hs, y = kid_score)) +
  geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) +
  scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))


# posterior sample for intercept and slope 
draws <- as.data.frame(post1)
colnames(draws)[1:2] <- c("a", "b")
draws <- draws %>% slice_sample(n=500)
# head(draws)
# dim(draws) 

base +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2],
              color = "skyblue4", size = 1)




#-------------------------
# post2

draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
draws <- draws %>% slice_sample(n=250)

ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +
  geom_point(size = 1) +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2],
              color = "skyblue4", size = 1)



#-------------------------
# post3

reg0 <- function(x, ests) cbind(1, 0, x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
kidiq$clr <- factor(kidiq$mom_hs, labels = c("No HS", "HS"))
lgnd <- guide_legend(title = NULL)

base2 <- ggplot(kidiq, aes(x = mom_iq, fill = relevel(clr, ref = "HS"))) +
  geom_point(aes(y = kid_score), shape = 21, stroke = .2, size = 3) +
  guides(color = lgnd, fill = lgnd) +
  theme(legend.position = "right")

base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)

#-------------------------
# post4

reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))

base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)




#-------------------------
# posterior predictive 

IQ_SEQ <- seq(from = 75, to = 135, by = 5)

y_nohs <- posterior_predict(post4, newdata = data.frame(mom_hs = 0, mom_iq = IQ_SEQ))

y_hs <- posterior_predict(post4, newdata = data.frame(mom_hs = 1, mom_iq = IQ_SEQ))


dim(y_nohs)
dim(y_hs)

par(mfrow = c(1:2), mar = c(5,4,2,1))
boxplot(y_hs, axes = FALSE, outline = FALSE, ylim = c(10,170),
        xlab = "Mom IQ", ylab = "Predicted Kid IQ", main = "Mom HS")
axis(1, at = 1:ncol(y_hs), labels = IQ_SEQ, las = 3)
axis(2, las = 1)

boxplot(y_nohs, outline = FALSE, col = "red", axes = FALSE, ylim = c(10,170),
        xlab = "Mom IQ", ylab = NULL, main = "Mom No HS")
axis(1, at = 1:ncol(y_hs), labels = IQ_SEQ, las = 3)












#-------------------------
# compare conventional "interaction" parametrization with 'separate slope'

head(kidiq)
kidiq <- kidiq %>%
  mutate(mom_hs_f = factor(mom_hs))

#----
# Frequentist

# interaction model and separate slope model 
glm01 <- glm(kid_score ~     mom_hs_f*mom_iq, data = kidiq, family = gaussian(link = "identity"),)
glm02 <- glm(kid_score ~ 0 + mom_hs_f/mom_iq, data = kidiq, family = gaussian(link = "identity"),)
coef01 <- coefficients(glm01)
coef02 <- coefficients(glm02)

# reconstruct separate slope coefs from interaction model
c(coef01[1], coef01[1] + coef01[2], coef01[3], coef01[3] + coef01[4])
# compare 
coef02

#----
# Bayesian
# interction model
model01 <- stan_glm(kid_score ~     mom_hs_f*mom_iq, data = kidiq, family = gaussian(link = "identity"))
# separate slope model 
model02 <- stan_glm(kid_score ~ 0 + mom_hs_f/mom_iq, data = kidiq, family = gaussian(link = "identity"))

# get posterior of the betas
posterior_interval(model01)
posterior_interval(model02)

# renname columns
psam01 <- tibble(data.frame(model01))
names(psam01) <- c("icpt", "mom_hs", "mom_iq", "interaction", "sigma" )
psam02 <- tibble(data.frame(model02))
names(psam02) <- c("hs0", "hs1", "mom_iq0", "mom_iq1", "sigma")

# compute separate slop coefs from interaction models
psam01b <- psam01 %>% 
  mutate(hs0 = icpt  ) %>% 
  mutate(hs1 = icpt + mom_hs) %>% 
  mutate(mom_iq0 = mom_iq) %>% 
  mutate(mom_iq1 = mom_iq +interaction) %>% 
  select(-c(icpt, mom_hs, mom_iq, interaction)) %>% 
  relocate(sigma, .after = mom_iq1) %>% 
  mutate(source = "interaction")

# re-organize dfs for the plots   
psam02 <- psam02 %>% 
  mutate(source = "sep_slope")
psam <- bind_rows(psam01b, psam02) %>% 
  mutate(seq = seq(n()))

# graphically compare coefficients 
ggplot() +
  geom_point(data = transform(psam, panel = "hs0"), aes(x = seq, y = hs0, color = source)) +
  geom_point(data = transform(psam, panel = "hs1"), aes(x = seq, y = hs1, color = source)) +
  facet_wrap(~panel)

ggplot() +
  geom_point(data = transform(psam, panel = "mom_iq0"), aes(x = seq, y = mom_iq0, color = source)) +
  geom_point(data = transform(psam, panel = "mom_iq1"), aes(x = seq, y = mom_iq1, color = source)) +
  facet_wrap(~panel)

# so yes, the posterior sample can be simply added to obtain sep slope 


