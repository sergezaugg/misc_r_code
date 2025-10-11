#-----------------------------------------
# Project:   play around
# Author:    Serge Zaugg
# purpose    https://mc-stan.org/rstanarm/articles/binomial.html
#-----------------------------------------

#-----------------------------------------
# initialize session
library(ggplot2)
library(bayesplot)
library(rstanarm)
theme_set(bayesplot::theme_default())
# Show 6 significant digits
options(pillar.sigfig = 6)  


#-----------------------------------------
# prep data 
data(wells)
wells$dist100 <- wells$dist / 100

is.na(wells$arsenic) %>% sum()

wells <- wells %>% 
  tibble() %>% 
  mutate(dist_cat = cut(dist, breaks = c(0,25, 50, 100, 500))) %>% 
  mutate(arse_cat = cut(arsenic, breaks = c(0,1,2,3,100)) )

table(wells$dist_cat)
table(wells$arse_cat)

#-----------------------------------------
# plot data 

df_cru <- wells %>% 
  select(switch, dist_cat, arse_cat) %>% 
  group_by(dist_cat, arse_cat) %>% 
  summarise(prop_switch = mean(switch), strata_n = n()) %>% 
  ungroup()

ggplot(data = df_cru, aes(x = dist_cat, y = prop_switch)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0("n=", strata_n)), vjust = -0.5, size = 4) +
  facet_wrap(~ arse_cat)  

ggplot(data = df_cru, aes(x = arse_cat, y = prop_switch)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0("n=", strata_n)), vjust = -0.5, size = 4) +
  facet_wrap(~ dist_cat)  

  
# basic ugly plots   
ggplot() + 
  geom_jitter(aes(x=wells$dist , y = wells$switch), width = 0.0, height = 0.2)

ggplot(wells, aes(x = log10(dist100), y = ..density.., fill = switch == 1)) +
  geom_histogram() +
  scale_fill_manual(values = c("gray30", "skyblue"))



#-----------------------------------------
# ML  fit 
glmfit1 <- glm(switch ~ dist100, data = wells, family = binomial(link = "logit"))
summary(glmfit1)








#-----------------------------------------
# Bayesian fit 

t_prior <- student_t(df = 7, location = 0, scale = 2.5)

fit2 <- stan_glm(formula = switch ~ dist100 + arsenic, 
                 data = wells,
                 family = binomial(link = "logit"),
                 prior = t_prior, 
                 prior_intercept = t_prior,
                 cores = 2, 
                 seed = 12345
                 )

summary(fit2)
round(coef(fit2), 3)

# get posterior median from MCMC sample
posterior_data <- tibble(as.data.frame(fit2))
posterior_data %>% 
  summarise(across(where(is.numeric), median))

# get posterior median via coef
coef(fit2)

# formatted table as median + CrI
cbind(Median = coef(fit2), posterior_interval(fit2, prob = 0.95))

















