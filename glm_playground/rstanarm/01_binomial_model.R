#-----------------------------------------
# Project:   play around
# Author:    Serge Zaugg
# purpose    https://mc-stan.org/rstanarm/articles/rstanarm.html
#-----------------------------------------

#----------------------
# initialize session
source("R/init_session/load_packages.R")
library(ggplot2)
library(bayesplot)
library(rstanarm)
library(cowplot)
theme_set(bayesplot::theme_default())
options(pillar.sigfig = 6)  


# ------------
# load and prepare data 
data("womensrole", package = "HSAUR3")
dat <- tibble(womensrole) 
  
# # take smaller random sample of data
# dat <- dat %>%
#   dplyr::slice_sample(n=4)
red_factor <- 4
dat <- dat %>% 
  mutate(agree = round(agree / red_factor), disagree = round(disagree/red_factor))

dat <- dat %>%   
  mutate(total = agree + disagree)




# ------------
# plot data descriptively 
df <- dat %>% 
  as_tibble() %>% 
  mutate(prop_agree = agree / total) # crude proportion of agree

ggplot(data = df) +
  geom_point(aes(x = education , y = prop_agree, color = gender )) +
  facet_grid('gender')   + 
  scale_x_continuous(breaks = seq(0, 20, by = 1.0)) +  
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.5)) +    
  theme(panel.grid.major = element_line(color = "grey80", size = 0.5),  # Grey major gridlines
    panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  

# ------------
# ML GLM
glm_1 <- glm(
  cbind(agree, disagree) ~ education + gender,
  data = dat, family = binomial(link = "logit"))

coef(summary(glm_1))
confint(glm_1)


# ------------
# define prior
prior_beta_zero <- student_t(df = 7, 0, 5)
prior_betas     <- normal(location = c(0.5,-0.5), scale = c(0.3,0.3)) # strongly informative
# prior_betas <- normal(location = 0, scale = 10) # weakly informative


# ------------
# scrutinize priors Bayesian GLM

# sample from priors with "prior_PD = TRUE"
bglm_01_pri <- stan_glm(
  cbind(agree, disagree) ~ education + gender,
  data = dat,
  family = binomial(link = "logit"),
  prior = prior_betas,
  prior_intercept = prior_beta_zero,
  cores = 2, 
  seed = 12345,
  prior_PD = TRUE 
  )

# plot 'posterior' of the betas (actually priors because prior_PD = TRUE )
prior_df <- as.data.frame(bglm_01_pri)
head(prior_df)
pri_df_long <- prior_df %>% 
  pivot_longer(cols = everything() , names_to = "coef", values_to = "value") %>% 
  filter(coef != "(Intercept)")

# analytic plot of prior summaries
pri_suma <- prior_summary(bglm_01_pri) 
xx <- seq(-10, 10, 0.1)
fx_1 <- dnorm(x = xx, mean = pri_suma$prior$location[1], sd = pri_suma$prior$scale[1])
fx_2 <- dnorm(x = xx, mean = pri_suma$prior$location[2], sd = pri_suma$prior$scale[2])
df_pri <- data.frame(xx, fx_1, fx_2)



# ----------------------
# estimate with data 
bglm_01 <- stan_glm(
  cbind(agree, disagree) ~ education + gender,
  data = dat,
  family = binomial(link = "logit"),
  prior = prior_betas,
  prior_intercept = prior_beta_zero,
  cores = 2, 
  seed = 12345,
  prior_PD = FALSE
  )

# convergence diagnostics 
# launch_shinystan(bglm_01, ppd = FALSE)

# posterior summaries for the parameters (betas)
summary(bglm_01)
crIs <- posterior_interval(bglm_01, prob = 0.95)
cbind(Median = coef(bglm_01), crIs)
summary(residuals(bglm_01)) 
cov2cor(vcov(bglm_01))

# plot posterior of the betas
posterior_df <- as.data.frame(bglm_01)
head(posterior_df)
post_df_long <- posterior_df %>% 
  pivot_longer(cols = everything() , names_to = "coef", values_to = "value") %>% 
  filter(coef != "(Intercept)")

# fig_post <- ggplot() +                       
#   geom_histogram(data = post_df_long , aes(x = value, y = after_stat(density), fill = coef), 
#                  position = "identity", alpha = 0.5, bins = 201)





# ----------------------
# plot prior and posterior

pri_gen <- hist(x = prior_df$genderFemale, breaks = 20, plot = FALSE)
pri_edu <- hist(x = prior_df$education, breaks = 20, plot = FALSE)

pos_gen <- hist(x = posterior_df$genderFemale, breaks = 35, plot = FALSE)
pos_edu <- hist(x = posterior_df$education, breaks = 35, plot = FALSE)

xlim_all <- c(-1,1)

fig01 <- ggplot() + 
  geom_line(aes(x = pri_edu$mids, y = pri_edu$density), color= "red") +
  geom_line(aes(x = pri_gen$mids, y = pri_gen$density), color= "green") +
  geom_area(data = df_pri, aes(x = xx, y = fx_1), color= "red", fill = "red", alpha = 0.3) + 
  geom_area(data = df_pri, aes(x = xx, y = fx_2), color= "green", fill = "green", alpha = 0.3) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  xlim(xlim_all) +
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = 0, color = "grey")

fig02 <- ggplot() + 
  geom_area(aes(x = pos_edu$mids, y = pos_edu$density), color= "red", fill = "red", alpha = 0.3) +
  geom_area(aes(x = pos_gen$mids, y = pos_gen$density), color= "green", fill = "green", alpha = 0.3) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) + 
  xlim(xlim_all) +
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = 0, color = "grey")

# Combine plots 
combined_plot <- plot_grid(fig01, fig02, nrow = 2,ncol = 1)
combined_plot











#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------

# diagnostics with posterior predictive distribution



# ------------
# Draw from posterior predictive distribution with training data 
# i.e. use same binomial n as in training data 
dim(dat)
head(dat)
dat$education %>% range()

# Draw from posterior predictive
y_rep <- posterior_predict(bglm_01, newdata = dat, draws = 1000)
dim(t(y_rep))
dim(dat)

# prepare data for plot 
df_pred <- cbind(dat, t(y_rep)) %>% 
  tibble() %>% 
  na.omit() %>% 
  # divide binomial agree-counts by total
  mutate(across(-c(education, gender, agree, disagree, total), ~ .x/total)) %>% 
  select(-agree, -disagree, -total) %>% 
  pivot_longer(cols = -c(education, gender), names_to = "replicate", values_to = "prop") 
  
# plot box-plot of posterior predictive per group 
ggplot() +
  geom_boxplot(aes(group = education, x = education, y = prop), data = df_pred) +
  # over-plot the crude proportion 
  geom_point(aes(x = education , y = prop_agree, color = gender ), data = df) +
  facet_grid('gender') 








# ------------
# Draw predictive distribution from regular grid with fixed binomial n pr group 
n_binom <- 100

df_new = data.frame(
  agree = n_binom,
  disagree = 0,
  education = c(seq(0:19),seq(0:19)), 
  gender = c(rep("Male", 20),rep("Female", 20))
  )

y_rep <- posterior_predict(bglm_01, newdata = df_new, draws = 100)
dim(df_new)
dim(t(y_rep))

# prepare for plot 
df_pred_b <- cbind(df_new, t(y_rep)) %>% 
  select(-agree, -disagree) %>% 
  pivot_longer(cols = -c(education, gender), names_to = "replicate", values_to = "count",) %>% 
  mutate(prop_agree_sim = count / n_binom)

# plot 
ggplot(data = df_pred_b) +
  geom_jitter(aes(x = education , y = prop_agree_sim), width = 0.25, height = 0.0) +
  # over-plot the crude proportion 
  geom_point(aes(x = education , y = prop_agree, color = gender ), data = df,
             shape = 17, size = 4, color = "darkgreen") +
    facet_grid('gender') 


geom_point(shape = 17, size = 4, color = "darkgreen")











