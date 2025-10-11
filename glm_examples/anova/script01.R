#---------------------------
# Author: Serge Zaugg 
# Purpose: simulate known data and analyse with anova and lm
#
#---------------------------

#---------------------------
# initialize
library('tidyverse')
library(lme4)

#---------------------------
# generate two-way anova data 

# Define the factor levels
f1 <- factor(c("A", "B", "C", "D", "E", "F", "G", "H"))
f2 <- factor(c("X", "Y", "Z", "Xb", "Yb", "Zb"))
n_rep <- 50




# simulate data from crossed design with replicates, no interaction
df0 <- expand.grid(pred01 = f1, pred02 = f2, rep = seq_len(n_rep))

df <- tibble(df0) %>% 
  select(-rep) %>% 
  arrange(pred01, pred02) %>% 
  group_by(pred01) %>% 
  mutate(effect01 = rnorm(n=1, mean=0.0, sd = 1.5)) %>% 
  ungroup() %>% 
  group_by(pred02) %>% 
  mutate(effect02 = rnorm(n=1, mean=0.0, sd = 1.0)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(measur_err = rnorm(n=1, mean=0.0, sd = 0.30)) %>% 
  ungroup() %>% 
  mutate(resp = effect01 + effect02 + measur_err)

# df %>% print(n=30)
df

#---------------------------
# plot
plot(df$resp)
var(df$resp)
sd(df$resp)

ggplot(data = df) +
  geom_jitter(aes(x = pred01, y = resp), height = 0.0, width = 0.1, size = 1) +
  facet_grid(rows = vars(pred02 )) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") 


#---------------------------
# make classic ANOVA 
mod_aov <- aov(
  formula = resp ~ pred01 + pred02,
  data = df)

# Convert the summary output to a data frame for easier extraction
anova_table <- summary(mod_aov)
anova_df <- as.data.frame(anova_table[[1]])

# get total SS from ANOVA output 
sum(anova_df[,2]) 
# get total SS directly from data
sum((  (df$resp-mean(df$resp))^2))
# crude estimates of variance components 
anova_df[,'Sum Sq'] /  nrow(df) 



# 
# # get variance components - correct method 
# MSA <- anova_df[1, 'Mean Sq']
# MSB <- anova_df[2, 'Mean Sq']
# MSE <- anova_df[3, 'Mean Sq']
# 
# a <- length(levels(df$pred01))
# b <- length(levels(df$pred02))
# n <- nrow(df) / (a*b)   # number of replicates per cell (assumes balanced data)
# 
# sigma2_e <- MSE
# sigma2_A <- ( a*(MSA - MSE) - (MSB - MSE) ) / ( n*(a*b - 1) )
# sigma2_B <- ( b*(MSB - MSE) - (MSA - MSE) ) / ( n*(a*b - 1) )
# 
# sqrt(sigma2_A)
# sqrt(sigma2_B)
# sqrt(sigma2_e)









#---------------------------------------------
# get same result with lm()
mod_lmfull <- lm(resp ~ pred01 + pred02, data = df)
mod_lm01 <- lm(resp ~ pred01, data = df)
mod_lm02 <- lm(resp ~ pred02, data = df)

# ANOVA output
summary(mod_aov)
# improvement if pred02 added 
anova(mod_lm01, mod_lmfull)
# improvement if pred01 added 
anova(mod_lm02, mod_lmfull)



#---------------------------------------------
# get same result with lme4

# Random effects for A, B
mod_lmer <- lmer(resp ~ 1 + (1|pred01) + (1|pred02) , data=df, REML=TRUE)

summary(mod_lmer)
VarCorr(mod_lmer)










#---------------------------------------------
# get same result with rtsan
















