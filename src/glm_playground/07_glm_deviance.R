#----------------------
# initialize session

source("src/load_packages.R")
       
       
# ---------------------       
# create data 
n1 <- 200
x0 <- runif(n1, -10, 10)
x1 <- runif(n1, -10, 10)
resp_cont <- (2*x1) + rnorm(n1, 0.0 , 6.0)
resp_cate <- as.numeric(resp_cont > 5)
x_cat <- as.numeric(as.logical(-1*(resp_cate-1) + rbinom(n1, 1, 0.1) - rbinom(n1, 1, 0.1)))

df01 <- tibble(
  x0 = x0,
  x1 = x1,
  resp_cont = resp_cont,
  resp_cate = resp_cate,
  x_cat = x_cat
  ) %>% 
relocate(x_cat, .before = x0)  

table(df01$x_cat, df01$resp_cate)

# ---------------------       
# plot data 

plot(x1, resp_cont)

df01 %>% ggplot() +
  geom_jitter(aes(x = x0 , y = resp_cate), width = 0.0, height = 0.02)

df01 %>% ggplot() +
  geom_jitter(aes(x = x1 , y = resp_cate), width = 0.0, height = 0.02)

df01 %>% ggplot() +
  geom_jitter(aes(x = x_cat , y = resp_cate), width = 0.1, height = 0.1)



# ---------------------       
# apply glms

mod00 <- glm( resp_cate ~ 1 , data = df01, family = binomial(link = "logit"))

mod01 <- glm( resp_cate ~ 1 + x0 , data = df01, family = binomial(link = "logit"))

mod02 <- glm( resp_cate ~ 1 + x0 + x1 , data = df01, family = binomial(link = "logit"))

mod03 <- glm( resp_cate ~ 1 + x0 + x1 + x_cat , data = df01, family = binomial(link = "logit"))

summary(mod00)
summary(mod01)
summary(mod02)
summary(mod03)

anova(mod02, mod03)
anova(mod01, mod03)

anova(mod03)


summary(mod03)

df01$resp_cate


# compute deviance explicitly 

mod03 <- glm( resp_cate ~ 1 + x0 + x1 + x_cat , data = df01, family = poisson())
modSAT <- glm(resp_cate ~ factor(1:nrow(df01)) - 1, data = df01, family = poisson())

summary(mod03)

logLik(modSAT)
logLik(mod03)

2*(logLik(modSAT) - logLik(mod03))



# gamma models 

df01 <- df01 %>% 
  mutate(resp_ps = exp((resp_cont + 0.1)/10) )

modGAM <- glm( resp_ps ~ 1 + x0 + x1 + x_cat , data = df01, family = Gamma())

summary(modGAM)














       