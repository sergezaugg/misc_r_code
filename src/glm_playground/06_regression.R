#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")

#----------------------------
# get data 

df01 <- airquality %>% 
  tibble() %>% 
  mutate(across(Solar.R:Day, ~ as.numeric(scale(.x)) )) %>% 
  drop_na() 

head(df01)
dim(df01)



#----------------------------
# crude plots

df01 %>% 
  pivot_longer(cols = Solar.R:Day, names_to = "variable", values_to = "value", ) %>% 
  drop_na() %>% 
  ggplot() +
    geom_point(aes(x = value, y = Ozone)) +
    facet_grid(rows = 'variable', scales = "free")



#----------------------------
# base model

mod01 <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day, data = df01) 
summary(mod01)

plot(mod01)
vcov(mod01)


#--------------------
# check if betas are correlated 
corr <- round(cov2cor(vcov(mod01)),2)
# install.packages("corrplot")
library(corrplot)
corrplot(corr, method = "color", type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.cex = 0.8, tl.col = "black", addCoef.col = "black")


#--------------------
# predict 
df_shu <- df01 %>%
  mutate(across(everything(), ~ sample(.x))) %>% 
  select(-Ozone ) %>% 
  drop_na() 

predict(mod01, newdata = df_shu, interval = "prediction")




#--------------------
# variable selection 

mod01 <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day, data = df01) 
summary(mod01)

mod02 <- lm(Ozone ~ (Solar.R + Wind + Temp + Month + Day)^2, data = df01) 
summary(mod02)

mod03 <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day + Wind:Temp, data = df01) 
summary(mod03)

anova(mod01, mod03)
AIC(mod01, mod03)

mod04 <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Wind:Temp, data = df01) 
summary(mod04)

anova(mod04, mod03)
AIC(mod04, mod03)

# stick with model 3 
summary(mod03)
plot(mod03)


#--------------------
# reproduce per-variable t-test by removing the variable an making LR-test with anova()
mod03full <- lm(Ozone ~ Solar.R + Wind + Temp + Month + Day + Wind:Temp, data = df01) 
mod03redu <- lm(Ozone ~ Wind + Temp + Month + Day + Wind:Temp, data = df01) 
summary(mod03full)
anova(mod03redu, mod03full)









#--------------------
# do it Bayesian

# install.packages("rstanarm")
library(rstanarm)

mod03redu <- lm(Ozone ~ Wind + Temp + Month + Day + Wind:Temp, data = df01)

summary(mod03redu)

mod03bay <- stan_glm(Ozone ~ Wind + Temp + Month + Day + Wind:Temp, 
                data = df01,
                prior = normal(0, 2.5),           # prior for coefficients
                prior_intercept = normal(20, 5),  # prior for intercept
                prior_aux = exponential(1))       # prior for sigma

print(mod03bay, digits = 2)      # posterior summaries
summary(mod03bay)                # detailed output
plot(mod03bay)                   # posterior intervals

# posterior_predict(mod03bay)      # posterior predictive draws
# posterior_interval(mod03bay)     # credible intervals

posterior_interval(mod03bay) 

confint(mod03redu)





























