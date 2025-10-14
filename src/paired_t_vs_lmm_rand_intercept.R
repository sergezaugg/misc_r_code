

library(lme4)


N = 25

x1 = rnorm(N, 0, 1)
x2 = rnorm(N, 1, 1)

df <- data.frame(
  value = c(x1, x2),
  condition = c(rep(0,N),rep(1,N)) ,
  subject = factor(c(seq(1:N), seq(1:N)))
  )


# paired ttest 
mean(x2-x1)
t.test(x2, x1, paired = TRUE)

# lm
mod_lm <- lm(value ~ condition + subject, data = df)
mod_lm$coefficients['condition']
confint(mod_lm)['condition',]

# lmm
model <- lmer(value ~ condition + (1 | subject), data = df)
# summary(model)
fixef(model)['condition']
confint(model, method = "Wald")['condition',]


