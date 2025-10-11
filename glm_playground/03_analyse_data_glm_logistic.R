#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")

# load data 
path_data_file <- file.path(path_data_root, "data_prepared.rds")
dat <- read_rds(path_data_file)$data
dat %>%  names()

dat <- dat %>% 
  mutate(sex_female_fct = as.factor(sex_female))

dat$sex_female_fct

mod01 <- glm(event_death  ~ 1  ,  data = dat, family = binomial(link = "logit"))
mod02 <- glm(event_death  ~ sex_female_fct - 1 ,  data = dat, family = binomial(link = "logit"))
mod03 <- glm(event_death  ~ sex_female_fct,  data = dat, family = binomial(link = "logit"))
mod04 <- glm(event_death  ~ (group + sex_female_fct + response)^4,  data = dat, family = binomial(link = "logit"))

summary(mod01)
summary(mod02)
summary(mod03)
summary(mod04)




logistic <- function(x) {1 / (1 + exp(-x))}

# back-compute coefs from model 
logistic(mod01$coefficients)
# crude estimate 
sum(dat$event_death)/nrow(dat)



# glm with grouped data 

dattab <- dat %>% 
  select(event_death, sex_female_fct) %>% 
  group_by(sex_female_fct) %>% 
  summarise(event_death = sum(event_death), event_alive = n() - sum(event_death), ntot = n())

mod01b <- glm(cbind(event_death, event_alive) ~ 1,                  data = dattab, family = binomial(link = "logit"))
mod02b <- glm(cbind(event_death, event_alive) ~ sex_female_fct - 1, data = dattab, family = binomial(link = "logit"))
mod03b <- glm(cbind(event_death, event_alive) ~ sex_female_fct,     data = dattab, family = binomial(link = "logit"))

summary(mod01)
summary(mod01b)

summary(mod02)
summary(mod02b)

summary(mod03)
summary(mod03b)


anova(mod03, test="Chisq")
anova(mod03b, test="Chisq")




mod00 <- glm(event_death  ~ 1,  data = dat, family = binomial(link = "logit"))
mod10 <- glm(event_death  ~ sex_female_fct + group + response,  data = dat, family = binomial(link = "logit"))
mod11 <- glm(event_death  ~ group + sex_female_fct + response,  data = dat, family = binomial(link = "logit"))
mod12 <- glm(event_death  ~ sex_female_fct + group,             data = dat, family = binomial(link = "logit"))


# try with non-nested models -> does not trigger warning !!!!!!!
modx1 <- glm(event_death  ~ response-1 ,  data = dat, family = binomial(link = "logit"))
modx2 <- glm(event_death  ~ group ,  data = dat, family = binomial(link = "logit"))
anova(modx1, modx2)

summary(mod11)
summary(mod00)

anova(mod10)
anova(mod11)

anova(mod00, mod11)
anova(mod12, mod10)
anova(mod10, mod12)
anova(mod10, mod12)







# profiling 
confint(mod12)
# standard 'Wald'
confint.default(mod12)



#-------------------------------------------
# install.packages('effects')
library(effects)
all.effects <- allEffects(mod = mod10)
summary(all.effects)
plot(all.effects, type = "response", ylim = c(0, 1))











