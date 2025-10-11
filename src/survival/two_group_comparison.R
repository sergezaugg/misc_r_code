# ----------------------
# Author : Serge Zaugg
# Description : compare two groups in terms of survival / incidece
# ----------------------

library('tidyverse')
library('survival')
library('survminer')

#--------------------------------
# Define functions 

# n <- 2000
# p_event <- 0.8
# p_cens <- 0.99
# duration <- 365

make_surv_data <- function(n = 1000, p_event = 0.5, p_cens = 0.50, duration = 365, gr_a, gr_b){
  " A function to simulate time-to-event data"
  df <- tibble(
      event_true = rbinom(n, 1, p_event),
      event_date = floor(runif(n, 1, duration)),
      cens_date = floor(runif(n, 1, duration)),
      cencored_bef_eos = rbinom(n, 1, p_cens),
      group = gr_a,
      group_b = gr_b
      ) %>% 
    mutate(cens_date = if_else(cencored_bef_eos==1, cens_date, duration)) %>% 
    select(-cencored_bef_eos) %>% 
    mutate(event_date = if_else(event_true==1, event_date, NA)) %>% 
    mutate(event_obs = if_else(cens_date <= event_date & event_true == 1, 0, event_true)) %>% 
    mutate(evecen_date = if_else(cens_date <= event_date, cens_date, event_date)) %>%
    mutate(evecen_date = if_else(event_true==0 & event_obs==0, cens_date, evecen_date)) 
  return(df)
  }


#--------------------------------
# Simulate a two-group dataset 

# simulate data from two groups 
df01 <- make_surv_data(n = 500, p_event = 0.3, p_cens = 0.66, duration = 365, gr_a = "A", gr_b = "X")
df02 <- make_surv_data(n = 500, p_event = 0.6, p_cens = 0.66, duration = 365, gr_a = "B", gr_b = "X")
df03 <- make_surv_data(n = 500, p_event = 0.6, p_cens = 0.66, duration = 365, gr_a = "A", gr_b = "Y")
df04 <- make_surv_data(n = 500, p_event = 0.3, p_cens = 0.66, duration = 365, gr_a = "B", gr_b = "Y")

df10 <- bind_rows(df01, df02, df03, df04)

table(df10$group, df10$group_b)
table(df10$event_obs, df10$group)

#--------------------------------
# Create survival object 
s_obj <- Surv(time = df10$evecen_date, event = df10$event_obs, type='right')

#------------------------------------------
# log-rank test 
lrank_test <- survdiff(s_obj ~ group, data = df10)
# stratified 
lrank_test_strat <- survdiff(s_obj ~ group + strata(group_b), data = df10)

lrank_test
lrank_test_strat




#--------------------------------
# Kaplan-Meier analysis

km_mod01 <- survfit(s_obj ~ group,                   data = df10)
km_mod02 <- survfit(s_obj ~ group + strata(group_b), data = df10)
km_mod03 <- survfit(s_obj ~ group + group_b,         data = df10)

km_mod01 <- survfit(s_obj ~ 1, data = df10)

plot(km_mod01$time, km_mod01$surv, type = "l")
lines(km_mod01$time, km_mod01$surv^2, col = "green")
lines(km_mod01$time, km_mod01$surv^0.5, col = "red")

# plot 
ggsurvplot(
  fit = km_mod03,
  # fun = "event",  
  data = df10,
  conf.int = TRUE,       
  risk.table = TRUE, 
  break.time.by = 30,
  xlab = "Days",
  ylab = "Cumulative incidence",
  ggtheme = theme_minimal(),# ,
  # ylim = c(0, 1),
  # xlim = c(0, 366),
  # facet = TRUE,
  facet.by = "group_b"
  ) 




# Get KM-estimates at end of study 
estim_km <- as.tibble(summary(km_mod01, data.frame = TRUE)) %>% 
  mutate(group = substr(strata , 7, 7))  %>% 
  select(time, surv, group ) %>% 
  group_by(group) %>% 
  slice_max(time, n = 1) %>% 
  mutate(p_event = 1 - surv ) %>% 
  select(group, p_event) %>% 
  ungroup()




#------------------------------------------
# assess prop hazard assumption 

# direct
log_cumhaz <- log(-log(km_mod01$surv))
log_t      <- log(km_mod01$time)
plot(log_t, log_cumhaz, type = "s", xlab = "log(Time)")

# via ggsurvplot
ggsurvplot(
  km_mod01, data = df10,
  fun = "cloglog",       # log(-log(S(t)))
  ggtheme = theme_minimal()
)





#------------------------------------------
# cox model 

cox_model02 <- coxph(s_obj ~ group, data = df10)
summary(cox_model02)

hazard_ratio <- exp(cox_model02$coefficients)

cox_model02$xlevels
cox_model02$linear.predictors

# get H(t) - cumulative Haz
beta <- coef(cox_model02)["groupB"]

# Get baseline cumulative hazard
basehaz <- basehaz(cox_model02, centered = FALSE)
 
df_haz <- basehaz %>%
  as.tibble() %>% 
  relocate(time, .before = hazard) %>% 
  rename(cumhaz_A = hazard ) %>% 
  mutate(cumhaz_B = cumhaz_A * exp(beta)) %>% 
  mutate(hazrate_A = c(diff(cumhaz_A), NA)) %>% 
  mutate(hazrate_B = c(diff(cumhaz_B), NA)) 

df_haz %>% head()


crude_haz <- df_haz %>% 
  select(hazrate_A, hazrate_B) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

# get crude h(t)
crude_haz$hazrate_B / crude_haz$hazrate_A

# plot cumulative hazard H(t)
ggplot(data = df_haz) +
  geom_line(aes(x = time, y = cumhaz_A, color = "A")) + 
  geom_line(aes(x = time, y = cumhaz_B, color = "B")) +
  scale_color_manual(name = "Groups",values = c("A" = "blue", "B" = "red")) +
  labs(x = "Time", y = "Cumulative Hazard (H(t))") 

# plot hazard rate h(t)
ggplot(data = df_haz) +
  geom_point(aes(x = time, y = hazrate_A,   color = "A")) + 
  geom_point(aes(x = time, y = hazrate_B, color = "B")) +
  geom_smooth(aes(x = time, y = hazrate_A,   color = "A"), 
              se = FALSE, color = "blue", span = 10, method = "loess") +
  geom_smooth(aes(x = time, y = hazrate_B, color = "B"), 
              se = FALSE, color = "red", span = 10, method = "loess") +
  scale_color_manual(name = "Groups",values = c("A" = "blue", "B" = "red")) +
  labs(x = "Time", y = "Hazard") 











# --------------------------
# plot predicted survival curves 
# Survival curves for each group
surv_curves <- survfit(cox_model02, newdata = data.frame(group = unique(df10$group)))

ggsurvplot(
  fit = surv_curves,
  fun = "event",  
  data = df10,
  conf.int = TRUE,       
  risk.table = TRUE, 
  break.time.by = 30,
  xlab = "Days",
  ylab = "Model based Cumulative incidence",
  ggtheme = theme_minimal()# ,
  # ylim = c(0, 1),
  # xlim = c(0, 366),
) 




