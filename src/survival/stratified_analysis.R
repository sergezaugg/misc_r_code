# ----------------------
# Author : Serge Zaugg
# Description : compare two groups in terms of survival / incidece
# ----------------------

library('tidyverse')
library('survival')
library('survminer')


#--------------------------------
# Define functions 
make_survival_data <- function(n = 1000, rate = 0.5, p_cens = 0.50, duration = 365, group_name){
  " A function to simulate time-to-event data"
  df <- tibble(
    event_true = 1,
    event_date = floor( rexp(n, rate = rate)),
    cens_date = floor(runif(n, 1, duration)),
    cencored_bef_eos = rbinom(n, 1, p_cens),
    group = group_name
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

rates_exp <- c("A" = 0.01, "B" = 0.01)

# simulate data from two groups 
df01 <- make_survival_data(n = 500, rate = rates_exp[1], p_cens = 0.050, duration = 365, group_name = "A")
df02 <- make_survival_data(n = 500, rate = rates_exp[2], p_cens = 0.50, duration = 365, group_name = "B")
df10 <- bind_rows(df01, df02)


#--------------------------------
# Create survival object 
s_obj <- Surv(time = df10$evecen_date, event = df10$event_obs, type='right')

#--------------------------------
# Kaplan-Meier analysis
km_mod01 <- survfit(s_obj ~ group, data = df10)

# plot 
ggsurvplot(
  fit = km_mod01,
  fun = "event",  
  data = df10,
  conf.int = TRUE,       
  risk.table = TRUE, 
  break.time.by = 30,
  xlab = "Days",
  ylab = "Cumulative incidence",
  ggtheme = theme_minimal()#,
  # ylim = c(0, 1),
  # xlim = c(0, 366),
) 






#------------------------------------------
# cox model 

cox_model02 <- coxph(s_obj ~ group, data = df10)
summary(cox_model02)

hazard_ratio <- exp(cox_model02$coefficients)

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








