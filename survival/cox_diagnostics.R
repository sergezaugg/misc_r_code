# ----------------------
# Author : Serge Zaugg
# Description : compare two groups in terms of survival / incidece
# ----------------------

library('tidyverse')
library('survival')
library('survminer')

#--------------------------------
# Define function
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
df03 <- make_surv_data(n = 500, p_event = 0.4, p_cens = 0.66, duration = 365, gr_a = "A", gr_b = "Y")
df04 <- make_surv_data(n = 500, p_event = 0.7, p_cens = 0.66, duration = 365, gr_a = "B", gr_b = "Y")

df10 <- bind_rows(df01, df02, df03, df04)

table(df10$group, df10$group_b)
table(df10$event_obs, df10$group)

#--------------------------------
# Create survival object 
s_obj <- Surv(time = df10$evecen_date, event = df10$event_obs, type='right')






#--------------------------------
# Kaplan-Meier analysis

km_mod01 <- survfit(s_obj ~ group,                   data = df10)
# km_mod01 <- survfit(s_obj ~ group + strata(group_b), data = df10)
# km_mod01 <- survfit(s_obj ~ group + group_b,         data = df10)

# plot 
ggsurvplot(
  fit = km_mod01,
  # fun = "event",  
  data = df10,
  conf.int = TRUE,       
  risk.table = TRUE, 
  break.time.by = 30,
  xlab = "Days",
  ylab = "Cumulative incidence",
  ggtheme = theme_minimal()# ,
  # ylim = c(0, 1),
  # xlim = c(0, 366),
  # facet = TRUE,
  # facet.by = "group_b"
) 



#------------------------------------------
# cox model 

cox_model02 <- coxph(s_obj ~ group, data = df10)
summary(cox_model02)

hazard_ratio <- exp(cox_model02$coefficients)




#------------------------------------------
# assess prop hazard assumption 


dfkm <- summary(km_mod01, data.frame = TRUE)

# direct
dfkm$log_S <- log(-log(dfkm$surv))
dfkm$log_t <- log(dfkm$time)
ggplot(data = dfkm, aes(x = log_t, y = log_S, color = strata)) +
  geom_step()


# via ggsurvplot
ggsurvplot(
  km_mod01, 
  data = df10,
  fun = "cloglog",       # log(-log(S(t)))
  ggtheme = theme_minimal()
)















