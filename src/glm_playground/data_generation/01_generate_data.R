############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    Crate synthetic data
############################################################

#----------------------
# initialize session
source("R/init_session/load_paths.R")
source("R/init_session/load_packages.R")
# create dir for temp data 
if (! dir.exists(path_data_root)) {
  dir.create(path_data_root)
  }


#----------------------
# create data 
n1 = 400
n2 = 600

# group A and B
g1 = data.frame(
  "PID" = cut(runif(n1), breaks=10, labels=FALSE),
  "group" = rep("A", times=n1), 
  "response" = rnorm(n=n1, 2, 0.6),
  "event_death" = rbinom(n=n1, size=1, prob=0.25)
  )

pid_max_g1 <- max(g1$PID)

g2 = data.frame(
  "PID" = cut(runif(n2), breaks=10, labels=FALSE) + pid_max_g1,
  "group" = rep("B", times=n2), 
  "response" = rnorm(n=n2, 1, 0.6),
  "event_death" = rbinom(n=n2, size=1, prob=0.10)
  )

# row-combine groups
df01 <- rbind(g1,g2) %>% 
  as_tibble() %>% 
  arrange(group, PID )
# head(df01)

# check 
df01$PID %>% table()
table(df01$group, df01$event_death)
prop.table(table(df01$group, df01$event_death), margin=1)

# add two uniform covariates 
mat <- data.frame(matrix(runif(2*(n1+n2)), nrow = n1+n2, ncol = 2))
names(mat) <- c("var01","var02")
head(mat)
dim(mat)
df02 = bind_cols(df01, mat)
# add interaction
df02 <- df02 %>%
  mutate(response = if_else(group == "A", response+0.4*var01, response))
# add effect 
df02 <- df02 %>% mutate(response = response-0.7*var02)

# add a binary response and a censoring variable 
data_sta <- ymd("2020-01-01")
date_end <- ymd("2020-12-31")
date_recruit <- sample(seq(data_sta, date_end, by = "day"), size = n1+n2, replace = TRUE)
date_cen_dea <- date_recruit + days(sample(1:365, size = n1+n2, replace = TRUE))

# sum(date_cen_dea <= date_recruit)
df02 <- df02 %>% 
  mutate(
    date_recruit = date_recruit,
    date_cen_dea = date_cen_dea
    ) %>% 
  relocate(event_death, .after = last_col())

# add a binary response dependent on var01 and var02
df02 <- df02 %>% 
  mutate(event_accute = as.integer(response >=1.8))

#----------------------
# make patient level data 
pid_var <- as.data.frame(df02 %>% distinct(PID))$PID
n <- length(pid_var)
dfp <- tibble(
    pid = pid_var,
    age_years = round(runif(n, 18,120),0), 
    sex_female = as.numeric(runif(n) > 0.5)
    )

#----------------------    
# join 
df01 <- df02 %>% left_join(dfp, by = c("PID" = "pid"))
# Rename variable etc.
df02 <- df01 %>% 
  # handle serum lab values
  rename(
    chol_ldl_mgperl = var01,
    chol_hdl_mgperl = var02
  ) %>% 
  mutate(
    chol_ldl_mmol = chol_ldl_mgperl / 0.12365,
    chol_hdl_mmol = chol_hdl_mgperl / 0.12365,
  ) %>% 
  select(-contains("_mgperl")) %>% 
  # handle the grouping variables
  mutate(group  = as_factor(group )) %>% 
  mutate(group = fct_relevel(group, "A", "B")) %>% 
  mutate(PID  = as.integer(PID)) %>% 
  # handle the survival variables 
  mutate(time_to_event_d = date_cen_dea - date_recruit ) %>%  
  relocate(date_recruit, date_cen_dea, event_death, event_accute, time_to_event_d, .after = group) 

#----------------------
# save with list including time stamp
formatted_ts <- format(now(tzone = "UTC"), "%Y%m%d_%H%M%S")
df_li <- list("ts" = formatted_ts, "data" = df02)
path_data_file <- file.path(path_data_root, "data_prepared.rds")
write_rds(x = df_li, file = path_data_file, compress = "gz" )









