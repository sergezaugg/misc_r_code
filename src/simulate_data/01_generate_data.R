############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    Crate synthetic data
# Rscript src/script01.R
############################################################

#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")

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
df01 = rbind(g1,g2)
head(df01)
dim(df01)
summary(df01)

df01 <- as_tibble(df01)

df01 <- df01 %>% arrange(group, PID )

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

# check
head(df02)
dim(df02)


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

# df02 %>% head()
# df02$response
# ggplot(data = df02) +
#   geom_point(aes(x = var01, y = response, color = group)) 
# ggplot(data = df02) +
#   geom_point(aes(x = var02, y = response, color = group)) 

df02 <- df02 %>% 
  mutate(event_accute = as.integer(response >=1.8))

# table(df02$event_accute, df02$group)

# save data to disc
path_data_file <- file.path(path_data_root, "data_synth_01.csv")
write.csv(df02, path_data_file, row.names = FALSE)
print("Done")
write_csv(df02, path_data_file)







#----------------------
# make patient level data 

pid_var <- as.data.frame(df02 %>% distinct(PID))
pid_var <- pid_var$PID
n <- length(pid_var)

dfp <- tibble(
    pid = pid_var,
    age_years = round(runif(n, 18,120),0), 
    sex_female = as.numeric(runif(n) > 0.5)
    )
    
path_data_file <- file.path(path_data_root, "data_synth_02.csv")
write.csv(dfp, path_data_file, row.names = FALSE)






