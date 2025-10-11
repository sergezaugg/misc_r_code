############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    Create synthetic data
############################################################

#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")

data01 = read_csv(file.path(path_data_root, "data_synth_01.csv"))
data02 = read_csv(file.path(path_data_root, "data_synth_02.csv"))
# head(data01)
# head(data02)

# join 
df01 <- data01 %>% left_join(data02, by = c("PID" = "pid"))
# dim(df01)
# head(df01)
# names(df01) 

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

# head(df02)
# names(df02)

# save with list including time stamp
formatted_ts <- format(now(tzone = "UTC"), "%Y%m%d_%H%M%S")
df_li <- list("ts" = formatted_ts, "data" = df02)
path_data_file <- file.path(path_data_root, "data_prepared.rds")
write_rds(x = df_li, file = path_data_file, compress = "gz" )






