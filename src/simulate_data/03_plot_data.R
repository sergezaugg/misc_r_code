############################################################
# Project:   play around
# Author:    Serge Zaugg
# purpose    xxxxxx
############################################################

#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")

# load
data01 <- read_rds(file = file.path(path_data_root, "data_prepared.rds"))$data
# dim(data01)
# head(data01)
# names(data01) 


#------------------------------------
# aggregate and then mutate (will keep nrows same)

data01 <- data01 %>% 
  group_by(PID) %>% 
  mutate(agg_var01 = mean(chol_ldl_mmol)) %>% 
  mutate(resid = chol_ldl_mmol - mean(chol_ldl_mmol)) %>% 
  ungroup() %>% 
  mutate(idx = seq_len(n()))

data01$idx

ggplot(data = data01) +
  geom_point(aes(x = idx, y = agg_var01, color = group)) + # plot per group mean
  geom_point(aes(x = idx, y = chol_ldl_mmol), color = "yellow") + # plot original data 
  geom_point(aes(x = age_years, y = response  ), color = "blue") # over-plot another variable pair 



#------------------------------------
# call ggplot within a function and save multiple plots with lapply 

make_scatter <- function(x) {
  ggplot(data = data01, aes(x = !!sym(x), y = .data[["response"]], color = .data[["group"]])) +
  geom_point() +
  scale_color_manual(values = c("A" = "red", "B" = "blue"))
  }

# generate plot objects
p01 <- lapply(c("chol_ldl_mmol", "chol_hdl_mmol", "date_recruit", "time_to_event_d"), make_scatter)
p01

# save plots as png
lapply(p01, 
  function(s){
    var_name <- s$labels$x
    ggsave(file.path(path_resu_root, paste("plot_", var_name, ".png", sep = "")), 
      plot = s, width = 6, height = 4, dpi = 300)}
  )



#-----------------------------------
# simple ggplots 

ggplot(data = data01, aes(x = chol_ldl_mmol, y = response, color = group)) +
  geom_point() +
  labs(title = "Scatterplot by Group") +
  theme_minimal()

ggplot(data = data01, aes(x = chol_hdl_mmol, y = response)) +
  geom_point(color = "pink") 



#------------------------------------
# plot two columns in same plot with two facets -> reshape trick!

data01sel <- data01 %>% 
  select( PID, group, chol_ldl_mmol, chol_hdl_mmol, response)

# Reshape to long format
df_long <- data01sel %>% 
  pivot_longer(cols = c("chol_ldl_mmol", "chol_hdl_mmol"), names_to = "chol_group", values_to = "chol_value")

# dim(data01sel)
# dim(df_long)

fig01 <- ggplot(data = df_long, 
  mapping = aes(x = chol_group, y = response, color = group)) +
  geom_jitter(width = 0.1, height = 0.0) +
  labs(title = "Scatterplot by Group") +
  facet_wrap(~chol_group, scales = "free_x") + 
  theme_minimal()

# Save as PNG
ggsave(file.path(path_resu_root, "plot.png") , plot = fig01, width = 6, height = 4, dpi = 300)
# Save as PDF
ggsave(file.path(path_resu_root, "plot.pdf"), plot = fig01, width = 6, height = 4)



#------------------------------------
# Convert to interactive plotly

interactive_p <- ggplotly(fig01)
interactive_p








