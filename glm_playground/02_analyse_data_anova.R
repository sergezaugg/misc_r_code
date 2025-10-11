#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")
library(nlme)


#----------------------
# create data 
n1 = 100
n2 = 20
n3 = 25
df01 <- bind_rows(
  tibble(
    "group" = rep("A", times=n1), 
    "cont_response" = rnorm(n=n1, 2, 0.6),
    "event_death" = rbinom(n=n1, size=1, prob=0.05)),
  tibble(
    "group" = rep("B", times=n1), 
    "cont_response" = rnorm(n=n1, 2.5, 1.2),
    "event_death" = rbinom(n=n1, size=1, prob=0.10)),
  tibble(
    "group" = rep("C", times=n2), 
    "cont_response" = rnorm(n=n2, 2.0, 0.6),
    "event_death" = rbinom(n=n2, size=1, prob=0.20)),
  tibble(
    "group" = rep("D", times=n3), 
    "cont_response" = rnorm(n=n3, 2.5, 1.6),
    "event_death" = rbinom(n=n3, size=1, prob=0.35))
  )

table(df01$event_death, df01$group)


#-------------------------------------
# (1) one way ANOVA (assumes homoscedasticity)
mod01 <- aov(cont_response ~ group, data = df01)

# defaults diagnostic plots 
# plot(mod01)

# plot residuals diagnostic 
tibble(group = df01$group, residual = residuals(mod01)) %>% 
  ggplot() +  geom_jitter(aes(x=group , y = residual), width = 0.1, height=0.0)
  
# pairwise -tests, only for aov()
TukeyHSD(mod01)


#--------------------------------------
# (2) same with lm (gls to account for unequal variance )

# handle un-equal variance
mod01 <- gls(cont_response ~ group - 1, data = df01,
           weights = varIdent(form = ~1 | group))  # different variance per group

summary(mod01)

goup_ss <- table(df01$group)

# extract mean + CIs + sample size 
df_for <- tibble(
  group = factor(names(mod01$coefficients)),
  mean  = mod01$coefficients,
  ci_lo = confint(mod01)[,1],
  ci_up = confint(mod01)[,2]
  ) %>% 
  mutate(label_text = sprintf("%.2f (%.2f to %.2f)", mean, ci_lo, ci_up)) %>% 
  mutate(group_nicef = fct_recode(group,
     "A" = "groupA", "B" = "groupB", "C" = "groupC", "D" = "groupD")) %>% 
  mutate(group_ss_name = names(goup_ss), group_ss = goup_ss) %>% 
  mutate(n_text = sprintf("n=%.0f", group_ss))  %>% 
  # to make sure A plotted on top and D at bottom
  mutate(group_nicef = fct_relevel(group_nicef , "D", "C", "B", "A"))


# make a forest plot 
fig01 <- ggplot(df_for, aes(x = mean, y = group_nicef)) +
  geom_point(shape = 15, size = 4) +   # square for mean
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_up), width = 0.2) +
  geom_vline(xintercept = 2.0, color = "darkgreen") +
  geom_vline(xintercept = c(1.0,3.0), color = "magenta", linetype = "dotted", size = 1.0) +
  xlim(-1.0, 4.3) +
  # geom_text(aes(label = label_text, hjust = 1.5, vjust = 0.0)) + # text beside point
  geom_text(aes(label = label_text), x = 0, hjust = 0) +  
  geom_text(aes(label = n_text),     x = 3.7, hjust = 0) +  
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
    panel.background = element_blank(),   # remove gray panel
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 5.0), # horizontal major
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    ) +  
  labs(x = "Model-based mean", y = "Groups", 
       title = "Plot of synthetic data - to warm-up ggplot skills") +
  theme(plot.margin = margin(t = 40, r = 30, b = 20, l = 100))  # top, right, bottom, left

fig01

ggsave(filename = file.path(path_resu_root, "forest_plot01.png"), fig01,
       width = 30, height = 20, units = 'cm')


#-----------------------------------
### Kruskal-wallis approach

kw_test <- kruskal.test(cont_response ~ group, data = df01)
kw_test$statistic
kw_test$p.value

# make pairwise tests with multiple test correction
mat <- pairwise.wilcox.test(df01$cont_response, df01$group, p.adjust.method = "holm")
mat <- mat$p.value

df_pvals <- as.data.frame(mat) %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "p_value") %>%
  drop_na() %>%
  mutate(pair = paste(Var1, Var2, sep = " vs ")) %>%
  select(pair, p_value) %>% 
  arrange(p_value) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# make a plot 
fig02a <- df01 %>% 
  mutate(group = fct_relevel(group , "D", "C", "B", "A")) %>% 
  ggplot() +
  geom_boxplot(aes(x = cont_response, y = group)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
    panel.background = element_blank()
  ) +  
  geom_vline(xintercept = 2.0, color = "darkgreen") +
  geom_vline(xintercept = c(1.0,3.0), color = "magenta", linetype = "dotted", size = 1.0) +
  labs(x = "Median (Quartiles)", y = "Groups", 
       title = "Plot of synthetic data - to warm-up ggplot skills") +
  geom_jitter(aes(x = cont_response, y = group), width = 0.0, height=0.2) 
  
fig02a

# get rank-based descriptive
summary_tbl <- df01 %>%
  group_by(group) %>%
  summarise(
    median = median(cont_response),
    q25 = quantile(cont_response, 0.25),
    q75 = quantile(cont_response, 0.75)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Table as ggplot object
fig03a <- ggtexttable(summary_tbl, rows = NULL)
fig04a <- ggtexttable(df_pvals, rows = NULL)

# Combine
# fig02 <- ggarrange(fig02a, fig03a, fig04a, ncol = 2, nrow=2, widths=c(2, 1), heights=c(2,1))

library(gridExtra)
fig02 <-  grid.arrange(fig02a, fig03a, fig04a,
  layout_matrix = rbind(c(1,1,1), c(2,3,4)),
  heights = c(3,1.5))

ggsave(filename = file.path(path_resu_root, "box_plot_kw.png"), fig02,
       width = 30, height = 20, units = 'cm')


