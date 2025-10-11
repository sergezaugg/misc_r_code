###############################################################################
# Project   : [Project name or study title]
# Script    : [File name or script purpose]
# Author    : [Your name]
###############################################################################

library(dplyr) 
library(tidyr)

" 
pivot_longer(data, cols, names_to,   values_to    )
pivot_wider (data,       names_from, values_from  )
"

df00 <- starwars

dim(df00)

# select a few variables 
df01 <- df00 %>% 
  select(name, height, eye_color) %>% 
  mutate(bla = height *2, bli = bla-56.23, blu = 555) %>% 
  rename(blo = height) %>% 
  relocate(eye_color, .after = name)

# pivot_longer
df02 <- df01 %>%  
  pivot_longer(cols = blo:blu, names_to = "categ", values_to = "num_val")

# re-create orig df with pivot_wider
df03 <- df02 %>% 
  pivot_wider(names_from = categ, values_from = num_val)

# check
dim(df01)
dim(df02)
87*4
dim(df03)
df01 == df03

#-------------------------------
# expand and complete
mysel <- c("Luke Skywalker", "R2-D2", "Darth Vader", "Leia Organa", "Owen Lars", "R5-D4")   

df01 <- df00 %>% 
  select(name, eye_color, height, birth_year) %>% 
  filter(name %in% mysel)

df02 <- df01 %>% 
  expand(name, eye_color)

df03 <- df01 %>% 
  complete(name, eye_color) 

df04 <- df01 %>% 
  complete(name, eye_color, fill = list(height = 0, birth_year = -99)) 











