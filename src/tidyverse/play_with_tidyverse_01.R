

"
-------------------------
###### row ops
arrange(mtcars, desc(disp))   --- Order rows
filter                        --- Keep rows that match a condition
drop_na

slice
slice_max
slice_min
slice_sample(n=6)


-------------------------
###### col ops 
select
select(ends_with())
  starts_with
  ends_with
  contains
  matches
  num_range
relocate
rename(new_name = old_name)

-------------------------
###### create new cols 
mutate

-------------------------
###### aggregate 
summarise
group_by
tally

-------------------------
###### two-table commands 
anti_join
semi_join
inner_join( ) 
left_join()
right_join( )
full_join( )


-------------------------
###### reshape stuff 
pivot_longer
pivot_wider


pivot_longer(data, cols, names_to,   values_to    )

pivot_wide  (data,       names_from, values_from  )

"


library(dplyr) 
library(tidyr)

dim(starwars)
x0 <- starwars

names(x0)

# 
x1 <- x0 %>% 
  select(! (contains('world') | contains('color'))) 
  
dim(x0)
dim(x1)

# transform list to multiple rows
x2 <- x1 %>% 
  select(name, films) %>% 
  unnest_longer(films)

x2 <- x1 %>% 
  select(name, films) %>% 
  unnest_wider(films, names_sep = "_")

# x2 %>% print(n=200)

x3 <- x1 %>% 
  mutate(is_old = birth_year >= 100) %>%
  select(name, gender, species, is_old) %>% 
  mutate(is_old = factor(is_old, levels = c(TRUE, FALSE),labels = c("old", "Young")))

x3 %>% 
  left_join(x2, by = "name")


# ------------------------------
# behaviour of mutate() under group_by()
# prep
x2b <- x1 %>% 
  select(name, films, mass) %>% 
  unnest_longer(films) %>% 
  drop_na()
# without group_by() grand total is written to each row
x2b %>% 
  mutate(count = n())
# with group_by() per-group total is written to each row
x2b %>% 
  group_by(name) %>% 
  mutate(count = n())
# without group_by() grand mean is written to each row
x2b %>% 
  mutate(mean = mean(mass), .keep = "all")
# with group_by() per-group mean is written to each row
x2b %>% 
  group_by(name) %>% 
  mutate(mean = mean(mass), .keep = "all")
# ------------------------------















x4 <- x2 %>% 
  group_by(name) %>% 
  summarise(count = n())
 


# summarize accross multiple vars and functions 
x1 %>% 
  drop_na() %>% 
  group_by(sex) %>% 
  summarize(
    height_mean = mean(height, na.rm = TRUE), 
    mass_mean = mean(mass), 
    birth_y_mean = mean(birth_year),
    height_median = median(height),
    mass_median = median(mass),
    birth_y_median = median(birth_year)
    )

# same but simpler syntax
x1 %>%
  drop_na() %>% 
  group_by(sex) %>% 
  summarise(
    across(c(height, mass, birth_year),
           list(mean = ~mean(.x, na.rm = TRUE), median = ~median(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")  )














data.frame("count" = sort(summary(as.factor(x0$skin_color)), TRUE))
data.frame("count" = sort(summary(as.factor(x0$eye_color)), TRUE))

x1 <- x0 %>% filter(skin_color == "light", eye_color == "brown")

x0
x1

# row sort / select  
starwars %>% arrange(height, mass)
starwars %>% arrange(desc(height))

starwars %>% slice(5:10)
starwars %>% arrange(height, mass) %>% slice_head(n = 3)


set.seed(667788)
starwars %>% slice_sample(n = 5)
starwars %>% slice_sample(prop = 0.1)

starwars %>%
  filter(!is.na(height)) %>%
  slice_max(height, n = 3)

starwars %>%
  filter(!is.na(height)) %>%
  slice_min(height, n = 3)






# Select columns by name
starwars %>% select(hair_color, skin_color, eye_color)
# Select all columns between hair_color and eye_color (inclusive)
starwars %>% select(hair_color:eye_color)
# Select all columns except those from hair_color to eye_color (inclusive)
starwars %>% select(!(hair_color:eye_color))
# Select all columns ending with color
starwars %>% select(ends_with("color"))
# select and rename 
starwars %>% select(home_world = homeworld)


vars <- c("name", "height")
select(starwars, all_of(vars), "mass")

# mutate
starwars %>% mutate(height_m = height / 100)

starwars %>%
  mutate(height_m = height / 100) %>%
  select(height_m, height)

starwars %>%
  mutate(height_m = height / 100, BMI = mass / (height_m^2)) %>%
  select(BMI, height_m, everything())

starwars %>%
  mutate(height_m = height/100, BMI = mass/(height_m^2), .keep = "none")

# relocate
starwars %>% relocate(sex:homeworld, .before = height)




# summarize
starwars %>% summarise(
  height1 = mean(height, na.rm = TRUE),
  height2 = min(height, na.rm = TRUE),
  height3 = max(height, na.rm = TRUE),
  height4 = median(height, na.rm = TRUE)
  )




df <- starwars %>% select(name, height, mass)

mutate(df, "height", 2)

mutate(df, newvar = height + 10)


var <- seq(1, nrow(df))

mutate(df, new = var)


# group_by
group_by(starwars, sex)

group_by(starwars, sex = as.factor(sex))

table(cut(starwars$height, 3), exclude = NULL)

df4 <- group_by(starwars, height_binned = cut(height, 3))
df4 %>% tally()

by_species <- starwars %>% group_by(species)
by_species %>% tally()

by_sex_gender <- starwars %>% group_by(sex, gender)
by_sex_gender %>% tally(sort = TRUE)
by_sex_gender %>% group_keys()

by_species %>% group_keys() %>% print(n = 55)
by_species %>% group_indices()
by_species %>% group_vars()
by_sex_gender %>% group_vars()


by_species_2 <- by_species %>%
  group_by(homeworld, .add = TRUE) 

by_species_2 %>% tally()

by_species %>% ungroup() %>% tally()

by_sex_gender %>% 
  ungroup(sex) %>% 
  tally()


bmi_breaks <- c(0, 18.5, 25, 30, Inf)
star <- starwars %>% select(mass, height)

# mutate and then group_by
df01 <- star %>% 
  mutate(bmi_cat = cut(mass/(height/100)^2, breaks=bmi_breaks))  %>% 
  group_by(bmi_cat)

# same but all in one line 
df02 <- star %>%
  group_by(bmi_cat = cut(mass/(height/100)^2, breaks=bmi_breaks))
 
df01
df02

df01 %>% tally()
df01 %>% group_keys()
df02 %>% group_keys()





# operation after group_by 

by_species <- starwars %>% group_by(species)

by_species %>% group_keys() %>% print(n = 100)

by_species %>%
  summarise(
    n = n(),
    height_mean = mean(height, na.rm = TRUE),
    mass_mean   = mean(mass, na.rm = TRUE),
    mass_std    = sd(mass, na.rm = TRUE)
    
  )

by_sex <- starwars %>% group_by(sex)
by_sex %>% group_keys() %>% print(n = 100)


by_sex %>%
  summarise(
    n = n(),
    height_mean = mean(height, na.rm = TRUE),
    mass_mean   = mean(mass, na.rm = TRUE),
    mass_std    = sd(mass, na.rm = TRUE)
    
  )





# Subtract off global mean
starwars %>% 
  select(name, homeworld, mass) %>% 
  mutate(standard_mass = mass - mean(mass, na.rm = TRUE))




# Subtract off home-world mean
starwars %>% 
  select(name, homeworld, mass) %>% 
  group_by(homeworld) %>% 
  mutate(standard_mass = mass - mean(mass, na.rm = TRUE))


# Overall rank
starwars %>% 
  select(name, homeworld, height) %>% 
  mutate(rank = min_rank(height))

# Rank per homeworld
starwars %>% 
  select(name, homeworld, height) %>% 
  group_by(homeworld) %>% 
  mutate(rank = min_rank(height))



#------------------
# group_by and the select 

# global select 
starwars %>%
  select(name, species, height) %>% 
  drop_na() %>%
  filter(height == min(height))

# select by groups 
starwars %>%
  group_by(species) %>%
  select(name, species, height) %>% 
  drop_na() %>%
  filter(height == min(height))



by_species %>%
  filter(n() != 1) %>% 
  tally()





