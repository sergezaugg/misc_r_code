# install.packages('nycflights13')
require(dplyr) 
require(tidyr)
library(nycflights13)

# anti_join and semi_join
flights %>% 
  anti_join(planes, by = "tailnum")

flights %>% 
  semi_join(planes, by = "tailnum")

# Drop unimportant variables so it's easier to understand the join results.
flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>% 
  left_join(airlines)

flights2 %>% left_join(planes, by = "tailnum")

inner_join( ) 
left_join()
right_join( )
full_join( )




