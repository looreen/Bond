library('ggthemes')
library(transformr)
library(gganimate)
library(here)
library(tidyverse)
library('HoRM')  
library(grid)
library(rworldmap)
library('gganimate')
library('gifski')
library(ggrepel)

bond <- read_csv('jamesbond.csv')

data(JamesBond)

names(JamesBond)
names(bond)

bond_loc <- bond %>% 
  separate_rows(Depicted_Film_Loc, sep = ', ') %>% 
  mutate(Depicted_Film_Loc = ifelse(Depicted_Film_Loc == 'England' |
                                      Depicted_Film_Loc == 'Scotland', 'Great Britain', Depicted_Film_Loc)) %>% 
  group_by(Depicted_Film_Loc, Year) %>% 
  summarise(count_n = n(), .groups = 'keep') %>% 
  arrange(desc(count_n)) 

bond <- bond %>% 
  separate_rows(Depicted_Film_Loc, sep = ', ') %>% 
  mutate(Depicted_Film_Loc = ifelse(Depicted_Film_Loc == 'England' |
                                      Depicted_Film_Loc == 'Scotland', 
                                    'Great Britain', 
                                    Depicted_Film_Loc), 
         country_to_join = case_when(
           Depicted_Film_Loc == 'Great Britain' ~ 'United Kingdom', 
           Depicted_Film_Loc == 'South Korea' ~ 'S. Korea', 
           Depicted_Film_Loc == 'Cube' ~ 'Cuba',   
           Depicted_Film_Loc == 'Czech Republic' ~ 'Czech Rep.',  
           Depicted_Film_Loc == 'Gibraltar' ~ 'United Kingdom',
           Depicted_Film_Loc == 'Jamaic' ~ 'Jamaica', 
           Depicted_Film_Loc == 'Monenegro' ~ 'Montenegro', 
           Depicted_Film_Loc == 'North Korea' ~ 'N. Korea', 
           Depicted_Film_Loc == 'Vatican City' ~ 'Vatican', 
           TRUE ~ Depicted_Film_Loc
         ))


bond <- bond %>% 
  mutate(rstats_countries = case_when(
    country_to_join == 'United Kingdom' ~ 'UK',
    country_to_join == 'United States' ~ 'USA',
    country_to_join == 'S. Korea' ~ 'South Korea',
    country_to_join == 'N. Korea' ~ 'North Korea', 
    country_to_join == 'Czech Rep.' ~ 'Czech Republic',
    country_to_join == 'Hong Kong' ~ 'China',
    country_to_join == 'Macau' ~ 'China', 
    TRUE ~ country_to_join
  )) 


