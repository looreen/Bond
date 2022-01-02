#https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/ 
#http://www.colorhunter.com/tag/jamesbond/9

library('ggthemes')
library(transformr)
library(gganimate)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

world

world_map <- map_data("world")

bond %>% select(country_to_join) %>% head()

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



italy_importance <- bond %>% 
  left_join(by = 'Bond', 
            y = (bond %>% 
              group_by(Bond) %>% 
              select(Bond, Movie) %>%
              unique() %>%
              summarise(no_movies = n(), .groups = 'keep'))) %>% 
  filter(Depicted_Film_Loc == 'Italy') %>% 
  group_by(Bond) %>% 
  mutate(italy_index = n()/no_movies) %>% 
  select(Bond, italy_index)

italy_importance %>% 
  unique() %>% 
  ggplot(aes(Bond, italy_index))+
  geom_col()

map_bond <- bond %>% 
  filter(Bond == 'Daniel Craig') %>% 
  group_by(Year, Movie, Depicted_Film_Loc, rstats_countries, Bond) %>% 
  summarise(count_n = n(), .groups = 'keep') %>% 
  group_by(Depicted_Film_Loc) %>% 
  mutate(cumulated_appearance = sum(count_n))

world_joined <- world_map %>% 
  left_join(map_bond, by = c('region' = 'rstats_countries'))

names(world_joined)

world_joined %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = cumulated_appearance)) +
  theme(
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    text = element_text(family = 'sans'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom'
  ) +
  ggtitle('How often does Bond visit a country?') +
  scale_fill_gradient(NULL, 
                      low = '#A17671', high = '#EB0000', na.value = '#C8C2CE')

# Animation 

m <- world_joined %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = count_n)) +#, color = 'white') +
  theme(
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    text = element_text(family = 'sans'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_fill_viridis_c(option = "C")

m
  
anim <- m +
  transition_states(Year, transition_length = 2, state_length = 2)+
  shadow_mark(alpha = 0.5)+
  ggtitle('{closest_state}')

animate(anim, end_pause = 10)

