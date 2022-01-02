#https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/ 

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
  )) #%>% 
  # left_join(world_map, by = c('rstats_countries' = 'region'), keep = T)

map_bond <- bond %>% 
  group_by(Year, Movie, Depicted_Film_Loc, rstats_countries) %>% 
  summarise(count_n = n(), .groups = 'keep') %>% 
  group_by(Depicted_Film_Loc) %>% 
  mutate(cumulated_appearance = sum(count_n))

world_joined <- world_map %>% 
  left_join(map_bond, by = c('region' = 'rstats_countries'))

names(world_joined)

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
