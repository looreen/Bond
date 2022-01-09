#https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
#https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/ 
#http://www.colorhunter.com/tag/jamesbond/9

source('data load.R')

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

world

world_map <- map_data("world")

bond %>% select(country_to_join) %>% head()

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
  ggtitle('How often does Craig-Bond visit a country?') +
  scale_fill_gradient(NULL, 
                      low = '#A17671', high = '#EB0000', na.value = '#C8C2CE')


# Cumulated

unique(bond$rstats_countries)

bond_cumsum <- bond %>% 
  select(Year, Movie, Bond, rstats_countries) %>% 
  arrange(Year) %>% 
  group_by(Year, rstats_countries) %>% 
  summarise(count_n = n(), .groups = 'keep') %>% 
  ungroup() %>% 
  group_by(rstats_countries) %>% 
  mutate(cumulated_n = cumsum(count_n)) %>% 
  ungroup()

world_cum_joined <- world_map %>% 
  left_join(bond_cumsum, by = c('region' = 'rstats_countries'))

names(world_cum_joined)

world_cum_joined %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = cumulated_n)) +
  theme(
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    text = element_text(family = 'sans'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_fill_gradient(NULL, 
                      low = '#A17671', high = '#EB0000', na.value = '#C8C2CE')

# Animation 

m <- world_cum_joined %>% 
  mutate(cumulated_n = ifelse(is.na(cumulated_n), 0, cumulated_n),
         count_n = ifelse(is.na(count_n), 0, count_n)) %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = cumulated_n)) +#, color = 'white') +
  theme_void() +
  theme(
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    text = element_text(family = 'sans'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_fill_gradient('Number of visits', low = '#C8C2CE', high = '#EB0000')

  # scale_fill_gradient('Number of visits', low = '#A17671', high = '#EB0000', na.value = '#C8C2CE')

m

anim <- m +
  ggtitle('Countries visited by a James Bond',
          subtitle = 'Year: {closest_state}') +
  transition_states(Year, transition_length = 2, state_length = 5)+
  shadow_mark()

to_save_map <- animate(anim, end_pause = 10)
anim_save("animated_map.gif", to_save_map)


