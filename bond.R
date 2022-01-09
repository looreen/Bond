source('data load.R')

bond %>% 
  filter(Bond == 'Daniel Craig',
         Depicted_Film_Loc == 'Italy') %>% 
  select(Movie) %>% unique()


bond_grouped <- bond %>% 
  group_by(Movie, Year, Bond) %>% 
  summarise(count_n = n(), .groups = 'keep') 

avg_locs <- mean(bond_grouped$count_n)

ranks <- bond_grouped %>% 
  filter(count_n > 5) %>% 
  mutate(nudge = 
    case_when(
      Year == 2008 ~ 0.1,
      Year == 2012 ~ 0.5,
      Year == 2015 ~ 0.8,
      TRUE ~ 0.2
    ))

ranks

p <- bond_grouped %>% 
  ggplot(aes(Year, count_n, fill = Bond))+
  geom_col()+
  theme(
    axis.title = element_blank(), 
    panel.background = element_blank(), 
    text = element_text(family = 'sans'),
    legend.position = 'bottom'
  )+
  geom_text(data = bond_grouped %>% filter(count_n>5), aes(label = Movie), nudge_y = (ranks$nudge),
            )+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  scale_fill_manual(NULL, values = c('#1C1414', '#5F4B40', '#EB0000', '#BC5244', '#C8C2CE', '#A17671'))+
  xlim(1955, 2025)
p

anim <- p + 
  transition_states(Year,
                    transition_length = 2,
                    state_length = 2)+
  shadow_mark(alpha = 0.5)+
  exit_fade()+
  ggtitle('{closest_state}')

anim

final <- animate(anim, end_pause = 10)
# anim_save("bond_no_location.gif", final)
