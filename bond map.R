worldMap <- getMap()
plot(worldMap)

countries <- worldMap$NAME %>% as_tibble()
countries

bond %>% 
  left_join(countries, by = c('country_to_join' = 'value'), keep = T)

bond <- bond %>% 
  group_by(Movie, Year, Depicted_Film_Loc, country_to_join, Bond) %>% 
  summarise(count_n = n(), .groups = 'keep')

bond$count_n

sPDF <- joinCountryData2Map(bond, joinCode='NAME', nameJoinColumn='country_to_join')
#somehow a problem with matching

#default map (see rworldmap documentation for options e.g. catMethod, numCats, colourPalette, mapRegion)
#missingCountryCol used for NA and countries not in the join file
p <- mapCountryData(sPDF, nameColumnToPlot='count_n', missingCountryCol='dark grey', catMethod = 'pretty')

