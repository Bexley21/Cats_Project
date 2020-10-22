library("tidyverse")
gapminder <- read_csv("./data/gapminder-FiveYearData.csv")
gapminderPopCharacter <- read_csv("./data/gapminder-FiveYearData.csv", col_types =cols(
  country = col_character(),
  year = col_double(),
  pop = col_character(),
  continent = col_character(),
  lifeExp = col_double(),
  gdpPercap = col_double()
))
year_country_gdp <- select(gapminder,year,country,gdpPercap)
gapminderEurope <- filter(gapminder, continent == "Europe")
gapminderOld <- filter(gapminder, lifeExp > 60)
Africa_selected_data <- gapminder %>% filter(continent == "Africa") %>% 
  select(lifeExp,country,year) %>% 
  print(Africa_selected_data)
gapminder_totalgdp <- gapminder %>% 
  mutate(gdp = gdpPercap * pop)
europe_ranked_life_expectancy <- gapminder %>%
  filter(continent == "Europe", year == 2007) %>%
  select(country, lifeExp) %>%
  mutate(Rank = min_rank(desc(lifeExp))) %>% 
  arrange(Rank)
mean_2007 <- gapminder %>%
  filter (year == 2007) %>%
  summarise(lifeExp_mean = mean(lifeExp))
continent_mean_lifeExp <- gapminder %>% 
  group_by(continent,year) %>% 
  summarise(lifeExp_mean = mean(lifeExp))
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
gapminder %>% group_by(year) %>% 
  summarise(meanlifeExp = mean(lifeExp)) %>% 
  ggplot(aes(x=year, y = meanlifeExp)) + geom_point()
gapminder %>% 
  ggplot(aes(x=year, y = lifeExp, group = country, color = continent)) + geom_line() + geom_point()
gapminder %>% 
  ggplot(aes(x=year, y = lifeExp,group = country)) + geom_line(aes(colour = continent)) + geom_point(aes(colour="yellow"))
gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, group = country)) + geom_line() + facet_wrap("continent")
#challenge 3 - present gdp per capita data over time
gapminder %>% 
  ggplot(aes(x = year, y = gdpPercap, group = country)) + geom_line(aes(colour = continent))
gapminder %>% 
  ggplot(aes(x=year, y = gdpPercap, group = country)) + geom_line() + facet_wrap("continent", scale = "free_y")
#
#challenge 4 - working with ggiraph
library(ggplot2)
library(ggiraph)
country <- gapminder$country
interactive_graph <- ggplot(data = gapminder, aes(x= year, y = gdpPercap, group = country)) + geom_line(aes(colour = continent)) + geom_point_interactive(aes(x=year, y = gdpPercap, tooltip = country)) 
girafe(ggobj = interactive_graph)
