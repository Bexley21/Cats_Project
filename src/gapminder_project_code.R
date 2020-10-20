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
