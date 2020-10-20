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
