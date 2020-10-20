library("tidyverse")
cats_data <- read_csv(file = "data/feline-data.csv", col_types = cols(
  coat = col_character(),
  weight = col_double(),
  likes_string = col_logical()))
cats_data2 <- read_csv("data/feline-data_v2.csv", col_types = cols(
  coat = col_character(),
  weight = col_double(),
  likes_string = col_logical()
) )

catcoats <- gl(3,4,21)
levels(catcoats) <- c("tabby", "black", "white")
catcoats
