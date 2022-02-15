load_and_clean <- function(input) { 
  require(tidyverse)

  d <- read_delim(file = input, delim = '\t')
  
  d <- d %>% 
    rename(
      average_rating   = rate, 
      count_of_ratings = ratings) %>% 
    mutate(
      log_of_average_rating = log(average_rating)
    )
}