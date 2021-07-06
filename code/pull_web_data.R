##### Load libraries #####

library(rtweet)
library(tidyverse)

##### Obtain and filter data #####

get_bach_tweets <- function(hashtag){
  bach_tweets <- search_tweets(hashtag, n = 18000)
  write_as_csv(bach_tweets, paste("tweets_", Sys.Date(), ".csv", sep = ""))
  return(bach_tweets)
}