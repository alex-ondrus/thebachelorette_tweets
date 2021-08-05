##### Load libraries #####

library(rtweet)
library(tidyverse)
library(lubridate)

##### Obtain data #####

# This function will only pull tweets during the last 7 days, so if run within 7 days of
# the episode in interest ending, you should catch everything. DON'T INCLUDE THE '#'

get_bach_tweets <- function(hashtag){
  bach_tweets <- search_tweets(paste("#",hashtag, sep = ""),
                               n = 30000, 
                               include_rts = FALSE, 
                               retryonratelimit = TRUE)
  write_as_csv(bach_tweets, paste(hashtag, "_tweets_", Sys.Date(), ".csv", sep = ""))
  return(bach_tweets)
}

##### Filter data #####

# Filter for the tweets during the episode start and end in Eastern time
# e.g. episode_start = ymd_hms("2021-07-05 20:00:00", tz = "America/Toronto")

episode_tweet_text_time <- function(bach_tweets,
                                    episode_start,
                                    episode_end){
  episode_tweets <- bach_tweets %>% 
    filter(created_at <= episode_end,
           created_at >= episode_start) %>% 
    select(status_id, created_at, text) %>% 
    mutate(seconds_in = created_at - episode_start,
           minutes_in = round(as.numeric(seconds_in/60)))
  
  return(episode_tweets)
}

##### Egs for debugging #####

# bach_tweets <- get_bach_tweets("theBachelorette")
# episode_tweet_text <- episode_tweet_text_time(bach_tweets,
#                                               ymd_hms("2021-07-05 20:00:00", tz = "America/Toronto"),
#                                               ymd_hms("2021-07-05 22:00:00", tz = "America/Toronto"))