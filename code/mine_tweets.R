##### Load libraries and data #####

library(tidyverse)
library(tidytext)
library(magrittr)
library(ggrepel)
library(ggthemes)

afinn_sent <- get_sentiments("afinn")

##### Analyze sentiment #####

words_w_sentiment <- function(episode_tweets_w_mins){
  sents <- episode_tweets_w_mins %>% 
    select(text, minutes_in) %>% 
    unnest_tokens(word, text) %>% 
    inner_join(afinn_sent)
  return(sents)
}

sentiment_by_minute <- function(sents){
  sent_by_minute <- sents %>% 
    group_by(minutes_in) %>% 
    summarise(minute_sent = sum(value)) %>% 
    mutate(cumulative_sent = cumsum(minute_sent))
}

popular_sentimental_words <- function(sents){
  minute_words <- sents %>% 
    group_by(minutes_in, word) %>% 
    summarise(word_count = n())
  
  max_counts <- minute_words %>% 
    summarise(max_count = max(word_count))
  
  minute_words %<>% 
    left_join(max_counts) %>% 
    mutate(is_max = (max_count == word_count)) %>% 
    ungroup() %>% 
    filter(is_max) %>% 
    group_by(minutes_in) %>% 
    summarise(most_popular = paste0(word, collapse = ", "))
}

build_plot_df <- function(episode_tweets_w_mins){
  sentiments <- words_w_sentiment(episode_tweets_w_mins)
  plot_df <- inner_join(sentiment_by_minute(sentiments),
                    popular_sentimental_words(sentiments)) %>% 
    mutate(label_point = (abs(cume_dist(minute_sent) - 0.5) > 0.48),
           point_alpha = ifelse(label_point, 1, 0),
           point_label = ifelse(label_point, most_popular, NA_character_))
}

##### Generate visuals #####

generate_sent_plot <- function(plot_df, episode_number){
  sent_plot <- ggplot(plot_df,
                      aes(x = minutes_in,
                          y = cumulative_sent)) +
    geom_point(aes(alpha = point_alpha)) +
    geom_label_repel(aes(label = point_label),
                     alpha = 0.75) +
    geom_line() +
    scale_alpha(guide = "none") +
    labs(title = "Tell Us How You Really Feel",
         subtitle = "Cumulative sentiment of words tweeted with hashtag #theBachelorette.\nLabels show most common 'sentimental' words.",
         x = paste("Minutes Into Episode", episode_number),
         y = "AFINN Sentiment, Higher = More Positive",
         caption = "Data: Twitter API\nGraph: @DrAOndrus") +
    theme_hc()
  ggsave("images/sent_by_min.jpeg",
         width = 6,
         height = 4, 
         dpi = 600)
  return(sent_plot)
}

