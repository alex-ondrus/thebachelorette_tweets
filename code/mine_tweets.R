##### Load libraries and data #####

library(tidyverse)
library(tidytext)
library(magrittr)
library(ggrepel)
library(ggthemes)
library(ggfittext)
library(widyr)

afinn_sent <- get_sentiments("afinn")

##### Cumulative sentiment by minute #####

# These functions prep the data for analyzing the cumulative sentiment
# of the words expressed at a minute-by-minute resolution

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

##### TF-IDF by 5 min Increments #####

# The goal of these functions is to create a histogram of the number of tweets,
# annotated by the word with the highest TF-IDF score

# This section takes the output of episode_tweet_text_time and creates a data frame of
# labels for the histogram bars

ten_min_tf_idf <- function(live_tweets){
  words_by_minute <- live_tweets %>% 
    filter(minutes_in < 60) %>% 
    mutate(increment_number = floor(minutes_in/10)+1) %>% 
    unnest_tokens(word, text) %>% 
    count(increment_number, word, sort = TRUE)
  
  total_words <- words_by_minute %>% 
    group_by(increment_number) %>% 
    summarise(total = sum(n))
  
  words_by_minute <- left_join(words_by_minute, total_words)
  
  words_tf_idf <- words_by_minute %>% 
    bind_tf_idf(word, increment_number, n) %>% 
    group_by(increment_number) %>%
    filter(tf_idf == max(tf_idf)) %>%
    summarise(word = paste0(word, collapse = ", "),
              total = total,
              tf_idf = tf_idf) %>%
    distinct() %>%
    arrange(increment_number) %>%
    select(increment_number, word, total, tf_idf)
}

tf_idf_clock_plot <- function(tf_idf_summary, episode_date = ''){
  plot_df <- tf_idf_summary %>% 
    add_column(x_start = (0:5)*10,
               x_end = (1:6)*10)
  
  clock_plot <- ggplot(plot_df,
                       aes(xmin = x_start,
                           xmax = x_end,
                           ymax = total,
                           label = word,
                           ymin = 0)) +
    geom_rect(fill = "#a1d5ee",
              colour = "grey") +
    geom_fit_text(place = "top",
                  contrast = TRUE) +
    coord_polar() +
    theme_minimal() +
    #scale_y_continuous(labels = NULL) +
    labs(title = paste("#BB23 Tweets", episode_date),
         subtitle = "Number of words/most 'summary' words for tweets\nsent in each 10 minute segment during episode airing",
         caption = "Radius = number of words tweeted\nLabel = Word with highest tf-idf score\nData = @twitter via rtweet\nGraph = @DrAOndrus")
  
  ggsave("images/ten_min_tf_idf_summary.jpeg",
         width = 5,
         height = 5,
         units = "in")
  return(clock_plot)
}

##### Word Association #####

# Given a list of contestants on a show, these functions combine to generate a 
# bar graph showing the top num_words associated words for each contestant

gen_contest_cors <- function(live_tweets,
                             contestant_list,
                             num_words = 3,
                             freq_filter = 20,
                             extra_stop_words = c()){
  tweet_cors <- live_tweets %>% 
    unnest_tokens(word, text) %>% 
    select(status_id, word) %>% 
    group_by(word) %>% 
    filter(n() >= freq_filter) %>% 
    pairwise_cor(word, status_id, sort = TRUE) %>% 
    filter(item1 %in% contestant_list) %>% 
    anti_join(stop_words, by = c("item2" = "word")) %>% 
    filter(!(item2 %in% str_to_lower(extra_stop_words))) %>% 
    group_by(item1) %>% 
    slice_max(correlation, n = num_words) %>% 
    mutate(item1 = str_to_title(item1),
           item2 = str_to_title(item2)) %>% 
    rename(contestant = item1,
           word = item2)
}

gen_cor_bars <- function(contest_cors, eps_date, num_cols = 3, im_width = 7, im_height = 8){
  cor_bars <- ggplot(contest_cors,
                      aes(x = correlation,
                          y = reorder(word, correlation),
                          label = word)) +
    geom_bar(stat = "identity") + 
    geom_bar_text(colour = "white") + 
    facet_wrap(~contestant, scales = "free", ncol = num_cols) + 
    scale_y_discrete(breaks = NULL) + 
    labs(y = NULL,
         title = "Word Association",
         subtitle = paste("3 words most strongly associated with each contestant during",
                          eps_date,
                          "episode"),
         x = "Phi coefficient",
         caption = "Data = @twitter via rtweet\nGraph = @DrAOndrus") + 
    theme_economist_white() +
    theme(panel.spacing.x = unit(2, "lines"))
  ggsave(filename = "images/cor_bars.jpg",
         plot = cor_bars,
         width = im_width,
         height = im_height,
         units = "in")
  return(cor_bars)
}

##### BB23 Specific Fixes #####

bb23_cast <- c("azah",
               "christian",
               "hannah",
               "britini",
               "derekf",
               "alyssa",
               "tiffany",
               "xavier",
               "claire",
               "sarah",
               "whitney",
               "derekx",
               "kyland")

bb23_name_fixes <- function(bb_live){
  aliases <- c("derek f", "derek x", "sarah beth", "sb","dx")
  matched_names <- c("derekf", "derekx", "sarah", "sarah","derekx")
  bb_live %<>% mutate(text = str_to_lower(text))
  for(i in 1:length(aliases)){
    bb_live %<>% 
      mutate(text = str_replace(text, coll(aliases[i]), matched_names[i]))
  }
  return(bb_live)
}

bb23_cor_fixes <- function(bb_cor){
  bb_cor %<>% mutate(contestant = recode(contestant,
                                         "Derekx" = "Derek X",
                                         "Derekf" = "Derek F"))
}