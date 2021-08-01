##### Load Libraries #####

library(rtweet)

##### Bachelorette Sentiment Tweets #####

# Standard text to use for each post

cumulative_sent_text <- "How do people feel about the Bachelorette? Techniques from https://www.tidytextmining.com/sentiment.html. #theBachelorette #data"

# Assumes that the graph has been built already and that the image is saved in the 'images' folder
# as sent_by_min.jpeg

generate_sent_tweet <- function(){
  post_tweet(status = cumulative_sent_text,
             media = "images/sent_by_min.jpeg",
             token = token)
}

##### Big Brother TF-IDF Tweets #####

# Data processing pipeline. Takes in filtered tweets, calculates the tf-idf, and generates the visual

prep_bb_tf_idf_visual <- function(filtered_tweets, episode_date){
  tf_idf_df <- ten_min_tf_idf(filtered_tweets)
  tf_idf_clock_plot(tf_idf_df, episode_date)
}

# Standard text

bb_tf_idf_text <- "How a computer would summarize each 10 minutes of #bb23 in one word. Code: https://github.com/alex-ondrus/tv_tweets #bb23 #datavis #textmining"

# Summary tweet in status 1421671653101096963, assumes that the graph has been built already and that the image
# is saved in the 'images' folder as ten_min_tf_idf_summary.jpeg

generate_bb_tf_idf_tweet <- function(){
  post_tweet(status = bb_tf_idf_text,
             in_reply_to_status_id = "1421671653101096963",
             media = "images/ten_min_tf_idf_summary.jpeg",
             token = token)
}