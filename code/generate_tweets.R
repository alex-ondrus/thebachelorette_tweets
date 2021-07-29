##### Load Libraries #####

library(rtweet)

##### Store tweet text #####

cumulative_sent_text <- "How do people feel about the Bachelorette? Techniques from https://www.tidytextmining.com/sentiment.html. #theBachelorette #data"

##### Generate tweet #####

# Assumes that the graph has been built already and that the image is saved in the 'images' folder
# as sent_by_min.jpeg

generate_sent_tweet <- function(){
  post_tweet(status = cumulative_sent_text,
             media = "images/sent_by_min.jpeg",
             token = token)
}
