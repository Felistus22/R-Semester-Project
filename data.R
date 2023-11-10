# Load the RedditExtractor package
library(RedditExtractoR)
library(SentimentAnalysis)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(ggpmisc)

wsb_threads <- find_thread_urls(subreddit = "wallstreetbets", sort_by = "top", period = "year")
# view the results
head(wsb_threads)

wsb_comments <- get_thread_content(wsb_threads$url[1:15])

# changing text format
wsb_comments$comments$comment <- enc2utf8(wsb_comments$comments$comment)

# run sentiment analysis
wsb_sentiment <- analyzeSentiment(wsb_comments$comments$comment)

# combine data
wsb_dataset <- bind_cols(wsb_comments$comments, wsb_sentiment)

# plot histogram of sentiment
ggplot(wsb_dataset, aes(x = SentimentGI)) + geom_histogram() + labs(title = "sentiment for wallstreetbets")

# plot sentiment vs day of posting
# scatterplot
ggplot(wsb_dataset, aes(x = date, y = SentimentGI)) + geom_point() + labs(title = "Observation over time")

# plot sentiment by the comment score
ggplot(wsb_dataset, aes(x = score, y = SentimentGI)) + geom_point() + geom_smooth(method = lm )

# hexplot of density with linear model
ggplot(wsb_dataset, aes(x = score, y = SentimentGI)) + geom_hex(bins = 20, color = "white") +
  scale_fill_gradient(low = 'blue', high = 'orange') +
  labs(title = "sentiment by comment score") +
  geom_smooth(method = lm, formula = y ~ poly(x,2)) +
  theme_minimal() +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~" )),
    formula = y ~ poly(x,2), parse = TRUE
  )

# plot sentiment by word count
# scatterplot
ggplot(wsb_dataset, aes(x = WordCount, y = SentimentGI)) + geom_point() + geom_smooth(method = lm )

# hexplot of density with linear model
ggplot(wsb_dataset, aes(x = WordCount, y = SentimentGI)) + geom_hex(bins = 20, color = "white") +
  scale_fill_gradient(low = 'blue', high = 'orange') +
  labs(title = "sentiment by comment score") +
  geom_smooth(method = lm, formula = y ~ poly(x,2)) +
  theme_minimal() +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~" )),
    formula = y ~ poly(x,2), parse = TRUE
  )
