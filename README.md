
# Reddit Sentiment Analysis

This script is a demonstration of how to use the RedditExtractoR, SentimentAnalysis, SnowballC, dplyr, ggplot2, and ggpmisc packages in R to perform sentiment analysis on Reddit comments from the wallstreetbets subreddit.

## Data Extraction

The script uses the find_thread_urls and get_thread_content functions from the RedditExtractoR package to extract the URLs and the content of the top threads from the wallstreetbets subreddit in the past year. The script selects the first 15 threads from the URLs obtained.

## Sentiment Analysis

The script uses the analyzeSentiment function from the SentimentAnalysis package to perform sentiment analysis on the comments, using the General Inquirer dictionary. This function returns a score for each comment, ranging from -5 to 5, where negative values indicate negative sentiment and positive values indicate positive sentiment. The script also uses the SnowballC package to perform word stemming on the comments, which reduces the words to their root form.

## Data Manipulation

The script uses the bind_cols and filter functions from the dplyr package to combine the data from the comments and the sentiment analysis into a single data frame, and to remove any comments that are empty or have NA values.

## Data Visualization

The script uses the ggplot2 and ggpmisc packages to create various plots to visualize the sentiment scores and their relationship with other variables, such as the date of posting, the comment score, and the word count. The script uses different types of plots, such as histograms, scatterplots, hexplots, and regression lines, to show the distribution, the trend, and the density of the sentiment scores. The script also uses the facet_wrap function to create multiple plots for each thread, which allows for comparison across different threads.# R-Semester-Project
