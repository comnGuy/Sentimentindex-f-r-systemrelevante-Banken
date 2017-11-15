
require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)

setwd('C:/Clouds/OwnCloud/Studium/Text Mining/Sentimentindex-f-r-systemrelevante-Banken/Testing')
normalDataFrame <- read.csv("data/splittedDataframe1.csv")




normalDataFrame$Tweets <- as.character(normalDataFrame$Tweets)



splittedDataframe1_words <- normalDataFrame %>%
  unnest_tokens(Words, Tweets)



splittedDataframe1_words %>%
  count(Words, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(Words = reorder(Words, n)) %>%
  ggplot(aes(Words, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()




tidy_books <- normalDataFrame %>%
  group_by(X) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Tweets)



sentiments <- tidy_books %>%
  inner_join(get_sentiments("loughran")) %>%
  count(X, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


summary(normalDataFrame$Min)

sum(sentiments$negative)
sum(sentiments$positive)

head(sentiments,20)



total <- merge(normalDataFrame, sentiments,by="X")

summary(total$DayOfMonth)


ggplot(total, aes(X, sentiment, fill = Follower)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~DayOfMonth, ncol = 2, scales = "free_x")

