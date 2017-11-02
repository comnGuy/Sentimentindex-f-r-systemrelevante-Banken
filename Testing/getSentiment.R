
require(ggplot2)
library(tidyr)

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
  inner_join(get_sentiments("bing")) %>%
  count(X, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)




sum(sentiments$negative)
sum(sentiments$positive)

head(sentiments,20)
