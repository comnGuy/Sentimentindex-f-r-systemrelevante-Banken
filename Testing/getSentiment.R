
require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
source("SentimentFunctions.R")

setwd('C:/Clouds/OwnCloud/Studium/Text Mining/Sentimentindex-f-r-systemrelevante-Banken/Testing')
#normalDataFrame <- read.csv("data/splittedDataframe1.csv")


normalDataFrame <- read_delim("data/bankEU_test.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)


someRes <- generateSentiment(normalDataFrame)
head(someRes)




columnText <- 'full_text'
ColumnWords <- 'Words'

normalDataFrame$full_text <- as.character(normalDataFrame[[columnText]])



splittedDataframe1_words <- normalDataFrame %>%
  unnest_tokens_(ColumnWords, columnText)



# Ausgabe der top Wörter
#splittedDataframe1_words %>%
#  count(Words, sort = TRUE) %>%
#  filter(n > 10) %>%
#  mutate(Words = reorder(Words, n)) %>%
#  ggplot(aes(Words, n)) +
#  geom_col() +
#  xlab(NULL) +
#  coord_flip()






# BING
#tidy_books <- normalDataFrame %>%
#  group_by(X) %>%
#  mutate(linenumber = row_number()) %>%
#  ungroup() %>%
#  unnest_tokens(word, Tweets)

#sentiments <- tidy_books %>%
#  inner_join(get_sentiments("bing")) %>%
#  count(X, index = linenumber %/% 80, sentiment) %>%
#  spread(sentiment, n, fill = 0) %>%
#  mutate(sentiment = positive - negative)


tidy_books <- normalDataFrame %>%
  #group_by(X) %>%
  mutate(X = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, full_text)

sentiments <- tidy_books %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(X) %>%
  summarise(sentiment = sum(score))



#head(afinn, 30)
#head(sentiments)


summary(normalDataFrame$Min)

sum(sentiments$negative)
sum(sentiments$positive)

head(sentiments,20)



total <- merge(normalDataFrame, sentiments,by="X")

summary(total$DayOfMonth)


ggplot(total, aes(X, sentiment, fill = Follower)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~DayOfMonth, ncol = 2, scales = "free_x")

