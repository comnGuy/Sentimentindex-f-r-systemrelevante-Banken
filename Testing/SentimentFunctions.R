
# sentimentTest
# You should give a data frame with a 
compareSentiments <- function(sentimentTest, sentimentOriginal, sentimentBook) {
  sentimentTest$sentiment[sentimentTest$sentiment < 0] <- -1
  sentimentTest$sentiment[sentimentTest$sentiment > 0] <- 1
  
  mergedSentiments <- merge(sentimentTest, sentimentOriginal, by = 'X')
  
  Hits <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal,])
  noHits <- count(sentimentOriginal) - Hits
  positive <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal & mergedSentiments$sentimentOriginal == 1,])
  negative <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal & mergedSentiments$sentimentOriginal == -1,])
  neutral <- count(mergedSentiments[mergedSentiments$sentiment == mergedSentiments$sentimentOriginal & mergedSentiments$sentimentOriginal == 0,])
  
  results <- c(Hits$n, noHits$n)
  names <- c("Hits", "noHits")
  sentimentBookC <- c(sentimentBook, sentimentBook)
  data.frame(results, names, sentimentBookC)
}



#
#
generateSentiment <- function(dataFrame, columnText = 'full_text', ColumnWords = 'word', sentimentBook = 'afinn') {
  # Set the column as char
  dataFrame[[columnText]] <- as.character(dataFrame[[columnText]])
  
  # Split up the Tweets
  tidy_books <- dataFrame %>%
    mutate(X = row_number()) %>%
    ungroup() %>%
    unnest_tokens_(ColumnWords, columnText)
  
  sentiments <- ''
  # Summarize the word by Tweet
  if(sentimentBook == 'afinn') {
    sentiments <- tidy_books %>%
      inner_join(get_sentiments(sentimentBook), by = ColumnWords) %>%
      group_by(X) %>%
      summarise(sentiment = sum(score))
  }
  if(sentimentBook == 'bing' || sentimentBook == 'loughran') {
    sentiments <- tidy_books %>%
      inner_join(get_sentiments(sentimentBook)) %>%
      count(X, index = X %/% 80, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
  }
  if(sentimentBook == 'nrc') {
    sentiments <- tidy_books %>%
      inner_join(get_sentiments(sentimentBook)) %>%
      filter(sentiment %in% c('positive', 'negative')) %>%
      count(X, index = X %/% 80, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
  }
  sentiments
}